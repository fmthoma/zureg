-- | Storing the registrants in a DynamoDB table.  Uses the `Eventful` library.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Zureg.Database.Sqlite
    ( Config (..)
    , withHandle
    ) where

import           Control.Exception       (throwIO)
import           Control.Lens            ((&), (.~), (^.))
import           Control.Monad           (forM, void, when)
import           Control.Monad.Logger    (runStderrLoggingT, NoLoggingT (..))
import           Control.Monad.Trans     (liftIO)
import qualified Data.Aeson              as A
import qualified Data.Aeson.TH.Extended  as A
import qualified Data.HashMap.Strict     as HMS
import           Data.Maybe              (listToMaybe)
import qualified Data.Text               as T
import qualified Database.Persist.Sql    as Sql
import qualified Database.Persist.Sqlite as Sqlite
import qualified Eventful                as E
import qualified Eventful.Store.Sqlite   as E
import qualified Network.AWS             as Aws
import qualified Network.AWS.DynamoDB    as DynamoDB
import qualified Zureg.Database          as Database
import           Zureg.Model

data Config = Config
    { cRegistrantTable :: !T.Text
    , cEmailTable      :: !T.Text
    }

data Handle = Handle
    { hConfig :: !Config
    , hWriter :: !(E.EventStoreWriter (Sql.SqlPersistT IO) E.JSONString)
    , hReader :: !(E.VersionedEventStoreReader (Sql.SqlPersistT IO) E.JSONString)
    }

withHandle :: Config -> (Database.Handle -> IO a) -> IO a
withHandle hConfig@Config {..} f = do
    let sqlConfig = E.defaultSqlEventStoreConfig

        hWriter = E.sqliteEventStoreWriter sqlConfig
        hReader = E.sqlEventStoreReader sqlConfig

    let handle = Handle {..}
    f Database.Handle
        { Database.writeEvents            = writeEvents        handle
        }

writeEvents :: Handle -> E.UUID -> [Event] -> IO ()
writeEvents Handle {..} uuid events = do
    mbError <- runNoLoggingT $
        Sqlite.withSqlitePool "database.sqlite" 1 $ \pool -> NoLoggingT $
        flip Sql.runSqlPool pool $ E.storeEvents hWriter E.AnyVersion uuid $
        map (E.serialize E.jsonStringSerializer) events
    maybe (return ()) (throwIO . Database.WriterException) mbError


{-
    hAwsEnv <- Aws.newEnv Aws.Discover

    let hWriter = E.dynamoDBEventStoreWriter dynamoConfig
        hReader = E.dynamoDBEventStoreReader dynamoConfig

        dynamoConfig = E.defaultDynamoDBEventStoreConfig
            { E.dynamoDBEventStoreConfigTableName            = cRegistrantTable
            , E.dynamoDBEventStoreConfigUUIDAttributeName    = "uuid"
            , E.dynamoDBEventStoreConfigVersionAttributeName = "version"
            , E.dynamoDBEventStoreConfigEventAttributeName   = "event"
            }


    let handle = Handle {..}
    f Database.Handle
        { Database.writeEvents            = writeEvents        handle
        , Database.getRegistrant          = getRegistrant      handle
        , Database.getRegistrantUuids     = getRegistrantUuids handle
        , Database.putEmail               = putEmail           handle
        , Database.lookupEmail            = lookupEmail        handle
        }

writeEvents :: Handle -> E.UUID -> [Event] -> IO ()
writeEvents Handle {..} uuid events = do
    mbError <- Aws.runResourceT $ Aws.runAWS hAwsEnv $
        E.storeEvents hWriter E.AnyVersion uuid $ map A.toJSON events
    maybe (return ()) (throwIO . Database.WriterException) mbError

getRegistrant :: Handle -> E.UUID -> IO Registrant
getRegistrant Handle {..} uuid = do
    values <- Aws.runResourceT $ Aws.runAWS hAwsEnv $
        E.getEvents hReader (E.allEvents uuid)
    events <- forM values $ \val ->
        case A.fromJSON (E.streamEventEvent val) of
            A.Error err -> throwIO $ Database.DecodeException (show err)
            A.Success e -> return e

    when (null events) $ throwIO $ Database.NotFoundException $
        "UUID " ++ show uuid ++ " not found"
    return $ E.latestProjection (registrantProjection uuid) events

-- | Perform a scan of the table to just return everything with version ID 0.
getRegistrantUuids :: Handle -> IO [E.UUID]
getRegistrantUuids Handle {..} = do
    values <- Aws.runResourceT $ Aws.runAWS hAwsEnv $ loop [] Nothing
    return values
  where
    loop acc mbLastKey | Just k <- mbLastKey, HMS.null k = return acc
    loop acc mbLastKey = do
        response <- Aws.send $ DynamoDB.scan (cRegistrantTable hConfig)
            & DynamoDB.sProjectionExpression .~ Just "#uuid"
            & DynamoDB.sFilterExpression .~ Just "version = :v"
            & DynamoDB.sExpressionAttributeValues .~ HMS.singleton ":v" zeroAv
            & DynamoDB.sExpressionAttributeNames .~ HMS.singleton "#uuid" "uuid"
            & (case mbLastKey of
                Nothing -> id
                Just k  -> DynamoDB.sExclusiveStartKey .~ k)

        uuids <- forM (response ^. DynamoDB.srsItems) $ \item -> maybe
            (liftIO $ throwIO $ Database.DecodeException "Could not read uuid")
            return (itemUuid item)

        loop (uuids ++ acc) (Just $ response ^. DynamoDB.srsLastEvaluatedKey)

    zeroAv = DynamoDB.attributeValue & DynamoDB.avN .~ Just "0"

putEmail :: Handle -> T.Text -> E.UUID -> IO ()
putEmail Handle {..} email uuid = Aws.runResourceT $ Aws.runAWS hAwsEnv $
    void $ Aws.send $ DynamoDB.putItem (cEmailTable hConfig)
        & DynamoDB.piItem .~ HMS.fromList
            [ ("email", DynamoDB.attributeValue & DynamoDB.avS .~ Just email)
            , ("uuid",  DynamoDB.attributeValue &
                DynamoDB.avS .~ Just (E.uuidToText uuid))
            ]

lookupEmail :: Handle -> T.Text -> IO (Maybe E.UUID)
lookupEmail Handle {..} email = Aws.runResourceT $ Aws.runAWS hAwsEnv $ do
    response <- Aws.send $ DynamoDB.query (cEmailTable hConfig)
        & DynamoDB.qKeyConditionExpression .~ Just "email = :e"
        & DynamoDB.qExpressionAttributeValues .~ HMS.singleton ":e" emailAv

    return $ itemUuid =<< listToMaybe (response ^. DynamoDB.qrsItems)
  where
    emailAv = DynamoDB.attributeValue & DynamoDB.avS .~ Just email

itemUuid :: HMS.HashMap T.Text DynamoDB.AttributeValue -> Maybe E.UUID
itemUuid item = do
    uuid <- HMS.lookup "uuid" item
    text <- uuid ^. DynamoDB.avS
    E.uuidFromText text

-}

$(A.deriveJSON A.options ''Config)
