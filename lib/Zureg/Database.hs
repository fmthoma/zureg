-- | Storing the registrants in a DynamoDB table.  Uses the `Eventful` library.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Zureg.Database
    ( DatabaseException (..)
    , Handle (..)
    ) where

import           Control.Exception (Exception)
import qualified Data.Text         as T
import qualified Eventful          as E
import           Zureg.Model

data DatabaseException
    = WriterException E.EventWriteError
    | DecodeException String
    | NotFoundException String
    deriving (Show)

instance Exception DatabaseException

data Handle = Handle
    { writeEvents        :: E.UUID -> [Event] -> IO ()
    , getRegistrant      :: E.UUID -> IO Registrant
    , getRegistrantUuids :: IO [E.UUID]
    , putEmail           :: T.Text -> E.UUID -> IO ()
    , lookupEmail        :: T.Text -> IO (Maybe E.UUID)
    }
