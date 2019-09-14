{-# LANGUAGE
    TemplateHaskell,
    FlexibleContexts,
    FlexibleInstances,
    RecordWildCards,
    OverloadedStrings
    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Zureg.Model.Csv
    ( itemHeader
    ) where

import           Zureg.Model
import qualified Data.Time              as Time
import qualified Eventful               as E
import           Data.Csv               as CSV
import qualified Data.HashMap.Strict    as HM

instance ToNamedRecord (Registrant a) where
    toNamedRecord Registrant {..}
        =  HM.unions [ namedRecord [ "UUID" .= rUuid ]
                     , toNamedRecord rState
                     , toNamedRecord rInfo
                     , namedRecord [ "Scanned" .= rScanned ]
                     ]

instance ToNamedRecord RegisterState where
    toNamedRecord registerState = namedRecord [ "State" .= registerState ]

instance ToNamedRecord RegisterState => ToNamedRecord (Maybe RegisterState) where
    toNamedRecord registerState = namedRecord [ "State" .= registerState ]

instance ToNamedRecord RegisterInfo => ToNamedRecord (Maybe RegisterInfo)  where
    toNamedRecord registerInfo = case registerInfo of
        Just registerInfo' -> toNamedRecord registerInfo'
        Nothing            -> namedRecord [ "Name" .= ("No Registration info present" :: String)]

instance ToNamedRecord RegisterInfo where
    toNamedRecord RegisterInfo {..}  
        = namedRecord [ "Name"               .= riName 
                      , "Name on Badge"      .= riBadgeName 
                      , "Email"              .= riEmail 
                      , "Affiliation"        .= riAffiliation 
                      , "AskMeAbout"         .= riAskMeAbout 
                      , "Registered At"      .= riRegisteredAt
                      ]
         
           
instance ToField RegisterState where
    toField Registered = toField ("Registered" :: String)
    toField Confirmed  = toField ("Confirmed" :: String)
    toField Cancelled  = toField ("Cancelled" :: String)
    toField Waitlisted  = toField ("Waitlisted" :: String)

instance ToField Bool where
    toField True  = toField ("true" :: String)
    toField False = toField ("false" :: String)

instance ToField E.UUID where
    toField uuid' = toField (show uuid' :: String)

instance ToField Time.UTCTime where
    toField time' = toField (show time' :: String)

itemHeader :: Header
itemHeader = header
                [ "UUID"
                , "State"
                , "Scanned"
                , "Name"
                , "Name on Badge"
                , "Email"
                , "Affiliation"
                , "AskMeAbout"
                , "Beginner Track"
                , "Intermediate Track"
                , "Advanced Track"
                , "GhcDevOps Track"
                , "Mentor"
                , "T-Shirt Cut"
                , "T-Shirt Size"
                , "Mentor"
                , "Project Name"
                , "Project Website"
                , "Project Short Description"
                , "CL Beginner"
                , "CL Intermediate"
                , "CL Advanced"
                , "Registered At"
                ]
