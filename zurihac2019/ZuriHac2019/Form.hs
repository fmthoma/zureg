{-# LANGUAGE OverloadedStrings #-}
module ZuriHac2019.Form (
    ( additionalInfoForm
    ) where

import qualified ZuriHac2019.Model as ZH19

import qualified Data.Text                   as T
-- import qualified Data.Time                   as Time
-- import qualified Eventful                    as E
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Digestive              as D
import qualified Text.Digestive.Blaze.Html5  as DH
-- import           Zureg.Model
-- import qualified Zureg.ReCaptcha             as ReCaptcha

additionalInfoForm :: Monad m => D.Form H.Html m ZH19.RegisterInfo
additionalInfoForm = RegisterInfo
    <$> "trackInterest" D..: (TrackInterest
            <$> "beginner" D..: D.bool Nothing
            <*> "intermediate" D..: D.bool Nothing
            <*> "advanced" D..: D.bool Nothing
            <*> "ghcDevOps" D..: D.bool Nothing)
    <*> ("tshirt" D..: (D.validate tshirtCheck $ (,)
            <$> "cut" D..: D.choice
                    [ (Nothing,     "I don't want a T-Shirt")
                    , (Just Female, "Female")
                    , (Just Male,   "Male")
                    ] (Just (Just Male))
            <*> "size" D..: D.choice (
                    [(Just s, H.toHtml $ show s) | s <- [minBound .. maxBound]] ++
                    [(Nothing, "I don't want a T-Shirt")])
                    (Just (Just M))))
    <*> ("mentor" D..: D.bool Nothing)
    <*> ("project" D..: (Project
            <$> "name" D..: optionalText
            <*> "website" D..: optionalText
            <*> "description" D..: optionalText
            <*> ("contributorLevel" D..: (ContributorLevel
                    <$> "beginner" D..: D.bool Nothing
                    <*> "intermediate" D..: D.bool Nothing
                    <*> "advanced" D..: D.bool Nothing))))
  where
    tshirtCheck (Just c,  Just s)  = D.Success (Just (c, s))
    tshirtCheck (Nothing, Nothing) = D.Success Nothing
    tshirtCheck (_,       _)       = D.Error
        "Fill in both T-Shirt cut and size or neither of the two"

    optionalText =
        (\t -> let t' = T.strip t in if T.null t' then Nothing else Just t') <$>
        (D.text Nothing)

registerView :: Hackathon -> ReCaptcha.ClientHtml -> D.View H.Html -> H.Html
registerView hackathon recaptcha view = DH.form view "?" $ do
    H.h1 $ H.toHtml (hName hackathon) <> " registration"
    H.div H.! A.class_ "errors" $ DH.childErrorList "" view

    DH.label "name" view $ H.strong "Full name"
    DH.inputText "name" view
    H.br

    DH.label "badgeName" view $ H.strong "Name on badge (optional)"
    H.p $ do
        "Fill in this field if you would rather use a nickname on your badge. "
        "By default we will use your full name."
    DH.inputText "badgeName" view
    H.br

    DH.label "email" view $ H.strong "Email"
    H.p $ do
        "We will only use your email address to send you your ticket, as well "
        "as information about the event.  It is not shared with any other "
        "parties."
    DH.inputText "email" view
    H.br

    DH.label "confirmEmail" view $ H.strong "Confirm your email"
    H.p "We want to be sure we that we can email you your ticket."
    DH.inputText "confirmEmail" view
    H.br

    DH.label "affiliation" view $ H.strong "Affiliation (optional)"
    H.p $ do
        "Affiliations that you want to display on your badge (e.g.: employer, "
        "university, open source project...)"
    DH.inputText "affiliation" view
    H.br

    DH.label "askMeAbout" view $ H.strong "Ask me about (optional)"
    H.p $ do
        "Topic(s) that you want to display on your badge.  It's a good ice "
        "breaker for people who want to chat with you."
    DH.inputText "askMeAbout" view
    H.br

    H.p $ H.strong "Track Interest (optional)"
    H.p $ do
        "Let us know which track(s) you would participate in.  Note that we "
        "are still in the process of organizing these, so this serves as an "
        "indication for us, not a commitment on your part.  We may not "
        "organize all of these tracks depending on availability and interest."
    DH.inputCheckbox "trackInterest.beginner" view H.! A.class_ "checkbox"
    DH.label "trackInterest.beginner" view $ "Beginner Track"
    H.br
    DH.inputCheckbox "trackInterest.intermediate" view H.! A.class_ "checkbox"
    DH.label "trackInterest.intermediate" view $ "Intermediate Track"
    H.br
    DH.inputCheckbox "trackInterest.advanced" view H.! A.class_ "checkbox"
    DH.label "trackInterest.advanced" view $ "Advanced Track"
    H.br
    DH.inputCheckbox "trackInterest.ghcDevOps" view H.! A.class_ "checkbox"
    DH.label "trackInterest.ghcDevOps" view $ "GHC DevOps Track"
    H.br

    H.p $ H.strong "T-Shirt"

    H.p $ H.strong $ do
        "Please note that we have ordered the T-Shirts and cannot guarantee "
        "that you will receive one if you register at this time."

    H.p $ "In what size would you like the free " <> H.toHtml (hName hackathon) <> " T-Shirt?"

    H.p $ do
        "The sizes should be fairly standard. "
        {- TODO only works with a more generic link.
        "You can see the "
        H.a H.! A.href "https://zfoh.ch/images/zurihac2019/tshirts-sizing.png"
            H.! A.target "_blank" $
            "specifications here"
        "."
        -}

    DH.label "tshirt.cut" view "Cut"
    DH.inputSelect "tshirt.cut" view
    H.br
    DH.label "tshirt.size" view "Size"
    DH.inputSelect "tshirt.size" view
    H.br

    H.p $ H.strong "Mentors (optional)"
    H.p $ do
        "Every year, we are looking for volunteers to help Haskell newcomers "
        "with questions. The idea is that people can call on your help if you "
        "happen to be around, however you won't be busy with this full-time. "
        "You will get a different t-shirt so that people can visibly identify "
        "you as a mentor."
    DH.inputCheckbox "mentor" view H.! A.class_ "checkbox"
    DH.label "mentor" view $ "I want to be a mentor"
    H.br

    H.p $ H.strong "Project (optional)"
    H.p $ do
        "Do you have a project or an idea to hack on with others? Do you have "
        "something you want to teach people?"
    H.p $ do
        "We greatly appreciate projects. We have had very good experience with "
        "announcing the project early on the homepage, so that potential "
        "participants can prepare before the Hackathon.  Of course, we're also "
        "happy to add projects during the Hackathon itself, so if you're not "
        "sure yet, don't worry about it."
    DH.label "project.name" view "Project name"
    DH.inputText "project.name" view
    DH.label "project.website" view "Project website"
    DH.inputText "project.website" view
    DH.label "project.description" view "Project description"
    DH.inputText "project.description" view
    H.p "Recommended contributor level(s)"
    DH.inputCheckbox "project.contributorLevel.beginner" view H.! A.class_ "checkbox"
    DH.label "project.contributorLevel.beginner" view $ "Beginner"
    H.br
    DH.inputCheckbox "project.contributorLevel.intermediate" view H.! A.class_ "checkbox"
    DH.label "project.contributorLevel.intermediate" view $ "Intermediate"
    H.br
    DH.inputCheckbox "project.contributorLevel.advanced" view H.! A.class_ "checkbox"
    DH.label "project.contributorLevel.advanced" view $ "Advanced"
    H.br
    H.br
    H.br

    ReCaptcha.chForm recaptcha

    DH.inputSubmit "Register"