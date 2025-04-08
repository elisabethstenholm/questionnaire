module Question.Finished

import Data.Vect

import Web.MVC

import ValidData
import Questionnaire

%default total

initialize : Maybe MobilePhoneNumber -> Ref Tag.Div -> Cmd Void
initialize Nothing ref =
  child ref $ p [] ["You don't have any mobile phone number :("]
initialize (Just mobilePhoneNumber) ref =
  child ref $ p [] [ fromString ("What a beautiful phone number: " ++ show mobilePhoneNumber) ]

finishedData : Maybe MobilePhoneNumber -> FinishedData
finishedData maybeNumber =
  MkFinishedData
    Void
    (initialize maybeNumber)

export
question : Maybe MobilePhoneNumber -> Questionnaire (Maybe MobilePhoneNumber)
question maybeNumber = Finished (finishedData maybeNumber) maybeNumber
