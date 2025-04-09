module Question.Phonenumber

import Data.Vect

import Web.MVC

import ValidData
import Questionnaire
import Question.Finished

%default total

State : Type
State = ()

InitState : State
InitState = ()

data LocalEvent : State -> Type where
  InvalidPhoneNumberGiven : String -> LocalEvent state

phoneNumberInput : Ref Tag.Input
phoneNumberInput = Id "phonenumber_input"

validationText : Ref Tag.P
validationText = Id "validation_text"

tryValidatePhoneNumber : String -> Either (LocalEvent state) MobilePhoneNumber
tryValidatePhoneNumber string =
  case tryParseMobilePhoneNumber string of
    Nothing => Left $ InvalidPhoneNumberGiven string
    Just mobilePhoneNumber => Right mobilePhoneNumber

content : Node (Either (LocalEvent state) MobilePhoneNumber)
content =
  div []
      [ p [] ["Phone number:"]
      , input [ Id phoneNumberInput
              , onInput tryValidatePhoneNumber ]
              []
      , p [ Id validationText ] [""] ]

initialize : Ref Tag.Div -> Cmd (Either (LocalEvent state) MobilePhoneNumber)
initialize ref = child ref content

update : (state : State) -> (event : LocalEvent state) -> State
update state _ = state

display : Ref Tag.Div
        -> (state : State)
        -> (event : LocalEvent state)
        -> Cmd (Either (LocalEvent (update state event)) MobilePhoneNumber)
display _ _ (InvalidPhoneNumberGiven string) =
  batch [ value phoneNumberInput string
        , replace validationText (p [ Id validationText ] ["Invalid phone number!"]) ]

questionData : Question.Data
questionData =
  MkData
    State
    LocalEvent
    MobilePhoneNumber
    InitState
    initialize
    update
    display

export
question : Questionnaire (Maybe MobilePhoneNumber)
question = Question questionData (Question.Finished.question . Just)
