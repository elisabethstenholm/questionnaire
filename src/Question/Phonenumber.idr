module Question.Phonenumber

import Data.Vect

import Web.MVC

import ValidData
import Questionnaire
import Question.Location

%default total

State : Type
State = String

InitState : State
InitState = ""

data LocalEvent : State -> Type where
  Input : String -> LocalEvent state
  SubmitInvalidNumber : LocalEvent state

phoneNumberInput : Ref Tag.Input
phoneNumberInput = Id "phonenumber_input"

validationText : Ref Tag.P
validationText = Id "validation_text"

submitButton : Ref Tag.Button
submitButton = Id "submit_button"

tryValidatePhoneNumber : String -> Either (LocalEvent state) MobilePhoneNumber
tryValidatePhoneNumber string =
  case tryParseMobilePhoneNumber string of
    Nothing => Left $ SubmitInvalidNumber
    Just mobilePhoneNumber => Right mobilePhoneNumber

content : Node (Either (LocalEvent state) MobilePhoneNumber)
content =
  div []
      [ p [] ["Phone number:"]
      , input [ Id phoneNumberInput , onInput (Left . Input) ] []
      , p [ Id validationText, class "validation" ] []
      , div [ class "buttons" ] [ button [ Id submitButton , onClick (Left SubmitInvalidNumber) ] [Text "Submit"] ] ]

initialize : Ref Tag.Div -> Cmd (Either (LocalEvent state) MobilePhoneNumber)
initialize ref = child ref content

update : (state : State) -> (event : LocalEvent state) -> State
update state (Input string) = string
update state SubmitInvalidNumber = state

display : Ref Tag.Div
        -> (state : State)
        -> (event : LocalEvent state)
        -> Cmd (Either (LocalEvent (update state event)) MobilePhoneNumber)
display _ state (Input string) =
  batch [ replace validationText (p [ Id validationText, class "validation" ] [])
        , value phoneNumberInput string
        , replace submitButton (button [ Id submitButton , onClick (tryValidatePhoneNumber string) ] [Text "Submit"]) ]
display _ state SubmitInvalidNumber =
  replace validationText (p [ Id validationText, class "validation" ] ["Invalid phone number!"])

questionData : Question.Data
questionData =
  MkData {
    State = State,
    Event = LocalEvent,
    SubmitDataType = MobilePhoneNumber,
    initialState = InitState,
    initialize = initialize,
    update = update,
    display = display
  }

export
question : Name -> Questionnaire ValidData
question name = Question questionData (Question.Location.question . Other name)
