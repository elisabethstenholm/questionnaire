module Question.Name

import Web.MVC

import Data.List

import ValidData
import Questionnaire
import Question.Phonenumber

%default total

State : Type
State = List Char

InitState : State
InitState = []

data LocalEvent : State -> Type where
  Input : String -> LocalEvent state
  SubmitEmptyString : LocalEvent []

nameInput : Ref Tag.Input
nameInput = Id "name_input"

validationText : Ref Tag.P
validationText = Id "validation_text"

submitButton : Ref Tag.Button
submitButton = Id "submit_button"

tryValidateName : (state : State) -> Either (LocalEvent state) Name
tryValidateName [] = Left SubmitEmptyString
tryValidateName name@(x::xs) = Right (name ** IsNonEmpty)

content : Node (Either (LocalEvent InitState) Name)
content =
  div []
      [ p [] [ "Name:" ]
      , input [ Id nameInput , onInput (Left . Input) ] []
      , p [ Id validationText, class "validation" ] [""]
      , div [ class "buttons" ] [ button [ Id submitButton , onClick (Left SubmitEmptyString) ] [Text "Submit"] ] ]

initialize : Ref Tag.Div -> Cmd (Either (LocalEvent InitState) Name)
initialize ref = child ref content

update : (state : State) -> (event : LocalEvent state) -> State
update state (Input name) = unpack name
update [] SubmitEmptyString = []

display : Ref Tag.Div
        -> (state : State) 
        -> (event : LocalEvent state)
        -> Cmd (Either (LocalEvent (update state event)) Name)
display _ state (Input name) =
  batch [ replace validationText (p [ Id validationText, class "validation" ] [])
        , value nameInput name
        , replace submitButton (button [ Id submitButton , onClick (tryValidateName (unpack name)) ] [Text "Submit"]) ]
display _ [] SubmitEmptyString =
  replace validationText (p [ Id validationText, class "validation" ] ["Name cannot be empty!"])

questionData : Question.Data
questionData =
  MkData {
    State = State,
    Event = LocalEvent,
    AnswerType = Name,
    initialState = InitState,
    initialize = initialize,
    update = update,
    display = display
  }

export
question : Questionnaire ValidData
question = Question questionData Question.Phonenumber.question
