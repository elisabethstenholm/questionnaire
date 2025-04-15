module Question.Initial

import Web.MVC

import ValidData
import Common
import Questionnaire
import Question.Name
import Question.Location

%default total

State : Type
State = ()

InitState : State
InitState = ()

LocalEvent : State -> Type
LocalEvent _ = ()

yesButton : Ref Tag.Button
yesButton = Id "yes_button"

noButton : Ref Tag.Button
noButton = Id "no_button"

yesNoButtons : Node Bool
yesNoButtons =
  div
    [ class "buttons" ]
    [ button [Id yesButton, onClick True] [Text "Yes"]
    , button [Id noButton, onClick False] [Text "No"] ]

update : (state : State) -> LocalEvent state -> State
update state event = state

display : Ref Tag.Div
        -> (state : State)
        -> (event : LocalEvent state)
        -> Cmd (Either (LocalEvent (update state event)) Bool)
display ref state event =
  children ref
           [ p [] ["Did you drive the car?"]
           , Right <$> yesNoButtons ]

initialize : Ref Tag.Div -> Cmd (Either (LocalEvent InitState) Bool)
initialize ref = display ref () ()

nextQuestion : Bool -> Questionnaire ValidData
nextQuestion True = Question.Location.question InsuranceHolder
nextQuestion False = Question.Name.question

questionData : Question.Data
questionData =
  MkData {
    State = State,
    Event = LocalEvent,
    AnswerType = Bool,
    initialState = InitState,
    initialize = initialize,
    update = update,
    display = display
  }

export
question : Questionnaire ValidData
question = Question questionData nextQuestion
