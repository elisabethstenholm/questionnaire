module Question.Location

import Web.MVC

import ValidData
import Questionnaire
import Question.Finished

%default total

State : Type
State = ()

InitState : State
InitState = ()

LocalEvent : State -> Type
LocalEvent _ = ()

withinNorwayButton : Ref Tag.Button
withinNorwayButton = Id "withinNorway_button"

outsideNorwayButton : Ref Tag.Button
outsideNorwayButton = Id "outsideNorway_button"

buttons : Node Location
buttons =
  div
    [ class "buttons" ]
    [ button [Id withinNorwayButton, onClick WithinNorway] [Text "Within Norway"]
    , button [Id outsideNorwayButton, onClick OutsideNorway] [Text "Outside of Norway"] ]

update : (state : State) -> LocalEvent state -> State
update state event = state

display : Ref Tag.Div
        -> (state : State)
        -> (event : LocalEvent state)
        -> Cmd (Either (LocalEvent (update state event)) Location)
display ref state event =
  children ref
           [ p [] ["Where did the accident happen?"]
           , Right <$> buttons ]

initialize : Ref Tag.Div -> Cmd (Either (LocalEvent InitState) Location)
initialize ref = display ref () ()

questionData : Question.Data
questionData =
  MkData {
    State = State,
    Event = LocalEvent,
    AnswerType = Location,
    initialState = InitState,
    initialize = initialize,
    update = update,
    display = display
  }

export
question : Driver -> Questionnaire ValidData
question driver = Question questionData (Question.Finished.question . MkValidData driver)
