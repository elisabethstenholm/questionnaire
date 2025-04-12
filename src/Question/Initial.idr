module Question.Initial

import Web.MVC

import ValidData
import Common
import Questionnaire
import Question.Phonenumber
import Question.Finished

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
    []
    [ button yesButton True "Yes"
    , button noButton False "No" ]

update : (state : State) -> LocalEvent state -> State
update state event = state

display : Ref Tag.Div
        -> (state : State)
        -> (event : LocalEvent state)
        -> Cmd (Either (LocalEvent (update state event)) Bool)
display ref state event =
  children ref
           [ p [] ["Do you have a mobile phone number?"]
           , Right <$> yesNoButtons ]

initialize : Ref Tag.Div -> Cmd (Either (LocalEvent InitState) Bool)
initialize ref = display ref () ()

nextQuestion : Bool -> Questionnaire (Maybe MobilePhoneNumber)
nextQuestion False = Question.Finished.question Nothing
nextQuestion True = Question.Phonenumber.question

questionData : Question.Data
questionData =
  MkData
    State
    LocalEvent
    Bool
    InitState
    initialize
    update
    display

export
question : Questionnaire (Maybe MobilePhoneNumber)
question = Question questionData nextQuestion
