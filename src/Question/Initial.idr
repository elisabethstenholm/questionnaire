module Question.Initial

import Web.MVC

import ValidData
import Questionnaire
import Question.Phonenumber
import Question.Finished

%default total

public export
State : Type
State = ()

public export
LocalEvent : Question.Initial.State -> Type
LocalEvent _ = ()

public export
Event : Question.Initial.State -> Type
Event state = GlobalEvent (Question.Initial.LocalEvent state) Bool

export
initEvent : Question.Initial.Event ()
initEvent = LocalEvent ()

export
update : (state : Question.Initial.State) -> Question.Initial.LocalEvent state -> Question.Initial.State
update state event = state

button : Ref Tag.Button -> Question.Initial.Event () -> String -> Node (Question.Initial.Event ())
button ref event label = button [Id ref, onClick event] [Text label]

yesButton : Ref Tag.Button
yesButton = Id "yes_button"

noButton : Ref Tag.Button
noButton = Id "no_button"

export
yesNoButtons : Node (Question.Initial.Event ())
yesNoButtons =
  div
    [ class "form" ]
    [ button yesButton (SubmitData True) "Yes"
    , button noButton (SubmitData False) "No" ]

export
initCmd : Cmd (GlobalEvent (Question.Initial.LocalEvent ()) Bool)
initCmd =
  batch [ child contentDiv content
        , children questionDiv
                  [ p [] ["Do you have a mobile phone number?"]
                  , yesNoButtons ] ]

export
display : (state : Question.Initial.State)
        -> (event : Question.Initial.LocalEvent state)
        -> Cmd (Question.Initial.Event (update state event))
display state event =
  batch [ child contentDiv content
        , children questionDiv
                  [ p [] ["Do you have a mobile phone number?"]
                  , yesNoButtons ] ]

nextQuestion : Bool -> Questionnaire (Maybe MobilePhoneNumber)
nextQuestion True = Question.Finished.question Nothing
nextQuestion False = Question.Phonenumber.question

questionData : QuestionData
questionData =
  MkQuestionData
    Question.Initial.State
    Question.Initial.LocalEvent
    Bool
    ()
    initCmd
    update
    display

export
question : Questionnaire (Maybe MobilePhoneNumber)
question = Question questionData nextQuestion
