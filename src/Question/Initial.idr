module Question.Initial

import Web.MVC

import Questionnaire
import Question.Phonenumber

%default total

public export
Event : Type
Event = GlobalEvent () Bool

export
init : Question.Initial.Event
init = LocalEvent ()

button : Ref Tag.Button -> Question.Initial.Event -> String -> Node Question.Initial.Event
button ref event label = button [Id ref, onClick event] [Text label]

yesButton : Ref Tag.Button
yesButton = Id "yes_button"

noButton : Ref Tag.Button
noButton = Id "no_button"

export
yesNoButtons : Node Question.Initial.Event
yesNoButtons =
  div
    [ class "form" ]
    [ button yesButton (SubmitData True) "Yes"
    , button noButton (SubmitData False) "No" ]
