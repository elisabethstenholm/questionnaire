module Question.Initial

import Web.MVC

import Question.Phonenumber

%default total

public export
data Event : Type where
  Init : Event
  AnswerSubmitted : Bool -> Event

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
    [ button yesButton (AnswerSubmitted True) "Yes"
    , button noButton (AnswerSubmitted False) "No" ]
