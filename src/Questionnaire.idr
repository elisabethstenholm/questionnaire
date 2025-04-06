
module Questionnaire

import Web.MVC

import Question.Initial
import Question.Phonenumber

%default total

data State : Type where
  InitialQuestion : State
  PhoneNumberQuestion : State
  Finished : Maybe MobilePhoneNumber -> State

Event : State -> Type
Event InitialQuestion = Question.Initial.Event
Event PhoneNumberQuestion = Question.Phonenumber.Event
Event (Finished _) = Void

contentDiv : Ref Tag.Body
contentDiv = Id "content"

questionDiv : Ref Div
questionDiv = Id "question_div"

content : Node (Event state)
content =
  div []
      [ h1 [] ["Phone number questionnaire"]
      , div [ Id questionDiv ] [] ]

update : (state : State) -> Event state -> State
update InitialQuestion Question.Initial.Init = InitialQuestion
update InitialQuestion (Question.Initial.AnswerSubmitted True) = PhoneNumberQuestion
update InitialQuestion (Question.Initial.AnswerSubmitted False) = Finished Nothing
update PhoneNumberQuestion (InvalidPhoneNumberGiven _) = PhoneNumberQuestion
update PhoneNumberQuestion (Question.Phonenumber.AnswerSubmitted number) = Finished $ Just number
update (Finished val) _ = Finished val

displayPhoneNumberQuestion : (event : Question.Phonenumber.Event) -> Cmd (Event (update PhoneNumberQuestion event))
displayPhoneNumberQuestion (InvalidPhoneNumberGiven string) =
  batch [ value phoneNumberInput string
        , replace validationText (p [] ["Invalid phone number!"]) ]
displayPhoneNumberQuestion (Question.Phonenumber.AnswerSubmitted mobilePhoneNumber) =
  child questionDiv $ p [] [ fromString ("What a beautiful phone number: " ++ show mobilePhoneNumber) ]

displayFirstQuestion : (event : Question.Initial.Event) -> Cmd (Event (update InitialQuestion event))
displayFirstQuestion Question.Initial.Init =
  batch [ child contentDiv (content {state = InitialQuestion})
        , children questionDiv
                  [ p [] ["Do you have a mobile phone number?"]
                  , yesNoButtons ] ]
displayFirstQuestion (Question.Initial.AnswerSubmitted False) =
  child questionDiv $ p [] ["You don't have any mobile phone number :("]
displayFirstQuestion (Question.Initial.AnswerSubmitted True) =
  child questionDiv $ phoneNumberQuestionContent

display : (state : State) -> (event : Event state) -> Cmd (Event (update state event))
display InitialQuestion = displayFirstQuestion
display PhoneNumberQuestion = displayPhoneNumberQuestion
display (Finished _) = const noAction

export covering
ui : IO ()
ui = runMVC Event update display (putStrLn . dispErr) InitialQuestion Question.Initial.Init
