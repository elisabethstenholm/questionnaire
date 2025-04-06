
module UI

import Web.MVC

import Questionnaire
import Question.Initial
import Question.Phonenumber

%default total

data State : Type where
  InitialQuestion : Question.Initial.State -> State
  PhoneNumberQuestion : Question.Phonenumber.State -> State
  Finished : Maybe MobilePhoneNumber -> State

Event : UI.State -> Type
Event (InitialQuestion state) = Question.Initial.Event state
Event (PhoneNumberQuestion state) = Question.Phonenumber.Event state
Event (Finished _) = Void

contentDiv : Ref Tag.Body
contentDiv = Id "content"

questionDiv : Ref Div
questionDiv = Id "question_div"

content : Node (UI.Event state)
content =
  div []
      [ h1 [] ["Phone number questionnaire"]
      , div [ Id questionDiv ] [] ]

update : (state : UI.State) -> UI.Event state -> UI.State
update (InitialQuestion state) (LocalEvent ()) = (InitialQuestion state)
update (InitialQuestion state) (SubmitData True) = (PhoneNumberQuestion ())
update (InitialQuestion state) (SubmitData False) = Finished Nothing
update (PhoneNumberQuestion state) (LocalEvent (InvalidPhoneNumberGiven _)) = (PhoneNumberQuestion state)
update (PhoneNumberQuestion state) (SubmitData number) = Finished $ Just number
update (Finished val) _ = Finished val

displayPhoneNumberQuestion : (event : Question.Phonenumber.Event ()) -> Cmd (UI.Event (update (PhoneNumberQuestion ()) event))
displayPhoneNumberQuestion (LocalEvent (InvalidPhoneNumberGiven string)) =
  batch [ value phoneNumberInput string
        , replace validationText (p [] ["Invalid phone number!"]) ]
displayPhoneNumberQuestion (SubmitData mobilePhoneNumber) =
  child questionDiv $ p [] [ fromString ("What a beautiful phone number: " ++ show mobilePhoneNumber) ]

displayFirstQuestion : (event : Question.Initial.Event ()) -> Cmd (UI.Event (update (InitialQuestion ()) event))
displayFirstQuestion (LocalEvent ()) =
  batch [ child contentDiv (content {state = InitialQuestion ()})
        , children questionDiv
                  [ p [] ["Do you have a mobile phone number?"]
                  , yesNoButtons ] ]
displayFirstQuestion (SubmitData False) =
  child questionDiv $ p [] ["You don't have any mobile phone number :("]
displayFirstQuestion (SubmitData True) =
  child questionDiv $ phoneNumberQuestionContent

display : (state : UI.State) -> (event : UI.Event state) -> Cmd (UI.Event (update state event))
display (InitialQuestion ()) = displayFirstQuestion
display (PhoneNumberQuestion ()) = displayPhoneNumberQuestion
display (Finished _) = const noAction

export covering
ui : IO ()
ui = runMVC UI.Event update display (putStrLn . dispErr) (InitialQuestion ()) Question.Initial.init
