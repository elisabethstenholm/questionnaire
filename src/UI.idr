
module UI

import Data.Vect

import Web.MVC

import ValidData
import Questionnaire
import Question.Initial
import Question.Phonenumber
import Question.Finished

%default total

data State : Type where
  InitialQuestion : Question.Initial.State -> State
  PhoneNumberQuestion : Question.Phonenumber.State -> State
  Finished : Maybe MobilePhoneNumber -> State

Event : UI.State -> Type
Event (InitialQuestion state) = Question.Initial.Event state
Event (PhoneNumberQuestion state) = Question.Phonenumber.Event state
Event (Finished _) = Void

update : (state : UI.State) -> UI.Event state -> UI.State
update (InitialQuestion state) (LocalEvent event) = InitialQuestion $ Question.Initial.update state event
update (InitialQuestion state) (SubmitData True) = PhoneNumberQuestion ()
update (InitialQuestion state) (SubmitData False) = Finished Nothing
update (PhoneNumberQuestion state) (LocalEvent event) = PhoneNumberQuestion $ Question.Phonenumber.update state event
update (PhoneNumberQuestion state) (SubmitData number) = Finished $ Just number
update (Finished val) _ = Finished val

displayFirstQuestion : (state : Question.Initial.State)
                     -> (event : Question.Initial.Event state)
                     -> Cmd (UI.Event (update (InitialQuestion state) event))
displayFirstQuestion state (LocalEvent event) = Question.Initial.display state event
displayFirstQuestion state (SubmitData True) = Question.Phonenumber.initCmd
displayFirstQuestion state (SubmitData False) = Question.Finished.initCmd Nothing

displayPhoneNumberQuestion : (state : Question.Phonenumber.State)
                           -> (event : Question.Phonenumber.Event state)
                           -> Cmd (UI.Event (update (PhoneNumberQuestion state) event))
displayPhoneNumberQuestion state (LocalEvent event) = Question.Phonenumber.display state event
displayPhoneNumberQuestion state (SubmitData mobilePhoneNumber) = Question.Finished.initCmd $ Just mobilePhoneNumber

display : (state : UI.State) -> (event : UI.Event state) -> Cmd (UI.Event (update state event))
display (InitialQuestion state) = displayFirstQuestion state
display (PhoneNumberQuestion state) = displayPhoneNumberQuestion state
display (Finished _) = const noAction

export covering
ui : IO ()
ui = runMVC UI.Event update display (putStrLn . dispErr) (InitialQuestion ()) Question.Initial.initEvent
