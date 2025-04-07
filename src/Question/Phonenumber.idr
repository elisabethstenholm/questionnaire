module Question.Phonenumber

import Data.Vect

import Web.MVC

import ValidData
import Questionnaire
import Question.Finished

%default total

public export
State : Type
State = ()

public export
data LocalEvent : Question.Phonenumber.State -> Type where
  InvalidPhoneNumberGiven : String -> LocalEvent ()

public export
Event : Question.Phonenumber.State -> Type
Event state = GlobalEvent (LocalEvent state) MobilePhoneNumber

export
phoneNumberInput : Ref Tag.Input
phoneNumberInput = Id "phonenumber_input"

export
validationText : Ref Tag.P
validationText = Id "validation_text"

tryValidatePhoneNumber : String -> GlobalEvent (LocalEvent ()) MobilePhoneNumber
tryValidatePhoneNumber string =
  case tryParseMobilePhoneNumber string of
    Nothing => LocalEvent $ InvalidPhoneNumberGiven string
    Just mobilePhoneNumber => SubmitData mobilePhoneNumber

export
phoneNumberQuestionContent : Node (GlobalEvent (LocalEvent ()) MobilePhoneNumber)
phoneNumberQuestionContent =
  div []
      [ p [] ["Phone number:"]
      , input [ Id phoneNumberInput
              , onInput tryValidatePhoneNumber ]
              []
      , p [ Id validationText ] [""] ]

export
initCmd : Cmd (Event ())
initCmd = child questionDiv phoneNumberQuestionContent

export
update : (state : Question.Phonenumber.State) -> (event : LocalEvent state) -> Question.Phonenumber.State
update () (InvalidPhoneNumberGiven string) = ()

export
display : (state : Question.Phonenumber.State)
        -> (event : LocalEvent state)
        -> Cmd (Event (update state event))
display () (InvalidPhoneNumberGiven string) =
  batch [ value phoneNumberInput string
        , replace validationText (p [] ["Invalid phone number!"]) ]

questionData : QuestionData
questionData =
  MkQuestionData
    Question.Phonenumber.State
    Question.Phonenumber.LocalEvent
    MobilePhoneNumber
    ()
    initCmd
    update
    display

export
question : Questionnaire (Maybe MobilePhoneNumber)
question = Question questionData (Question.Finished.question . Just)
