
module Questionnaire

import Data.Fin
import Data.Vect

import Web.MVC

%default total

Digit : Type
Digit = Fin 10

IsValidNumber : Vect 8 Digit -> Type
IsValidNumber number = Either (head number = 4) (head number = 9)

MobilePhoneNumber : Type
MobilePhoneNumber = (number : Vect 8 Digit ** IsValidNumber number)

Show MobilePhoneNumber where
  show ([n1, n2, n3, n4, n5, n6, n7, n8] ** _) =
    show n1 ++ show n2 ++ show n3 ++ " " ++ show n4 ++ show n5 ++ " " ++ show n6 ++ show n7 ++ show n8

fromCharToMaybeDigit : Char -> Maybe Digit
fromCharToMaybeDigit '0' = Just 0
fromCharToMaybeDigit '1' = Just 1
fromCharToMaybeDigit '2' = Just 2
fromCharToMaybeDigit '3' = Just 3
fromCharToMaybeDigit '4' = Just 4
fromCharToMaybeDigit '5' = Just 5
fromCharToMaybeDigit '6' = Just 6
fromCharToMaybeDigit '7' = Just 7
fromCharToMaybeDigit '8' = Just 8
fromCharToMaybeDigit '9' = Just 9
fromCharToMaybeDigit _   = Nothing

fromDigitToMaybeIsValidNumber : (digit : Digit) -> Maybe (Either (digit = 4) (digit = 9))
fromDigitToMaybeIsValidNumber 4 = Just $ Left Refl
fromDigitToMaybeIsValidNumber 9 = Just $ Right Refl
fromDigitToMaybeIsValidNumber _ = Nothing

fromStringToMaybeVect8Digit : String -> Maybe (Vect 8 Digit)
fromStringToMaybeVect8Digit string =
  toVect 8 =<< (sequence $ fromCharToMaybeDigit <$> unpack string)

fromStringToMaybeMobilePhoneNumber : String -> Maybe MobilePhoneNumber
fromStringToMaybeMobilePhoneNumber string = do
  vect <- fromStringToMaybeVect8Digit string
  eq <- fromDigitToMaybeIsValidNumber (head vect)
  pure $ (vect ** eq)

data State : Type where
  FirstQuestion : State
  PhoneNumberQuestion : State
  Finished : Maybe MobilePhoneNumber -> State

data FirstQuestionEvent : Type where
  InitEvent : FirstQuestionEvent
  AnswerSubmittedBool : Bool -> FirstQuestionEvent

data PhoneNumberEvent : Type where
  InvalidPhoneNumberGiven : String -> PhoneNumberEvent
  AnswerSubmittedMobilePhoneNumber : MobilePhoneNumber -> PhoneNumberEvent

Event : State -> Type
Event FirstQuestion = FirstQuestionEvent
Event PhoneNumberQuestion = PhoneNumberEvent
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

button : Ref Tag.Button -> (state : State) -> Event state -> String -> Node (Event state)
button ref state event label = button [Id ref, onClick event] [Text label]

yesButton : Ref Tag.Button
yesButton = Id "yes_button"

noButton : Ref Tag.Button
noButton = Id "no_button"

yesNoButtons : Node FirstQuestionEvent
yesNoButtons =
  div
    [ class "form" ]
    [ button yesButton FirstQuestion (AnswerSubmittedBool True) "Yes"
    , button noButton FirstQuestion (AnswerSubmittedBool False) "No" ]

phoneNumberInput : Ref Tag.Input
phoneNumberInput = Id "phonenumber_input"

validationText : Ref Tag.P
validationText = Id "validation_text"

tryValidatePhoneNumber : String -> PhoneNumberEvent
tryValidatePhoneNumber string =
  case fromStringToMaybeMobilePhoneNumber string of
    Nothing => InvalidPhoneNumberGiven string
    Just mobilePhoneNumber => AnswerSubmittedMobilePhoneNumber mobilePhoneNumber

phoneNumberQuestionContent : Node PhoneNumberEvent
phoneNumberQuestionContent =
  div []
      [ p [] ["Telefonnummer:"]
      , input [ Id phoneNumberInput
              , onInput tryValidatePhoneNumber ]
              []
      , p [ Id validationText ] [""] ]

update : (state : State) -> Event state -> State
update FirstQuestion InitEvent = FirstQuestion
update FirstQuestion (AnswerSubmittedBool True) = PhoneNumberQuestion
update FirstQuestion (AnswerSubmittedBool False) = Finished Nothing
update PhoneNumberQuestion (InvalidPhoneNumberGiven _) = PhoneNumberQuestion
update PhoneNumberQuestion (AnswerSubmittedMobilePhoneNumber number) = Finished $ Just number
update (Finished val) _ = Finished val

displayFirstQuestion : (event : FirstQuestionEvent) -> Cmd (Event (update FirstQuestion event))
displayFirstQuestion InitEvent =
  batch [ child contentDiv (content {state = FirstQuestion})
        , children questionDiv
                  [ p [] ["Do you have a mobile phone number?"]
                  , yesNoButtons ] ]
displayFirstQuestion (AnswerSubmittedBool False) =
  child questionDiv $ p [] ["You don't have any mobile phone number :("]
displayFirstQuestion (AnswerSubmittedBool True) =
  child questionDiv $ phoneNumberQuestionContent

displayPhoneNumberQuestion : (event : PhoneNumberEvent) -> Cmd (Event (update PhoneNumberQuestion event))
displayPhoneNumberQuestion (InvalidPhoneNumberGiven string) =
  batch [ value phoneNumberInput string
        , replace validationText (p [] ["Invalid phone number!"]) ]
displayPhoneNumberQuestion (AnswerSubmittedMobilePhoneNumber mobilePhoneNumber) =
  child questionDiv $ p [] [ fromString ("What a beautiful phone number: " ++ show mobilePhoneNumber) ]

display : (state : State) -> (event : Event state) -> Cmd (Event (update state event))
display FirstQuestion = displayFirstQuestion
display PhoneNumberQuestion = displayPhoneNumberQuestion
display (Finished _) = const noAction

export covering
ui : IO ()
ui = runMVC Event update display (putStrLn . dispErr) FirstQuestion InitEvent
