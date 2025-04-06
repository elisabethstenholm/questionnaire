module Question.Phonenumber

import Data.Fin
import Data.Vect
import Data.String

import Web.MVC

import Questionnaire

%default total

Digit : Type
Digit = Fin 10

IsValidNumber : Vect 8 Digit -> Type
IsValidNumber number = Either (head number = 4) (head number = 9)

export
MobilePhoneNumber : Type
MobilePhoneNumber = (number : Vect 8 Digit ** IsValidNumber number)

export
Show MobilePhoneNumber where
  show ([n1, n2, n3, n4, n5, n6, n7, n8] ** _) =
    show n1 ++ show n2 ++ show n3 ++ " " ++ show n4 ++ show n5 ++ " " ++ show n6 ++ show n7 ++ show n8

fromStringToMaybeMobilePhoneNumber : String -> Maybe MobilePhoneNumber
fromStringToMaybeMobilePhoneNumber string = do
  vect <- tryParseVect8Digit string
  valid <- tryValidateFirstDigit (head vect)
  pure $ (vect ** valid)
  where
    tryParseVect8Digit : String -> Maybe (Vect 8 Digit)
    tryParseVect8Digit string =
      toVect 8 =<< (sequence $ String.parsePositive . String.singleton <$> unpack string)

    tryValidateFirstDigit : (digit : Digit) -> Maybe (Either (digit = 4) (digit = 9))
    tryValidateFirstDigit 4 = Just $ Left Refl
    tryValidateFirstDigit 9 = Just $ Right Refl
    tryValidateFirstDigit _ = Nothing

public export
data LocalEvent : Type where
  InvalidPhoneNumberGiven : String -> LocalEvent

public export
Event : Type
Event = GlobalEvent LocalEvent MobilePhoneNumber

export
phoneNumberInput : Ref Tag.Input
phoneNumberInput = Id "phonenumber_input"

export
validationText : Ref Tag.P
validationText = Id "validation_text"

tryValidatePhoneNumber : String -> Event
tryValidatePhoneNumber string =
  case fromStringToMaybeMobilePhoneNumber string of
    Nothing => LocalEvent $ InvalidPhoneNumberGiven string
    Just mobilePhoneNumber => SubmitData mobilePhoneNumber

export
phoneNumberQuestionContent : Node Event
phoneNumberQuestionContent =
  div []
      [ p [] ["Telefonnummer:"]
      , input [ Id phoneNumberInput
              , onInput tryValidatePhoneNumber ]
              []
      , p [ Id validationText ] [""] ]
