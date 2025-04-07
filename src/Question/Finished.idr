module Question.Finished

import Data.Fin
import Data.Vect
import Data.String

import Web.MVC

import ValidData
import Questionnaire

%default total

export
tryParseMobilePhoneNumber : String -> Maybe MobilePhoneNumber
tryParseMobilePhoneNumber string = do
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

export
initCmd : Maybe MobilePhoneNumber -> Cmd Void
initCmd Nothing =
  child questionDiv $ p [] ["You don't have any mobile phone number :("]
initCmd (Just mobilePhoneNumber) =
  child questionDiv $ p [] [ fromString ("What a beautiful phone number: " ++ show mobilePhoneNumber) ]

export
question : Maybe MobilePhoneNumber -> Questionnaire (Maybe MobilePhoneNumber)
question Nothing = Finished Nothing (initCmd Nothing)
question (Just mobilePhoneNumber) = Finished (Just mobilePhoneNumber) (initCmd (Just mobilePhoneNumber))
