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
initCmd : Ref Tag.Div -> Maybe MobilePhoneNumber -> Cmd Void
initCmd ref Nothing =
  child ref $ p [] ["You don't have any mobile phone number :("]
initCmd ref (Just mobilePhoneNumber) =
  child ref $ p [] [ fromString ("What a beautiful phone number: " ++ show mobilePhoneNumber) ]

finishedData : Maybe MobilePhoneNumber -> FinishedData
finishedData maybeNumber =
  MkFinishedData
    Void
    (\ref => initCmd ref maybeNumber)

export
question : Maybe MobilePhoneNumber -> Questionnaire (Maybe MobilePhoneNumber)
question maybeNumber = Finished (finishedData maybeNumber) maybeNumber
