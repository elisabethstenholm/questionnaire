module ValidData

import Data.Fin
import Data.Vect
import Data.String

%default total

public export
Digit : Type
Digit = Fin 10

public export
IsValidNumber : Vect 8 Digit -> Type
IsValidNumber number = Either (head number = 4) (head number = 9)

public export
MobilePhoneNumber : Type
MobilePhoneNumber = (number : Vect 8 Digit ** IsValidNumber number)

export
Show MobilePhoneNumber where
  show ([n1, n2, n3, n4, n5, n6, n7, n8] ** _) =
    show n1 ++ show n2 ++ show n3 ++ " " ++ show n4 ++ show n5 ++ " " ++ show n6 ++ show n7 ++ show n8

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
