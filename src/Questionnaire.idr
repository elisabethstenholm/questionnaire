
module Questionnaire

import Derive.Finite

import Data.Fin
import Data.Vect

import CSS.Core

import Text.HTML.Select
import Web.MVC

%default total
%language ElabReflection

Digit : Type
Digit = Fin 10

IsValidNumber : Vect 8 Digit -> Type
IsValidNumber number = Either (head number = 4) (head number = 9)

data MobilePhoneNumber : Type where
  MakeValidNumber : (number : Vect 8 Digit)
                  -> IsValidNumber number
                  -> MobilePhoneNumber

fromCharToMaybeDigit : Char -> Maybe Digit
fromCharToMaybeDigit '0' = Just FZ
fromCharToMaybeDigit '1' = Just $ FS FZ
fromCharToMaybeDigit '2' = Just $ FS $ FS FZ
fromCharToMaybeDigit '3' = Just $ FS $ FS $ FS FZ
fromCharToMaybeDigit '4' = Just $ FS $ FS $ FS $ FS FZ
fromCharToMaybeDigit '5' = Just $ FS $ FS $ FS $ FS $ FS FZ
fromCharToMaybeDigit '6' = Just $ FS $ FS $ FS $ FS $ FS $ FS FZ
fromCharToMaybeDigit '7' = Just $ FS $ FS $ FS $ FS $ FS $ FS $ FS FZ
fromCharToMaybeDigit '8' = Just $ FS $ FS $ FS $ FS $ FS $ FS $ FS $ FS FZ
fromCharToMaybeDigit '9' = Just $ FS $ FS $ FS $ FS $ FS $ FS $ FS $ FS $ FS FZ
fromCharToMaybeDigit _   = Nothing

fromDigitToMaybeIsValidNumber : (digit : Digit) -> Maybe (Either (digit = 4) (digit = 9))
fromDigitToMaybeIsValidNumber (FS $ FS $ FS $ FS FZ) = Just $ Left Refl
fromDigitToMaybeIsValidNumber (FS $ FS $ FS $ FS $ FS $ FS $ FS $ FS $ FS FZ) = Just $ Right Refl
fromDigitToMaybeIsValidNumber _ = Nothing

fromStringToMaybeVect8Digit : String -> Maybe (Vect 8 Digit)
fromStringToMaybeVect8Digit string =
  toVect 8 =<< (sequence $ fromCharToMaybeDigit <$> unpack string)

fromStringToMaybeMobilePhoneNumber : String -> Maybe MobilePhoneNumber
fromStringToMaybeMobilePhoneNumber string =
  case fromStringToMaybeVect8Digit string of
    Nothing => Nothing
    Just vect =>
      case fromDigitToMaybeIsValidNumber (head vect) of
        Nothing => Nothing
        Just eq => Just (MakeValidNumber vect eq)

public export
data Event : Type where
  Init : Event
  InvalidPhoneNumber : String -> Event
  AnswerSubmittedBool : Bool -> Event
  AnswerSubmittedMobilePhoneNumber : MobilePhoneNumber -> Event

public export
State : Type
State = ()

button : Ref Tag.Button -> Event -> String -> Node Event
button ref event label = button [Id ref, onClick event] [Text label]

buttons : Node Event
buttons =
  div
    [ class "form" ]
    [ button (Id "answer") (AnswerSubmittedBool True) "Yes"
    , button (Id "answer") (AnswerSubmittedBool False) "No" ]


export
init : State
init = ()

update : Event -> State -> State
update Init = id
update (AnswerSubmittedBool bool) = id
update (AnswerSubmittedMobilePhoneNumber _) = id
update (InvalidPhoneNumber _) = id

displayAnswer : Bool -> Node Event
displayAnswer True = "Oh, Yeah!"
displayAnswer False = "Nah"

display : Event -> State -> Cmd Event
display Init state =
  child contentDiv 
    (div
      [ class "content" ]
      [ h1 [] ["Den beste veiviseren!"]
      , p [ id "question"] ["Har du et telefonnummer?"]
      , buttons ])
display (AnswerSubmittedBool False) state =
  child contentDiv
    (div
    [ class "content" ]
    [ h1 [] ["Den beste veiviseren!"]
    , p [class "error"] ["Du har inget telefonnummer :("]])
display (AnswerSubmittedBool True) state =
  child contentDiv
    (div
    [ class "content" ]
    [ h1 [] ["Den beste veiviseren!"]
    , p [ id "question"] ["Telefonnummer"]
    , form [class "form"] [
        input 
          [ id "phonenumberInput"
          , onInput (\string => 
            case fromStringToMaybeMobilePhoneNumber string of
              Nothing => InvalidPhoneNumber string
              Just mobilePhoneNumber => AnswerSubmittedMobilePhoneNumber mobilePhoneNumber) ] [] ]
    ])
display (AnswerSubmittedMobilePhoneNumber mobilePhoneNumber) state = 
  child contentDiv
    (div
    [ class "content" ]
    [ h1 [] ["Den beste veiviseren!"]
    , p [id "answer"] ["For et pent telefonnummer!"]])
display (InvalidPhoneNumber oldString) state =
  child contentDiv
    (div
    [ class "content" ]
    [ h1 [] ["Den beste veiviseren!"]
    , p [ id "question"] ["Telefonnummer"]
    , form [class "form"] [
        input 
          [ id "phonenumberInput"
          , value oldString
          , onInput (\userString => 
            case fromStringToMaybeMobilePhoneNumber userString of
              Nothing => InvalidPhoneNumber userString
              Just mobilePhoneNumber => AnswerSubmittedMobilePhoneNumber mobilePhoneNumber) ] [] ]
    , p [ class "error" ] ["Invalid phone number! You suck!"]
    ])

export covering
ui : IO ()
ui = runMVC update display (putStrLn . dispErr) Init init

