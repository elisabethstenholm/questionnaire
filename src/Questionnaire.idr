
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

Show MobilePhoneNumber where
  show (MakeValidNumber [n1, n2, n3, n4, n5, n6, n7, n8] _) =
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
  pure $ MakeValidNumber vect eq

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

button : Ref Tag.Button -> (state : State) -> Event state -> String -> Node (Event state)
button ref state event label = button [Id ref, onClick event] [Text label]

yesNoButtons : Node FirstQuestionEvent
yesNoButtons =
  div
    [ class "form" ]
    [ button (Id "answer") FirstQuestion (AnswerSubmittedBool True) "Yes"
    , button (Id "answer") FirstQuestion (AnswerSubmittedBool False) "No" ]

update : (state : State) -> Event state -> State
update FirstQuestion InitEvent = FirstQuestion
update FirstQuestion (AnswerSubmittedBool hasPhoneNumber) = if hasPhoneNumber then PhoneNumberQuestion else Finished Nothing
update PhoneNumberQuestion (InvalidPhoneNumberGiven _) = PhoneNumberQuestion
update PhoneNumberQuestion (AnswerSubmittedMobilePhoneNumber number) = Finished $ Just number
update (Finished val) _ = Finished val

addTopTo : List (Node ev) -> Cmd ev
addTopTo list =
  child contentDiv $
    div [ class "content" ]
        (h1 [] ["Phone number questionnaire"] :: list)

display : (state : State) -> (event : Event state) -> Cmd (Event (update state event))
display FirstQuestion InitEvent =
  addTopTo [ p [ id "question" ] ["Do you have a mobile phone number?"]
           , yesNoButtons ]
display FirstQuestion (AnswerSubmittedBool True) =
  addTopTo [ p [ id "question" ] ["Telefonnummer:"]
           , form [ class "form" ] 
                  [ input [ id "phonenumberInput"
                          , onInput (\string => 
                              case fromStringToMaybeMobilePhoneNumber string of
                                Nothing => InvalidPhoneNumberGiven string
                                Just mobilePhoneNumber => AnswerSubmittedMobilePhoneNumber mobilePhoneNumber) ]
                          []
                  ]
           ]
display FirstQuestion (AnswerSubmittedBool False) =
  addTopTo [ p [] ["You don't have any mobile phone number :("] ]
display PhoneNumberQuestion (InvalidPhoneNumberGiven string) =
  addTopTo [ p [ id "question" ] ["Telefonnummer:"]
           , form [ class "form" ] 
                  [ input [ id "phonenumberInput"
                          , onInput (\string => 
                              case fromStringToMaybeMobilePhoneNumber string of
                                Nothing => InvalidPhoneNumberGiven string
                                Just mobilePhoneNumber => AnswerSubmittedMobilePhoneNumber mobilePhoneNumber)
                          , value string ]
                          []
                  ]
           , p [ class "error" ] ["Invalid phone number!"]
           ]
display PhoneNumberQuestion (AnswerSubmittedMobilePhoneNumber mobilePhoneNumber) =
  addTopTo [ p [] [ fromString ("What a beautiful phone number: " ++ show mobilePhoneNumber) ] ]
display (Finished _) _ = noAction

export covering
ui : IO ()
ui = runMVC Event update display (putStrLn . dispErr) FirstQuestion InitEvent

