
module UI

import Data.Vect

import Web.MVC

import Questionnaire

import ValidData
import Static
import Question.Initial

%default total

QuestionState : Questionnaire dataType -> Type
QuestionState (Finished finishedData) = ()
QuestionState (Question questionData nextQuestion) = questionData.State

initialState : (questionnaire : Questionnaire dataType) -> QuestionState questionnaire
initialState (Finished finishedData) = ()
initialState (Question questionData nextQuestion) = questionData.initialState

data State : (questionnaire : Questionnaire dataType) -> Type where
  Init : State questionnaire
  AtQuestion : (currentQuestion : Zipper questionnaire)
             -> QuestionState (fst currentQuestion)
             -> State questionnaire

Event : {questionnaire : Questionnaire dataType} -> State questionnaire -> Type
Event Init = ()
Event (AtQuestion (Finished finishedData ** _) _) = Void
Event (AtQuestion (Question questionData nextQuestion ** _) state) =
  Either (questionData.Event state) questionData.SubmitDataType

update : {questionnaire : Questionnaire dataType}
       -> (state : State questionnaire)
       -> Event state
       -> State questionnaire
update {questionnaire} Init _ =
  AtQuestion (questionnaire ** (EmptyPathUntil, EmptyPathFrom)) (initialState questionnaire)
update (AtQuestion (Finished finishedData ** (pathUntil, pathFrom)) state) event =
  (AtQuestion (Finished finishedData ** (pathUntil, pathFrom)) state)
update (AtQuestion (Question questionData nextQuestion ** (pathUntil, pathFrom)) state) event =
  case event of
    Left localEvent => 
      AtQuestion
        (Question questionData nextQuestion ** (pathUntil, pathFrom)) 
        (questionData.update state localEvent)
    Right dataSubmitted =>
      AtQuestion
        (nextQuestion dataSubmitted ** (AppendToPathUntil questionData nextQuestion dataSubmitted pathUntil, EmptyPathFrom))
        (initialState $ nextQuestion dataSubmitted)

display : Ref Tag.Div
        -> {questionnaire : Questionnaire dataType}
        -> (state : State questionnaire)
        -> (event : Event state)
        -> Cmd (Event (update state event))
display ref {questionnaire} Init _ =
  let cmd = case questionnaire of
              Finished finishedData => finishedData.initialize ref
              Question questionData nextQuestion => questionData.initialize ref
  in batch [ child contentDiv content , cmd ]
display ref (AtQuestion (Finished finishedData ** (pathUntil, pathFrom)) state) event = noAction
display ref (AtQuestion (Question questionData nextQuestion ** (pathUntil, pathFrom)) state) event =
  case event of
    Left localEvent => 
      questionData.display ref state localEvent
    Right dataSubmitted =>
      initialize (nextQuestion dataSubmitted)
  where
    initialize : (subQuestionnaire : Questionnaire dataType)
              -> {pathUntil : PathUntil questionnaire subQuestionnaire}
              -> {pathFrom : PathFrom subQuestionnaire}
              -> Cmd (Event (AtQuestion (subQuestionnaire ** (pathUntil, pathFrom)) (initialState subQuestionnaire)))
    initialize (Finished finishedData) = finishedData.initialize ref
    initialize (Question questionData nextQuestion) = questionData.initialize ref


export covering
ui : (questionnaire : Questionnaire dataType) -> Ref Tag.Div -> IO ()
ui questionnaire ref = runMVC (Event {questionnaire = questionnaire}) update (display ref) (putStrLn . dispErr) Init ()
