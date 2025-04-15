
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
  Either (questionData.Event state) questionData.AnswerType

initialize : Ref Tag.Div
          -> (subQuestionnaire : Questionnaire dataType)
          -> {pathUntil : PathUntil questionnaire subQuestionnaire}
          -> {pathFrom : PathFrom subQuestionnaire}
          -> Cmd (Event (AtQuestion (subQuestionnaire ** (pathUntil, pathFrom)) (initialState subQuestionnaire)))
initialize ref (Finished finishedData) = finishedData.initialize ref
initialize ref (Question questionData nextQuestion) = questionData.initialize ref

update : {questionnaire : Questionnaire dataType}
       -> (state : State questionnaire)
       -> Event state
       -> State questionnaire
update {questionnaire} Init _ =
  AtQuestion (questionnaire ** (EmptyPathUntil, EmptyPathFrom)) (initialState questionnaire)
update (AtQuestion (Finished finishedData ** (pathUntil, pathFrom)) state) _ =
  (AtQuestion (Finished finishedData ** (pathUntil, pathFrom)) state)
update (AtQuestion (Question questionData nextQuestion ** (pathUntil, pathFrom)) state) (Left localEvent) =
  AtQuestion
    (Question questionData nextQuestion ** (pathUntil, pathFrom)) 
    (questionData.update state localEvent)
update (AtQuestion (Question questionData nextQuestion ** (pathUntil, pathFrom)) state) (Right dataSubmitted) =
  AtQuestion
    (nextQuestion dataSubmitted ** (AppendToPathUntil questionData nextQuestion dataSubmitted pathUntil, EmptyPathFrom))
    (initialState $ nextQuestion dataSubmitted)

display : Ref Tag.Div
        -> {questionnaire : Questionnaire dataType}
        -> (state : State questionnaire)
        -> (event : Event state)
        -> Cmd (Event (update state event))
display ref {questionnaire} Init _ =
  batch [ child contentDiv content , initialize ref questionnaire ]
display ref (AtQuestion (Finished finishedData ** (pathUntil, pathFrom)) state) event =
  absurd event
display ref (AtQuestion (Question questionData nextQuestion ** (pathUntil, pathFrom)) state) (Left localEvent) =
  questionData.display ref state localEvent
display ref (AtQuestion (Question questionData nextQuestion ** (pathUntil, pathFrom)) state) (Right dataSubmitted) =
  initialize ref (nextQuestion dataSubmitted)


export covering
ui : (questionnaire : Questionnaire dataType) -> Ref Tag.Div -> IO ()
ui questionnaire ref = runMVC (Event {questionnaire = questionnaire}) update (display ref) (putStrLn . dispErr) Init ()
