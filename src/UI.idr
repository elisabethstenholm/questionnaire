
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
QuestionState (Question questionData nextQuestion) = questionData.questionState

initialState : (questionnaire : Questionnaire dataType) -> QuestionState questionnaire
initialState (Finished finishedData) = ()
initialState (Question questionData nextQuestion) = questionData.initialState

data State : (questionnaire : Questionnaire dataType) -> Type where
  Init : State questionnaire
  AtQuestion : (currentQuestion : QuestionnaireZipper questionnaire)
             -> QuestionState (fst currentQuestion)
             -> State questionnaire

Event : State questionnaire -> Type
Event Init = ()
Event (AtQuestion (Finished finishedData ** _) _) = Void
Event (AtQuestion (Question questionData nextQuestion ** _) state) =
  Either (questionData.questionEvent state) questionData.validData

initialize : (subQuestionnaire : Questionnaire dataType)
           -> {pathUntil : PathUntil questionnaire subQuestionnaire}
           -> {pathFrom : PathFrom subQuestionnaire}
           -> Cmd (Event (AtQuestion (subQuestionnaire ** (pathUntil, pathFrom)) (initialState subQuestionnaire)))
initialize (Finished finishedData) = finishedData.initializeFinished questionDiv
initialize (Question questionData nextQuestion) = questionData.initializeQuestion questionDiv

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

display : {questionnaire : Questionnaire dataType}
        -> (state : State questionnaire)
        -> (event : Event state)
        -> Cmd (Event (update state event))
display {questionnaire} Init _ =
  let cmd = case questionnaire of
              Finished finishedData => finishedData.initializeFinished questionDiv
              Question questionData nextQuestion => questionData.initializeQuestion questionDiv
  in batch [ child contentDiv content , cmd ]
display (AtQuestion (Finished finishedData ** (pathUntil, pathFrom)) state) event = noAction
display (AtQuestion (Question questionData nextQuestion ** (pathUntil, pathFrom)) state) event =
  case event of
    Left localEvent => 
      questionData.display questionDiv state localEvent
    Right dataSubmitted =>
      initialize (nextQuestion dataSubmitted)


export covering
ui : (questionnaire : Questionnaire dataType) -> IO ()
ui questionnaire = runMVC (Event {questionnaire = questionnaire}) update display (putStrLn . dispErr) Init ()
