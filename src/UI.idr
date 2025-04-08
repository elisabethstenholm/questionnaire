
module UI

import Data.Vect

import Web.MVC

import ValidData
import Questionnaire
import Question.Initial
-- import Question.Phonenumber
-- import Question.Finished

%default total



QuestionState : Questionnaire dataType -> Type
QuestionState (Finished finishedData validData) = ()
QuestionState (Question questionData nextQuestion) = questionData.questionState

initialState : (questionnaire : Questionnaire dataType) -> QuestionState questionnaire
initialState (Finished finishedData validData) = ()
initialState (Question questionData nextQuestion) = questionData.initialState

data State : (questionnaire : Questionnaire dataType) -> Type where
  Init : State questionnaire
  AtQuestion : (currentQuestion : QuestionnaireZipper questionnaire)
             -> QuestionState (fst currentQuestion)
             -> State questionnaire

Event : State questionnaire -> Type
Event Init = ()
Event (AtQuestion (Finished finishedData validData ** _) _) = finishedData.finishedEvent
Event (AtQuestion (Question questionData nextQuestion ** _) state) =
  GlobalEvent (questionData.questionEvent state) questionData.validData

initialize : (subQuestionnaire : Questionnaire dataType)
           -> {pathUntil : PathUntil questionnaire subQuestionnaire}
           -> {pathFrom : PathFrom subQuestionnaire}
           -> Cmd (Event (AtQuestion (subQuestionnaire ** (pathUntil, pathFrom)) (initialState subQuestionnaire)))
initialize (Finished finishedData validData) = finishedData.initializeFinished questionDiv
initialize (Question questionData nextQuestion) = questionData.initializeQuestion questionDiv

update : {questionnaire : Questionnaire dataType}
       -> (state : State questionnaire)
       -> Event state
       -> State questionnaire
update {questionnaire} Init _ =
  AtQuestion (questionnaire ** (EmptyPathUntil, EmptyPathFrom)) (initialState questionnaire)
update (AtQuestion (Finished finishedData validData ** (pathUntil, pathFrom)) state) event =
  (AtQuestion (Finished finishedData validData ** (pathUntil, pathFrom)) state)
update (AtQuestion (Question questionData nextQuestion ** (pathUntil, pathFrom)) state) event =
  case event of
    LocalEvent localEvent => 
      AtQuestion
        (Question questionData nextQuestion ** (pathUntil, pathFrom)) 
        (questionData.update state localEvent)
    SubmitData dataSubmitted =>
      AtQuestion
        (nextQuestion dataSubmitted ** (AppendToPathUntil questionData nextQuestion dataSubmitted pathUntil, EmptyPathFrom))
        (initialState $ nextQuestion dataSubmitted)

display : {questionnaire : Questionnaire dataType}
        -> (state : State questionnaire)
        -> (event : Event state)
        -> Cmd (Event (update state event))
display {questionnaire} Init _ =
  case questionnaire of
    Finished finishedData validData => finishedData.initializeFinished questionDiv
    Question questionData nextQuestion => questionData.initializeQuestion questionDiv
display (AtQuestion (Finished finishedData validData ** (pathUntil, pathFrom)) state) event = noAction
display (AtQuestion (Question questionData nextQuestion ** (pathUntil, pathFrom)) state) event =
  case event of
    LocalEvent localEvent => 
      questionData.display questionDiv state localEvent
    SubmitData dataSubmitted =>
      initialize (nextQuestion dataSubmitted)


export covering
ui : IO ()
ui = runMVC (Event {questionnaire = Question.Initial.question}) update display (putStrLn . dispErr) Init ()
