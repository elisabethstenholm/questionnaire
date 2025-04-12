
module UI

import Data.Vect

import Web.MVC

import Questionnaire

import ValidData
import Static
import Question.Initial

%default total

namespace AtQuestion
  public export
  record State (questionnaire : Questionnaire dataType) where
    constructor MkState
    questionData : Question.Data
    nextQuestion : questionData.AnswerType -> Questionnaire dataType
    pathUntil : PathUntil questionnaire (Question questionData nextQuestion)
    pathFrom : PathFrom (Question questionData nextQuestion)
    localState : questionData.State
    maybeAnswer : Maybe questionData.AnswerType

||| The global state of a questionnaire
data State : (questionnaire : Questionnaire dataType) -> Type where
  ||| There is an initial state. Only used to set up the questionnaire.
  Init : State questionnaire
  ||| At the last question, the state contains the path traveled to get there,
  ||| in case the user wants to go backward.
  AtFinished : PathUntil questionnaire (Finished finishedData)
             -> State questionnaire
  ||| At a question, the state contains the zipper state, the local question
  ||| state and maybe a valid answer.
  AtQuestion : AtQuestion.State questionnaire -> State questionnaire

||| The global events of the questionnaire
data Event : UI.State questionnaire -> Type where
  ||| The initial event at the initial state. Only used to set up the questionnaire.
  InitEvent : Event Init
  ||| Submitting the answers of the questionnaire at the last question.
  SubmitQuestionnaire : Event (AtFinished path)
  ||| Going to the next question given a path forward and no valid answer
  NextByPath : Event (AtQuestion (AtQuestion.MkState { questionData = questionDt,
                                                       nextQuestion = next,
                                                       pathUntil = pathUntl,
                                                       pathFrom = PrependToPathFrom questionDt next answer pathFr,
                                                       localState = localSt,
                                                       maybeAnswer = Nothing }))
  ||| Going to the next question given a valid answer
  NextByAnswer : Event (AtQuestion (AtQuestion.MkState { questionData = questionDt,
                                                         nextQuestion = next,
                                                         pathUntil = pathUntl,
                                                         pathFrom = pathFr,
                                                         localState = localSt,
                                                         maybeAnswer = Just answer }))
  ||| Going to the previous question
  Previous : Event (AtQuestion (AtQuestion.MkState { questionData = questionDt,
                                                     nextQuestion = next,
                                                     pathUntil = AppendToPathUntil questionDt next answer pathUntl,
                                                     pathFrom = pathFr,
                                                     localState = localSt,
                                                     maybeAnswer = maybeAns }))

{- QuestionState : Questionnaire dataType -> Type
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

namespace Last
  data GlobalEvent : (answerType : Type) -> Type where
    Init : GlobalEvent answerType
    -- SubmitAnswers : completeAnswers -> GlobalEvent completeAnswers
    GoBackward : GlobalEvent answerType

namespace First
  data GlobalEvent : (answerType : Type) -> Type where
    GotAnswer : answerType -> GlobalEvent answerType
    GoForward : GlobalEvent answerType

namespace Middle
  data GlobalEvent : (answerType : Type) -> Type where
    GotAnswer : answerType -> GlobalEvent answerType
    GoBackward : GlobalEvent answerType
    GoForward : GlobalEvent answerType

Functor GlobalEvent where
  map f (GotAnswer x) = GotAnswer (f x)
  map f GoBackward = GoBackward
  map f GoForward = GoForward

Event : UI.State questionnaire -> Type
Event Init = ()
Event (AtQuestion (Finished finishedData ** _) _) = ()
Event (AtQuestion (Question questionData nextQuestion ** _) state) =
  Either (questionData.Event state) (GlobalEvent questionData.AnswerType)

update : {questionnaire : Questionnaire dataType}
       -> (state : State questionnaire)
       -> Event state
       -> State questionnaire
update Init _ =
  AtQuestion (questionnaire ** (EmptyPathUntil, EmptyPathFrom)) (initialState questionnaire)
update (AtQuestion (Finished finishedData ** (pathUntil, pathFrom)) state) event =
  (AtQuestion (Finished finishedData ** (pathUntil, pathFrom)) state)
update (AtQuestion (Question questionData nextQuestion ** (pathUntil, pathFrom)) state) (Left localEvent) =
  AtQuestion
    (Question questionData nextQuestion ** (pathUntil, pathFrom)) 
    (questionData.update state localEvent)
update (AtQuestion (Question questionData nextQuestion ** (pathUntil, pathFrom)) state) (Right (GotAnswer dataSubmitted)) =
  AtQuestion
    (nextQuestion dataSubmitted ** (AppendToPathUntil questionData nextQuestion dataSubmitted pathUntil, EmptyPathFrom))
    (initialState $ nextQuestion dataSubmitted)
update (AtQuestion zipper@(Question questionData nextQuestion ** (pathUntil, pathFrom)) state) (Right GoBackward) =
  AtQuestion (stepBackward zipper) (initialState $ fst $ stepBackward zipper)
update (AtQuestion zipper@(Question questionData nextQuestion ** (pathUntil, pathFrom)) state) (Right GoForward) =
  AtQuestion (stepForward zipper) (initialState $ fst $ stepForward zipper)

display : Ref Tag.Div
        -> (forall e. Node e)
        -> {questionnaire : Questionnaire dataType}
        -> (state : State questionnaire)
        -> (event : Event state)
        -> Cmd (Event (update state event))
display {questionnaire = Finished finishedData} ref title Init _ =
  batch [ child contentDiv (content ref title)
        , finishedData.initialize ref ]
display {questionnaire = Question questionData nextQuestion} ref title Init _ =
  batch [ child contentDiv (content ref title)
        , children navigationDiv [ button backwardButton (Right GoBackward) "Previous"
                                 , button forwardButton (Right GoForward) "Next" ]
        , (GotAnswer <$>) <$> questionData.initialize ref ]
display ref _ (AtQuestion (Finished finishedData ** (pathUntil, pathFrom)) state) event = noAction
display ref _ (AtQuestion (Question questionData nextQuestion ** (pathUntil, pathFrom)) state) (Left localEvent) =
  (GotAnswer <$>) <$> questionData.display ref state localEvent
display ref _ (AtQuestion (Question questionData nextQuestion ** (pathUntil, pathFrom)) state) (Right GoBackward) =
  ?back
display ref _ (AtQuestion (Question questionData nextQuestion ** (pathUntil, pathFrom)) state) (Right GoForward) =
  ?forward
display ref _ (AtQuestion (Question questionData nextQuestion ** (pathUntil, pathFrom)) state) (Right (GotAnswer dataSubmitted)) =
  initialize (nextQuestion dataSubmitted)
  where
    initialize : (subQuestionnaire : Questionnaire dataType)
              -> {pathUntil : PathUntil questionnaire subQuestionnaire}
              -> {pathFrom : PathFrom subQuestionnaire}
              -> Cmd (Event (AtQuestion (subQuestionnaire ** (pathUntil, pathFrom)) (initialState subQuestionnaire)))
    initialize (Finished finishedData) = finishedData.initialize ref
    initialize (Question questionData nextQuestion) = (GotAnswer <$>) <$> questionData.initialize ref
-}

||| All the data needed to create a questionnaire
public export
record Data (dataType : Type) where
  constructor MkData
  ||| The ref to the question div
  questionRef : Ref Tag.Div
  ||| The title content which is static across all questions
  title : forall e. Node e
  ||| The first question of the questionnaire
  questionnaire : Questionnaire dataType

export covering
ui : (uiData : UI.Data dataType) -> IO ()
ui uiData = pure ()
  -- runMVC
  --   (Event {questionnaire = uiData.questionnaire})
  --   update
  --   (display uiData.questionRef uiData.title)
  --   (putStrLn . dispErr)
  --   Init
  --   ()
