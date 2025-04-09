module Questionnaire

import Web.MVC

%default total

public export
record FinishedData (dataType : Type) where
  constructor MkFinishedData
  initializeFinished : Ref Tag.Div -> Cmd Void
  finishedValidData : dataType

public export
record QuestionData where
  constructor MkQuestionData
  questionState : Type
  questionEvent : questionState -> Type
  validData : Type
  initialState : questionState
  initializeQuestion : Ref Tag.Div -> Cmd (Either (questionEvent initialState) validData)
  update : (st : questionState) -> questionEvent st -> questionState
  display : Ref Tag.Div -> (st : questionState) -> (ev : questionEvent st) -> Cmd (Either (questionEvent (update st ev)) validData)

public export
data Questionnaire : (dataType : Type) -> Type where
  Finished : FinishedData dataType -> Questionnaire dataType
  Question : (questionData : QuestionData)
           -> (nextQuestion : questionData.validData -> Questionnaire dataType)
           -> Questionnaire dataType

public export
data PathFrom : (questionnaire : Questionnaire dataType) -> Type where
  EmptyPathFrom : PathFrom questionnaire
  PrependToPathFrom : (questionData : QuestionData)
                    -> (nextQuestion : questionData.validData -> Questionnaire dataType)
                    -> (ch : questionData.validData)
                    -> PathFrom (nextQuestion ch)
                    -> PathFrom (Question questionData nextQuestion)

public export
data PathUntil :  (questionnaire, subQuestionnaire : Questionnaire dataType) -> Type where
  EmptyPathUntil : PathUntil questionnaire questionnaire
  AppendToPathUntil : (questionData : QuestionData)
                    -> (nextQuestion : questionData.validData -> Questionnaire dataType)
                    -> (ch : questionData.validData)
                    -> PathUntil questionnaire (Question questionData nextQuestion)
                    -> PathUntil questionnaire (nextQuestion ch)

public export
QuestionnaireZipper : {dataType : Type} -> Questionnaire dataType -> Type
QuestionnaireZipper {dataType=dataType} questionnaire =
  (subQuestionnaire : Questionnaire dataType ** (PathUntil questionnaire subQuestionnaire, PathFrom subQuestionnaire))


||| Moving forward and back in the questionnaire zipper

stepForward : QuestionnaireZipper questionnaire -> QuestionnaireZipper questionnaire
stepForward (subQuestionnaire ** (pathUntil, EmptyPathFrom)) = (subQuestionnaire ** (pathUntil, EmptyPathFrom))
stepForward ((Question _ _) ** (pathUntil, (PrependToPathFrom questionData nextQuestion ch pathFrom))) =
  (nextQuestion ch ** (AppendToPathUntil questionData nextQuestion ch pathUntil, pathFrom))

stepBackward : QuestionnaireZipper questionnaire -> QuestionnaireZipper questionnaire
stepBackward (subQuestionnaire ** (EmptyPathUntil , pathFrom)) = (subQuestionnaire ** (EmptyPathUntil , pathFrom))
stepBackward ((nextQuestion ch) ** (AppendToPathUntil questionData nextQuestion ch pathUntil, pathFrom)) =
  ((Question questionData nextQuestion) ** (pathUntil, PrependToPathFrom questionData nextQuestion ch pathFrom))

