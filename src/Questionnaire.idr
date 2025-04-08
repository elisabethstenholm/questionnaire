module Questionnaire

import Web.MVC


export
contentDiv : Ref Tag.Body
contentDiv = Id "content"

export
questionDiv : Ref Tag.Div
questionDiv = Id "question_div"

export
content : Node e
content =
  div []
      [ h1 [] ["Phone number questionnaire"]
      , div [ Id questionDiv ] [] ]


public export
data GlobalEvent : (eventType : Type) -> (dataType : Type) -> Type where
  LocalEvent : forall eventType, dataType. (localEvent : eventType) -> GlobalEvent eventType dataType
  SubmitData : forall eventType, dataType. (dataSubmitted : dataType) -> GlobalEvent eventType dataType

public export
record FinishedData where
  constructor MkFinishedData
  finishedEvent : Type
  initializeFinished : Ref Tag.Div -> Cmd finishedEvent

public export
record QuestionData where
  constructor MkQuestionData
  questionState : Type
  questionEvent : questionState -> Type
  validData : Type
  initialState : questionState
  initializeQuestion : Ref Tag.Div -> Cmd (GlobalEvent (questionEvent initialState) validData)
  update : (st : questionState) -> questionEvent st -> questionState
  display : Ref Tag.Div -> (st : questionState) -> (ev : questionEvent st) -> Cmd (GlobalEvent (questionEvent (update st ev)) validData)

public export
data Questionnaire : (dataType : Type) -> Type where
  Finished : FinishedData -> dataType -> Questionnaire dataType
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

