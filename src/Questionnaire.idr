module Questionnaire

import Web.MVC

public export
data GlobalEvent : (eventType : Type) -> (dataType : Type) -> Type where
  LocalEvent : forall eventType, dataType. (localEvent : eventType) -> GlobalEvent eventType dataType
  SubmitData : forall eventType, dataType. (dataSubmitted : dataType) -> GlobalEvent eventType dataType

public export
record QuestionData where
  constructor MkQuestionData
  state : Type
  event : state -> Type
  choice : Type
  initialState : state
  initialize : Cmd (GlobalEvent (event initialState) choice)
  update : (st : state) -> event st -> state
  display : (st : state) -> (ev : event st) -> Cmd (GlobalEvent (event (update st ev)) choice)

public export
data Questionnaire : (dataType : Type) -> Type where
  Finished : forall e. dataType -> Cmd e -> Questionnaire dataType
  Question : (questionData : QuestionData)
           -> (nextQuestion : questionData.choice -> Questionnaire dataType)
           -> Questionnaire dataType


data PathFrom : (questionnaire : Questionnaire dataType) -> Type where
  EmptyPath : PathFrom questionnaire
  PrependToPath : (questionData : QuestionData)
                -> (nextQuestion : questionData.choice -> Questionnaire dataType)
                -> (ch : questionData.choice)
                -> PathFrom (nextQuestion ch)
                -> PathFrom (Question questionData nextQuestion)

data PathUntil :  (questionnaire, subQuestionnaire : Questionnaire dataType) -> Type where
  EmptyPathUntil : PathUntil questionnaire questionnaire
  AppendToPathUntil : (questionData : QuestionData)
                    -> (nextQuestion : questionData.choice -> Questionnaire dataType)
                    -> (ch : questionData.choice)
                    -> PathUntil questionnaire (Question questionData nextQuestion)
                    -> PathUntil questionnaire (nextQuestion ch)

QuestionnaireZipper : {dataType : Type} -> Questionnaire dataType -> Type
QuestionnaireZipper {dataType=dataType} questionnaire =
  (subQuestionnaire : Questionnaire dataType ** (PathUntil questionnaire subQuestionnaire, PathFrom subQuestionnaire))


||| Moving forward and back in the questionnaire zipper

stepForward : QuestionnaireZipper questionnaire -> QuestionnaireZipper questionnaire
stepForward (subQuestionnaire ** (pathUntil, EmptyPath)) = (subQuestionnaire ** (pathUntil, EmptyPath))
stepForward ((Question _ _) ** (pathUntil, (PrependToPath questionData nextQuestion ch pathFrom))) =
  (nextQuestion ch ** (AppendToPathUntil questionData nextQuestion ch pathUntil, pathFrom))

stepBackward : QuestionnaireZipper questionnaire -> QuestionnaireZipper questionnaire
stepBackward (subQuestionnaire ** (EmptyPathUntil , pathFrom)) = (subQuestionnaire ** (EmptyPathUntil , pathFrom))
stepBackward ((nextQuestion ch) ** (AppendToPathUntil questionData nextQuestion ch pathUntil, pathFrom)) =
  ((Question questionData nextQuestion) ** (pathUntil, PrependToPath questionData nextQuestion ch pathFrom))


export
contentDiv : Ref Tag.Body
contentDiv = Id "content"

export
questionDiv : Ref Div
questionDiv = Id "question_div"

export
content : Node e
content =
  div []
      [ h1 [] ["Phone number questionnaire"]
      , div [ Id questionDiv ] [] ]
