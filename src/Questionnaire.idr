module Questionnaire

import Web.MVC

%default total

namespace Finished
  ||| The data needed at the last page of the questionnaire
  public export
  record Data (dataType : Type) where
    constructor MkData
    ||| A command that initializes the page at the given ref
    initialize : Ref Tag.Div -> Cmd Void
    ||| An element of the data type that the questionnaire should return
    validData : dataType

namespace Question
  ||| The data needed at a question page
  public export
  record Data where
    constructor MkData
    ||| The states of the question page
    State : Type
    ||| The events at each state
    Event : State -> Type
    ||| The data type of the information gained at this question
    AnswerType : Type
    ||| The initial state of the question page
    initialState : State
    ||| A command that initializes the page at the given ref
    ||| This can return either an event or an element of the type submitted in this question
    initialize : Ref Tag.Div -> Cmd (Either (Event initialState) AnswerType)
    ||| A function for updating the state
    update : (state : State) -> Event state -> State
    ||| A function for displaying the state
    ||| This can return either an event or an element of the type submitted in this question
    display : Ref Tag.Div
            -> (state : State)
            -> (event : Event state)
            -> Cmd (Either (Event (update state event)) AnswerType)

||| A questionnaire is a graph of finite depth.
||| The last node should contain an element of the data type of valid
||| answers to the complete questionnaire. Aka when we're done, we should
||| have all the data that we asked for.
public export
data Questionnaire : (dataType : Type) -> Type where
  ||| The last node in the graph/last question in the questionnaire
  Finished : Finished.Data dataType -> Questionnaire dataType
  ||| A question node, which contains information about where to go next
  Question : (questionData : Question.Data)
           -> (nextQuestion : questionData.AnswerType -> Questionnaire dataType)
           -> Questionnaire dataType

||| Answers to a questionnaire are paths in the corresponding graph.

||| The type of paths going out from a given node
public export
data PathFrom : (questionnaire : Questionnaire dataType) -> Type where
  ||| There is always the empty path going out from any node
  EmptyPathFrom : PathFrom questionnaire
  ||| If we have a path going out from b and an edge from a to b then we can prepend that
  ||| edge to the path and get a path going out from a
  PrependToPathFrom : (questionData : Question.Data)
                    -> (nextQuestion : questionData.AnswerType -> Questionnaire dataType)
                    -> (answer : questionData.AnswerType)
                    -> PathFrom (nextQuestion answer)
                    -> PathFrom (Question questionData nextQuestion)

||| The type of paths from a given node to a given node
public export
data PathUntil :  (questionnaire, subQuestionnaire : Questionnaire dataType) -> Type where
  ||| There is always the empty path going from a node to itself
  EmptyPathUntil : PathUntil questionnaire questionnaire
  ||| If we have a path going from a to b and an edge from b to c then we can append that
  ||| edge to the path and get a path going from a to c
  AppendToPathUntil : (questionData : Question.Data)
                    -> (nextQuestion : questionData.AnswerType -> Questionnaire dataType)
                    -> (answer : questionData.AnswerType)
                    -> PathUntil questionnaire (Question questionData nextQuestion)
                    -> PathUntil questionnaire (nextQuestion answer)

||| The zipper of a questionnaire represents the state of the answers so far.
||| It consists of the question the user is currently at, the path they have
||| taken so far, and potentially a path going out from the current question,
||| in case they have gone back in the questionnaire.
public export
Zipper : {dataType : Type} -> Questionnaire dataType -> Type
Zipper questionnaire =
  (subQuestionnaire : Questionnaire dataType ** (PathUntil questionnaire subQuestionnaire, PathFrom subQuestionnaire))


||| Moving forward and back in the questionnaire zipper
export
stepForward : Zipper questionnaire -> Zipper questionnaire
-- If there is no outgoing path, then stepping forward doesn't do anything
stepForward (subQuestionnaire ** (pathUntil, EmptyPathFrom)) = (subQuestionnaire ** (pathUntil, EmptyPathFrom))
-- If there is an outgoing path, then we make the next node in the path the current node
-- and append to the PathUntil
stepForward ((Question _ _) ** (pathUntil, (PrependToPathFrom questionData nextQuestion ch pathFrom))) =
  (nextQuestion ch ** (AppendToPathUntil questionData nextQuestion ch pathUntil, pathFrom))

export
stepBackward : Zipper questionnaire -> Zipper questionnaire
-- If there is no previous path, then stepping backward doesn't do anything
stepBackward (subQuestionnaire ** (EmptyPathUntil , pathFrom)) = (subQuestionnaire ** (EmptyPathUntil , pathFrom))
-- If there is a previous path going into the current node, then we make the previous node the current node
-- and prepend to the PathFrom
stepBackward ((nextQuestion ch) ** (AppendToPathUntil questionData nextQuestion ch pathUntil, pathFrom)) =
  ((Question questionData nextQuestion) ** (pathUntil, PrependToPathFrom questionData nextQuestion ch pathFrom))

