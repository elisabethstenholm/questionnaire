module Question.Finished

import Data.Vect

import Web.MVC

import ValidData
import Questionnaire

%default total

initialize : ValidData -> Ref Tag.Div -> Cmd Void
initialize validData ref =
  child ref (viewValidData validData)

finishedData : ValidData -> Finished.Data ValidData
finishedData validData =
  MkData {
    initialize = initialize validData,
    validData = validData
  }

export
question : ValidData -> Questionnaire ValidData
question validData = Finished (finishedData validData)
