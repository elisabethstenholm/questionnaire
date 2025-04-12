module Main

import Web.MVC

import UI
import ValidData
import Question.Initial

%default total

questionDiv : Ref Tag.Div
questionDiv = Id "question_div"

covering
main : IO ()
main = 
  ui $
    MkData
      { questionRef = questionDiv,
        title = h1 [] ["Phonenumber questionnaire"],
        questionnaire = Question.Initial.question }
