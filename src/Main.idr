module Main

import UI
import ValidData
import Question.Initial

%default total

covering
main : IO ()
main = ui Question.Initial.question