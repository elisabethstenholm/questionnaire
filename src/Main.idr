module Main

import UI
import ValidData
import Static
import Question.Initial

%default total

covering
main : IO ()
main = ui Question.Initial.question questionDiv