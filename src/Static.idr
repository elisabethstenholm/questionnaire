module Static

import Web.MVC

%default total

export
contentDiv : Ref Tag.Body
contentDiv = Id "content"

export
questionDiv : Ref Tag.Div
questionDiv = Id "question_div"

export
navigationDiv : Ref Tag.Div
navigationDiv = Id "navigation_div"

export
content : Node e
content =
  div []
      [ h1 [] ["Car crash details"]
      , div [ Id questionDiv ] [] ]
