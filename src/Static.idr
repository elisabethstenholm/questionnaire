module Static

import Web.MVC

%default total

export
contentDiv : Ref Tag.Body
contentDiv = Id "content"

export
navigationDiv : Ref Tag.Div
navigationDiv = Id "navigation_div"

export
forwardButton : Ref Tag.Button
forwardButton = Id "forward_button"

export
backwardButton : Ref Tag.Button
backwardButton = Id "backward_button"

export
button : Ref Tag.Button -> event -> String -> Node event
button ref event label = button [Id ref, onClick event] [Text label]

export
content : Ref Tag.Div -> Node e -> Node e
content ref title =
  div []
      [ title
      , div [ Id ref ] []
      , div [ Id navigationDiv ] [] ]
