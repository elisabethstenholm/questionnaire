module Questionnaire

import Web.MVC

public export
data GlobalEvent : (e : Type) -> (d : Type) -> Type where
  LocalEvent : forall e, d. (localEvent : e) -> GlobalEvent e d
  SubmitData : forall e, d. (dataSubmitted : d) -> GlobalEvent e d

export
contentDiv : Ref Tag.Body
contentDiv = Id "content"

export
questionDiv : Ref Div
questionDiv = Id "question_div"

export
content : Node event
content =
  div []
      [ h1 [] ["Phone number questionnaire"]
      , div [ Id questionDiv ] [] ]
