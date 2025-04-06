module Questionnaire

public export
data GlobalEvent : (e : Type) -> (d : Type) -> Type where
  LocalEvent : forall e, d. (localEvent : e) -> GlobalEvent e d
  SubmitData : forall e, d. (dataSubmitted : d) -> GlobalEvent e d
