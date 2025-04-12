module Common

import Web.MVC

%default total

export
button : Ref Tag.Button -> event -> String -> Node event
button ref event label = button [Id ref, onClick event] [Text label]
