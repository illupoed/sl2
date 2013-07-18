DATA Node = Node
DATA Document = Document

PUBLIC FUN document : DOM Document
DEF document = {| document |} : DOM Document

PUBLIC FUN appendChild : Document -> Node -> DOM Void
DEF appendChild doc child = {| $doc.body.appendChild($child) |}

-- for a document creates a new button with name and callback
PUBLIC FUN createButton : Document -> String -> (DOM Void) -> (DOM Node)
DEF createButton doc text callback =
   {| $doc.createElement('button') |} : DOM Node &=
   (\button .
     {| $button.innerText = $text |} &
     {| $button.onclick = $callback |} &
     (yield button))
   
FUN hello : DOM String
DEF hello = (yield 23) & (yield "S")
   
