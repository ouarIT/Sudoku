namespace busqueda

type pila<'a> = list<'a>

module Pila =
    let push pila x = x :: pila
    let pop pila = 
        match pila with
        | h :: t -> Some (h, t)
        | [] -> None
    
    let empty = []
