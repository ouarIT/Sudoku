namespace busqueda

module DFSL =
    open Pila
    let estrategia l =
        {
            vacia = empty
            insertar = 
                fun pila n ->
                    if n.profundidad <= l then
                        push pila n
                    else
                        pila
            remover = pop
        }