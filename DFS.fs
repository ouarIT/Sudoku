namespace busqueda

module DFS =
    open Pila
    let estrategia =
        {
            vacia = empty
            insertar = push
            remover = pop
        }