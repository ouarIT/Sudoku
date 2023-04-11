namespace busqueda

type problema<'s, 'a> = {
    inicio :   's
    sucesores :   's -> list<'a * 's>
    meta :   's -> bool
    costo :   's -> 'a -> 's -> float
}

type nodo<'s, 'a> = {
    profundidad :   int
    costo_ruta :   float
    estado :   's
    accion :   option<'a>
    padre :   option<nodo<'s, 'a>>
}

type estrategia<'s, 'a, 'd> = {
    vacia : 'd
    insertar : 'd -> nodo<'s, 'a> -> 'd
    remover : 'd -> option<nodo<'s, 'a> * 'd>
}

module Capitulo3 =

    let expandir problema padre =
        problema.sucesores padre.estado
        |> List.map (fun (a, s) ->
            {
                profundidad = padre.profundidad + 1
                estado = s
                accion = Some a
                padre = Some padre
                costo_ruta = padre.costo_ruta + problema.costo padre.estado a s
                
            })
    
    let nodoInicial estado =
        {
            estado = estado
            profundidad = 0
            costo_ruta = 0.0
            accion = None
            padre = None 
        }


    let busquedaArbol estrategia problema = 
        let raiz = nodoInicial problema.inicio
        let bolsa = estrategia.insertar estrategia.vacia raiz

        let rec loop bolsa =
            match estrategia.remover bolsa with
            | Some(n, bolsa') ->
                if problema.meta n.estado then Some n
                else 
                    expandir problema n
                    |> List.fold estrategia.insertar bolsa'
                    |> loop
            | None -> None
            
        loop bolsa

    let rec acciones n =
        match n.accion, n.padre with
        | Some a, Some p -> acciones p @ [a]
        | _ -> []