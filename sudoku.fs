namespace busqueda

module Sudoku=
    // la unica accion que puede hacer es poner un numero en una casilla
    type accion = 
        | Put
    
    // definimos el estado como una lista de listas de enteros
    type estado = List<List<int>>
    
    //el costo es indiferente
    let costo _ _ _ = 0.0

    // definimos la que obtiene la region dependiendo de la coordenada ingresada
    let obtener_region (estado: int list list)  i j =
    // definimos la region
        let iaux = 
            // si i esta entre 0 y 3 entonces iaux = 0
            if i >= 0 && i < 3 then
                0
            // si i esta entre 3 y 6 entonces iaux = 3
            elif i >= 3 && i < 6 then
                3
            // si i esta entre 6 y 9 entonces iaux = 6
            elif i >= 6 && i < 9 then
                6
            // si no se cumple ninguna de las anteriores entonces iaux = 0
            else
                0
        // definimos la region para las columnas
        let jaux = 
            // si j esta entre 0 y 3 entonces jaux = 0
            if j >= 0 && j < 3 then
                0
            // si j esta entre 3 y 6 entonces jaux = 3
            elif j >= 3 && j < 6 then
                3
            // si j esta entre 6 y 9 entonces jaux = 6
            elif j >= 6 && j < 9 then
                6
            // si no se cumple ninguna de las anteriores entonces jaux = 0
            else
                0
        // definimos la region
        let region = [
                estado.[iaux].[jaux];
                estado.[iaux].[jaux+1];
                estado.[iaux].[jaux+2];
                estado.[iaux+1].[jaux];
                estado.[iaux+1].[jaux+1];
                estado.[iaux+1].[jaux+2];
                estado.[iaux+2].[jaux];
                estado.[iaux+2].[jaux+1];
                estado.[iaux+2].[jaux+2]
            ]
        // retornamos la region
        region

    // definimos la funcion que nos ayuda a validar una region
    let validar_region (estado: int list list) i j =
        // obtenemos la region
        let region = obtener_region estado i j
        // ordenamos la lista de enteros
        let region = List.sort region 
        // comparamos con una lista predefinida
        let region = List.forall2 (fun x y -> x = y) [1;2;3;4;5;6;7;8;9] region 
        // retornamos la region
        region

    //esta funcio devuelve la coordenada que encuentra con cero
    let rec obtener_coor_cero (estado: int list list) i j =
        // si i es igual al tamaño de la lista de listas entonces devuelve una lista vacia
        if i = estado.Length then
            (-1, -1)
        // si j es igual al tamaño de la lista de enteros entonces llama a la funcion con i+1 y j=0
        elif j = estado.[i].Length then
            obtener_coor_cero estado (i+1) 0
        // si no se cumple ninguna de las anteriores entonces
        else
            // si el valor de la coordenada es cero entonces devuelve una tupla con la coordenada
            if estado.[i].[j] = 0 then
                // devuelve una tupla
                (i, j)
            // si no se cumple ninguna de las anteriores entonces llama a la funcion con i y j+1
            else
                obtener_coor_cero estado i (j+1)

    let rec validaciones_regiones (estado: int list list) i j =
        if i = 9 then
            true
        elif j = 9 then
            validaciones_regiones estado (i+3) 0
        else
            if estado.[i].[j] <> 0 then
                validaciones_regiones estado i (j+1)
            else
                let region = obtener_region estado i j // region es una lista de enteros
                let region = List.sort region // ordenamos la lista de enteros
                let region = List.forall2 (fun x y -> x = y) [1;2;3;4;5;6;7;8;9] region // comparamos con una lista predefinida
                region && validaciones_regiones estado i (j+3)

    // definimos la funcion meta la cual recibe el estado y devuelve un booleano
    // estado es una lista de lista de enteros
    let meta estado =
        // verificamos que no haya ceros en el estado
        estado
        |> List.concat // convertimos la lista de lista de enteros en una lista de enteros
        |> List.forall (fun x -> x <> 0) // verificamos que no haya ceros
        // verificamos que la fila contenga numeros del 1 al 9
        && estado
            |> List.forall (fun fila ->
                fila // fila es una lista de enteros
                |> List.sort // ordenamos la lista de enteros
                |> List.forall2 (fun x y -> x = y) [1;2;3;4;5;6;7;8;9] // comparamos con una lista predefinida
            )
        // verificamos que la columna contenga numeros del 1 al 9
        && estado
            |> List.transpose // transponemos la lista de lista de enteros
            |> List.forall (fun columna ->
                columna // columna es una lista de enteros
                |> List.sort // ordenamos la lista de enteros
                |> List.forall2 (fun x y -> x = y) [1;2;3;4;5;6;7;8;9] // comparamos con una lista predefinida
            )
        // verificamos que la region contenga numeros del 1 al 9
        && validaciones_regiones estado 0 0

        // creamos una funcion que nos ayude a obtener los posibles valores de una casilla
    let obtener_posibles_valores (estado: int list list) i j =
        // verificamos que la casilla este vacia
        if estado.[i].[j] <> 0 then
            []
        else
        // si la casilla esta vacia    
            // obtenemos los valores de la fila
            let fila = estado.[i]
            // obtenemos los valores de la columna
            let columna = estado |> List.transpose |> List.item j
            // obtenemos los valores de la region
            let region = obtener_region estado i j
            // obtenemos los valores posibles
            // este pedazo filtra los valores que no se encuentran
            let posibles = [1;2;3;4;5;6;7;8;9] |> List.filter (fun x -> not (List.contains x fila)) |> List.filter (fun x -> not (List.contains x columna)) |> List.filter (fun x -> not (List.contains x region))
            posibles
            
    let sucesores estado =
        // definimos la accion
        let accion = Put 
        // obtenemos la coordenada de la casilla vacia
        let (i, j) = obtener_coor_cero estado 0 0
        // obtenemos los posibles valores de la casilla
        let posibles = obtener_posibles_valores estado i j
        // emparejamos la accion
        // si la accion es Put (unica)
        match accion with
        | Put ->
            // si la casilla esta vacia y hay posibles valores
            if estado.[i].[j] = 0 && posibles.Length <> 0 then
                // creamos una lista
                seq {
                    // recorremos la lista de posibles valores
                    for pos in posibles do
                        // retornamos una tupla con la accion y el estado
                        yield (accion, List.updateAt i (List.updateAt j pos (estado.[i])) estado)

                } |> Seq.toList // convertimos la secuencia en una lista
            // si la casilla esta vacia y no hay posibles valores
            else []

            
    // definimos el problema
    let problema estado = {
        inicio = estado 
        meta = meta
        sucesores = sucesores
        costo = costo
    }