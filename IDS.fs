namespace busqueda

module IDS =

    let IDS problema =
        let rec loop l =
            match 
                Capitulo3.busquedaArbol 
                    (DFSL.estrategia l) 
                    problema with
            | Some n -> Some n
            | None -> loop (l + 1)
        loop 0