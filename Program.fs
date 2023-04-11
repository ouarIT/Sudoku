open busqueda
open System

//definimos el estado inicial
let caso1 = [
    [0; 6; 0; 1; 0; 4; 0; 5; 0];
    [0; 0; 8; 3; 0; 5; 6; 0; 0];
    [2; 0; 0; 0; 0; 0; 0; 0; 1];
    [8; 0; 0; 4; 0; 7; 0; 0; 6];
    [0; 0; 6; 0; 0; 0; 3; 0; 0];
    [7; 0; 0; 9; 0; 1; 0; 0; 4];
    [5; 0; 0; 0; 0; 0; 0; 0; 2];
    [0; 0; 7; 2; 0; 6; 9; 0; 0];
    [0; 4; 0; 5; 0; 8; 0; 7; 0]
]

let caso2 = [
    [0 ;0 ;0 ;0 ;0 ;4 ;9 ;0 ;0];
    [0 ;0 ;5 ;3 ;2 ;0 ;0 ;0 ;0];
    [2; 0; 0; 0; 0; 6; 0; 4; 0];
    [8; 0; 4; 0; 0; 0; 0; 6; 0];
    [0; 5; 0; 0; 6; 0; 0; 1; 0];
    [0; 1; 0; 0; 0; 0; 3; 0; 9];
    [0; 2; 0; 8; 0; 0; 0; 0; 6];
    [0; 0; 0; 0; 7; 9; 1; 0; 0];
    [0; 0; 9; 5; 0; 0; 0; 0; 0]
]

let caso3 = [
    [0; 0; 9; 0; 2; 8; 0; 0; 0];
    [0; 8; 0; 0; 0; 0; 9; 0; 0];
    [0; 7; 0; 0; 5; 0; 0; 0; 0];
    [0; 3; 8; 9; 0; 0; 1; 0; 5];
    [0; 0; 0; 0; 0; 0; 0; 0; 0];
    [6; 0; 4; 0; 0; 5; 2; 9; 0];
    [0; 0; 0; 0; 4; 0; 0; 6; 0];
    [0; 0; 6; 0; 0; 0; 0; 3; 0];
    [0; 0; 0; 7; 3; 0; 5; 0; 0]
]
(*
let rec Newton_Raphson N k b =
    let f = pown b (k+1) + b* (1.0-float(N)) - 1.0
    let f' = (k+1)*(pown b k) + 1.0 - 1.0
    let b' = b - f/f'
    if abs(b-b') < 0.0001 then 
        b' 
    else 
        Newton_Raphson N K b'
*)
let leerEstado ()=
    let mutable estado = []
    for i = 1 to 9 do //lectura con formato de entrada 
        printf "Ingresa la fila %A, cada numero separado por espacios: " i
        let input = Console.ReadLine()
        let fila = input.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) |> Array.map int |> Array.toList
        estado <- estado @ [fila]
    estado

let imprimeEstado (estado:List<List<int>>) =
    for i = 0 to 8 do
        for j = 0 to 8 do
            printf "%A " estado.[i].[j]
        printfn ""

let formato_salida estado dfs_o_ids =
    printfn "Estado inicial:"
    imprimeEstado estado
    printfn "Estado final:"
    if dfs_o_ids =  0 then
        let t0 = System.DateTime.UtcNow
        match Capitulo3.busquedaArbol DFS.estrategia (Sudoku.problema estado) with
        | Some n -> imprimeEstado n.estado
        | None -> printfn "No hay solucion"
        let t1 = System.DateTime.UtcNow
        printfn "Tiempo transcurrido con DFS: %A segundos" (t1 - t0).TotalSeconds
    else
        let t2 = System.DateTime.UtcNow
        match IDS.IDS (Sudoku.problema estado) with
        | Some n -> imprimeEstado n.estado
        | None -> printfn "No hay solucion"
        let t3 = System.DateTime.UtcNow
        // muestra el resultado en segundos
        printfn "Tiempo transcurrido con IDS: %A segundos" (t3 - t2).TotalSeconds

let segundo_menu estado =
    let mutable opc = 0
    while opc <> 3 do
        printfn "1. Resolver con DFS"
        printfn "2. Resolver con IDS"
        printfn "3. Salir"
        printf "Ingresa una opcion: "
        opc <- Console.ReadLine() |> int
        match opc with
        | 1 -> formato_salida estado 0
        | 2 -> formato_salida estado 1
        | 3 -> printfn "regresando..."
        | _ -> printfn "Opcion invalida"

let menu() = 
    let mutable opc = 0
    while opc <> 5 do
        printfn "1. Leer entrada personalizada"
        printfn "2. Resolver caso 1"
        printfn "3. Resolver caso 2"
        printfn "4. Resolver caso 3"
        printfn "5. Salir"
        printf "Ingresa una opcion: "
        opc <- Console.ReadLine() |> int
        match opc with
        | 1 -> formato_salida (leerEstado()) 0
        | 2 -> segundo_menu caso1
        | 3 -> segundo_menu caso2
        | 4 -> segundo_menu caso3
        | 5 -> printfn "Adios"
        | _ -> printfn "Opcion invalida"

menu()