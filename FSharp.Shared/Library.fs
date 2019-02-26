namespace DataVault.Shared

module Env =
    let envVars = 
        System.Environment.GetEnvironmentVariables()
        |> Seq.cast<System.Collections.DictionaryEntry>
        |> Seq.map (fun d -> d.Key :?> string, d.Value :?> string)
        |> dict

    let getInt (key:string): int = 
        envVars.Item(key) |> int

    let getString (key:string): string =
        envVars.Item(key)

module Parser =
    open System
    open System.Collections.Generic
    open SharpYaml.Serialization
    open System.IO

    type Scalar =
        | Int of int
        | Int64 of int64
        | String of string
        | TimeSpan of TimeSpan
        | Bool of bool
        | Uri of Uri
        | Float of double
        | Guid of Guid

        static member FromObj : obj -> Scalar = function
            | null -> String ""
            | :? System.Boolean as b -> Bool b
            | :? System.Int32 as i -> Int i
            | :? System.Int64 as i -> Int64 i
            | :? System.Double as d -> Float d
            | :? System.String as s -> String s
            | t -> failwithf "Unknown type %s" (string (t.GetType()))

        member x.BoxedValue =
            match x with
            | Int x -> box x
            | Int64 x -> box x
            | String x -> box x
            | TimeSpan x -> box x
            | Bool x -> box x
            | Uri x -> box x
            | Float x -> box x
            | Guid x -> box x

    type Node =
        | Scalar of Scalar
        | List of Node list
        | Map of (string * Node) list

        member x.GetSection (key: string): Node =
            match x with
            | Map x -> x |> List.find (fun (k,v) -> k = key) |> snd
            | _ -> failwith "Not a map type"
        
        member x.GetString (key: string): string =
            x.GetSection key |> function | Scalar x -> x.BoxedValue :?> string | _ -> failwith "Not a string type";

        member x.GetInt (key: string): int =
            x.GetSection key |> function | Scalar x -> x.BoxedValue :?> int | _ -> failwith "Not an int type";

    let parse : (Stream -> Node) =
        let rec loop (n: obj) =
            match n with
            | :? List<obj> as l -> Node.List (l |> Seq.map loop |> Seq.toList)
            | :? Dictionary<obj,obj> as map ->
                map
                |> Seq.map (fun p -> string p.Key, loop p.Value)
                |> Seq.toList
                |> Map
            | scalar -> Scalar (Scalar.FromObj scalar)

        let settings = SerializerSettings (EmitDefaultValues = true, EmitTags = false, SortKeyForMapping = false)
        let serializer = Serializer(settings)
        fun stream ->
            try serializer.Deserialize(stream) |> loop
            with
              | :? SharpYaml.YamlException as e when e.InnerException <> null ->
                  raise e.InnerException // inner exceptions are much more informative
              | _ -> reraise()
