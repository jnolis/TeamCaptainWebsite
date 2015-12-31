namespace GishwhesWebsite

open WebSharper
open SharedCore

module Server =
    let crew = System.IO.File.ReadAllText(System.Web.HttpContext.Current.Request.PhysicalApplicationPath + @"/Content/Data/Crew.json")
                |> (fun x -> Newtonsoft.Json.JsonConvert.DeserializeObject<seq<Crew>>(x))
                |> Seq.indexed
                |> Map.ofSeq
    let itemsEachYear = System.IO.Directory.GetFiles(System.Web.HttpContext.Current.Request.PhysicalApplicationPath + @"/Content/Data/Items")
                        |> Array.map (fun file -> 
                                        System.Console.Write(file)
                                        (
                                        int (System.IO.Path.GetFileNameWithoutExtension(file)),
                                        Newtonsoft.Json.JsonConvert.DeserializeObject<seq<Item>>(System.IO.File.ReadAllText(file))
                                        |> Seq.map (fun item -> (item.ID,item))
                                        |> Map.ofSeq
                                        ))
                        |> Map.ofArray
    
    let mostRecentYear = itemsEachYear
                            |> Map.toSeq
                            |> Seq.map fst
                            |> Seq.max
        

    let getNextItem (year:int) (itemID:int) =
        match Map.tryFind year itemsEachYear with
        | None -> None
        | Some items -> 
            let ids = items
                        |> Map.toArray
                        |> Array.map (fun (key,item) -> item.ID)
            match Array.tryFindIndex (fun id -> itemID = id) ids with
                | None -> None
                | Some x -> Some (if x = (Array.length ids)-1 then 0 else ids.[x+1])

    let getPreviousItem (year:int) (itemID:int) =
        match Map.tryFind year itemsEachYear with
        | None -> None
        | Some items -> 
            let ids = items
                        |> Map.toArray
                        |> Array.map (fun (key,item) -> item.ID)
            match Array.tryFindIndex (fun id -> itemID = id) ids with
                | None -> None
                | Some x -> Some (if x = 0 then (Array.length ids)-1 else ids.[x-1])


