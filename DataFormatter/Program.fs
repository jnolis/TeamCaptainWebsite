// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open SharedCore

let getCrewInfo crewFile outputFile =
        use reader = System.IO.File.OpenText(crewFile)
        use csv = new CsvHelper.CsvReader(reader)
        let tempData = new System.Collections.Generic.List<string*string>()
        while csv.Read() do
            tempData.Add((csv.GetField<string>(0), csv.GetField<string>(1)))
        tempData
        |> Seq.map (fun (name,description)-> {Crew.Name=name;Description =Some description; ImageLink = None})
        |> Newtonsoft.Json.JsonConvert.SerializeObject
        |> (fun x -> System.IO.File.WriteAllText(outputFile,x))
let getItemInfo itemFile linkFile outputFile =
    let itemInfo =
        use reader = System.IO.File.OpenText(itemFile)
        use csv = new CsvHelper.CsvReader(reader)
        let tempData = new System.Collections.Generic.List<int*string*int>()
        while csv.Read() do
            tempData.Add((csv.GetField<int>(0), csv.GetField<string>(1),  csv.GetField<int>(2)))
        tempData
        |> Seq.map id
    let linkInfo = 
        use reader = System.IO.File.OpenText(linkFile)
        use csv = new CsvHelper.CsvReader(reader)
        let tempData = new System.Collections.Generic.List<int*Link>()
        while csv.Read() do
            let link = csv.GetField<string>(1)
                        |> (fun x -> if x.Contains("imgur") then
                                        x.Split('/')
                                        |> Array.last
                                        |> (fun (x:string)-> x.Substring(0,x.Length-4))
                                        |> Imgur
                                     else
                                        x.Split('/')
                                        |> Array.last
                                        |> YouTube)
            tempData.Add((csv.GetField<int>(0), link))
        tempData
        |> Seq.map id
        |> Map.ofSeq
    let json = itemInfo
                |> Seq.choose (fun (id,description,points) ->
                                    match Map.tryFind id linkInfo with
                                    | None -> None
                                    | Some link -> Some {ID = id; Description = description; Points = points; Link = link})
                |> Newtonsoft.Json.JsonConvert.SerializeObject
    System.IO.File.WriteAllText(outputFile,json)

[<EntryPoint>]
let main argv = 
    getItemInfo argv.[0] argv.[1] argv.[2]
    getCrewInfo argv.[3] argv.[4]
    0 // return an integer exit code
