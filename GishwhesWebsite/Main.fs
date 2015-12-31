namespace GishwhesWebsite

open WebSharper
open WebSharper.Sitelets
open SharedCore

type EndPoint =
    | [<EndPoint "/">] Home
    | [<EndPoint "/crew">] Crew
    | [<EndPoint "/item-list">] ItemList of year: int
    | [<EndPoint "/item-view-all">] ItemViewAll of year: int
    | [<EndPoint "/item">] Item of year: int * number: int


module Templating =
    open WebSharper.Html.Server

    type Page =
        {
            Title : string
            MenuBar : list<Element>
            Body : list<Element>
        }

    let MainTemplate =
        Content.Template<Page>("~/Main.html")
            .With("title", fun x -> x.Title)
            .With("menubar", fun x -> x.MenuBar)
            .With("body", fun x -> x.Body)

    // Compute a menubar where the menu item for the given endpoint is active
    let MenuBar (ctx: Context<EndPoint>) endpoint =
        let ( => ) txt act =
             LI [if endpoint = act then yield Attr.Class "active"] -< [
                A [Attr.HRef (ctx.Link act)] -< [Text txt]
             ]
        List.append
            [
                LI ["Home" => EndPoint.Home]
                LI ["The Crew" => EndPoint.Crew]
            ]
            (Server.itemsEachYear
                |> Map.toList
                |> List.map (fun (key,items) -> LI [(key.ToString() + " Items") => (EndPoint.ItemList key)]))
    let Main ctx endpoint title body : Async<Content<EndPoint>> =
        Content.WithTemplate MainTemplate
            {
                Title = "Team Captain - " + title
                MenuBar = MenuBar ctx endpoint
                Body = body
            }

module Site =
    open WebSharper.Html.Server
    let ErrorPage ctx (message:string) =
        Templating.Main ctx EndPoint.Crew "Nice Try" [
            H1 [Text "ಠ_ಠ"]
            P [Text message]
        ]

    let HomePage (ctx: Context<EndPoint>) =

        let sectionInfo = seq [("The Crew","Meet the stuff legends are made off: captains.",ctx.Link Crew, false);
                               ("The Goods","The items created for "+ Server.mostRecentYear.ToString() + ": the most recent gishwhes hunt.", ctx.Link (ItemList Server.mostRecentYear), false);
                               ("The Hunt","Learn more about what Gishwhes is and what's the point of it all.", "https://gishwhes.com/", true)]
                            |> Seq.map (fun (title,text,link,isNewWindow) -> 
                                            Div [ H2 [Text title]; P [Text text]; P [ A [Text "Learn More"] -< ((seq [Attr.HRef link; Attr.Class "btn btn-default"])
                                                                                                                |> (match isNewWindow with
                                                                                                                                | false -> id
                                                                                                                                | true -> (fun (x:Attribute seq) -> Seq.append x (Seq.singleton (Attr.Target "_blank")))))
                                                                                                                                ]] -< [Attr.Class "col-md-4"])
        Templating.Main (ctx: Context<EndPoint>) EndPoint.Home "Home" [
            Div [ Div [H1 [Text "Obey your captain"]; P [ Text "The stormy seas hottest Gishwhes team for one year and counting"]] -< [Attr.Class "container"]] -< [Attr.Class "jumbotron"]
            Div [
                (Div sectionInfo) -< [Attr.Class "row"]
                ] -< [Attr.Class "container"]
        ]

    let CrewPage ctx =
        let crewElements = Server.crew
                            |> Map.toSeq
                            |> Seq.map (fun (key,(crewMember:Crew)) -> TR [TD [Text crewMember.Name]; TD [Text (match crewMember.Description with | Some x -> x | None -> "")]])
                            |> (fun rows -> seq [ THead [TH [Text "Name"]; TH [Text "Description"]]; TBody rows])
                            |> (fun rows -> Table rows -< [Class "table table-striped"])
        Templating.Main ctx EndPoint.Crew "Crew" [
            H1 [Text "Crew"]
            P [Text "The crew of the ship."]
            crewElements
        ]

    let linkToEmbedded (link:Link) =
        match link with
                    | YouTube identifier -> 
                        IFrame [Attr.Width "560"; Attr.Height "315"; Attr.Src ("https://www.youtube.com/embed/" + identifier); Attr.FrameBorder "0"]
                    | Imgur identifier ->
                        VerbatimContent (@"<blockquote class=""imgur-embed-pub"" lang=""en"" data-id=""" + identifier + @"""><a href=""//imgur.com/" + identifier + @""">View post on imgur.com</a></blockquote><script async src=""//s.imgur.com/min/embed.js"" charset=""utf-8""></script>")
                                        

    let ItemsList ctx (year:int) =
        match Map.tryFind year Server.itemsEachYear with
            | Some items ->
                let itemsTable = 
                    items
                    |> Map.toSeq
                    |> Seq.map (fun (key,item) -> 
                                    TR [TD [Text (item.ID.ToString())]
                                        TD [Text (item.Points.ToString())]
                                        TD [Text (item.Description)]
                                        TD [(match item.Link with
                                            | YouTube _ -> (A [Text "Watch on YouTube"] -< [Attr.HRef (Helpers.linkToURL item.Link); Attr.Class "btn btn-danger btn-block"; Attr.Target "_blank"])
                                            | Imgur _ -> (A [Text "View on Imgur"] -< [Attr.HRef (Helpers.linkToURL item.Link); Attr.Class "btn btn-success btn-block"; Attr.Target "_blank"]))]
                                        ])
                    |> (fun rows -> seq [ THead [TH [Text "Number"]; TH [Text "Points"]; TH [Text "Description"]; TH [Text "Link"]]; TBody rows])
                    |> (fun rows -> Table rows -< [Class "table table-striped"])

                Templating.Main ctx (EndPoint.ItemList year) (year.ToString() + " Items") [
                    H1 [Text (year.ToString() + " Items")];
                    P [Text ("The items for the " + year.ToString() + " hunt.")];
                    P [A [Text "View all items at once"] -< [Attr.HRef (ctx.Link (EndPoint.ItemViewAll year)); Attr.Class "btn btn-primary"]];
                    P [Text "(viewing all items at once may take a moment to load)"];
                    itemsTable
                    ]
            | None -> ErrorPage ctx "Team Captain didn't compete that year, but good effort."

    let ItemsViewAll ctx (year:int) =
        match Map.tryFind year Server.itemsEachYear with
            | Some items ->
                let itemPanels = 
                    items
                    |> Map.toSeq
                    |> Seq.map (fun (key,item) -> 
                            Div [
                                Div [ Div [H4 [Text (item.ID.ToString())]] -< [Attr.Class "panel-title"]] -< [Attr.Class "panel-heading"]
                                Div [
                                    Div [linkToEmbedded item.Link] -< [Attr.Class "col-md-8"]
                                    Div [
                                        P [Text item.Description]
                                        P [Text (item.Points.ToString() + " points")]
                                        ] -< [Attr.Class "col-md-4"]
                                    ] -< [Attr.Class "panel-body"]
                                ] -< [Attr.Class "panel panel-primary"]
                                )
                    |> List.ofSeq

                Templating.Main ctx (EndPoint.ItemList year) (year.ToString() + " Items") 
                    (List.append [
                        H1 [Text (year.ToString() + " Items")];
                        P [Text ("The items for the " + year.ToString() + " hunt.")];
                        P [A [Text "View items as list"] -< [Attr.HRef (ctx.Link (EndPoint.ItemList year)); Attr.Class "btn btn-primary"]];
                        ]
                        itemPanels)
            | None -> ErrorPage ctx "Team Captain didn't compete that year, but good effort."
        
    let ItemPage ctx (year:int) (itemID:int) =
        match Map.tryFind year Server.itemsEachYear with
            | None -> ErrorPage ctx "Team Captain didn't compete that year, but good effort."
            | Some items -> 
                match Map.tryFind itemID items with
                    | None -> ErrorPage ctx "We aren't sharing this item with you."
                    | Some item ->
                        let itemName = "Item " + itemID.ToString() + " (" + year.ToString() + ")"

                        Templating.Main ctx (EndPoint.ItemList year) itemName ([   H1 [Text itemName];
                                                                                H2 [Text ("Points: " + item.Points.ToString())];
                                                                                P [Text item.Description];
                                                                                linkToEmbedded item.Link;
                                                                                ])


                


    [<Website>]
    let Main =
        Application.MultiPage (fun ctx endpoint ->
            match endpoint with
            | EndPoint.Home -> HomePage ctx
            | EndPoint.Crew -> CrewPage ctx
            | EndPoint.ItemList year -> ItemsList ctx year
            | EndPoint.ItemViewAll year -> ItemsViewAll ctx year
            | EndPoint.Item (year, number) -> ItemPage ctx year number
        )
