namespace SharedCore
type Link = | YouTube of string | Imgur of string
type Item = {ID: int; Description: string; Points: int; Link: Link}
type Crew = {Name: string; Description: string option; ImageLink: string option}
module Helpers =
    let linkToURL (link:Link) =
        match link with
        | YouTube s -> "https://youtu.be/" + s
        | Imgur s -> "http://i.imgur.com/" + s + ".jpg"

