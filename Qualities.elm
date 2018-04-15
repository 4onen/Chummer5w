module Qualities exposing (..)

type alias URL = String

getURL : String -> Bool -> URL
getURL str positive =
    if positive then
        getPositiveURL str
    else
        getNegativeURL str

getPositiveURL : String -> URL
getPositiveURL str =
    str
        |> String.map (\c -> if c/=' ' then c else '_')
        |> (flip String.append) "http://adragon202.no-ip.org/Shadowrun/index.php/SR5:Positive_Qualities#"

getNegativeURL : String -> URL
getNegativeURL str =
    if String.startsWith "SINner" str then
        "http://adragon202.no-ip.org/Shadowrun/index.php/SR5:Negative_Qualities#Sinner_.28layered.29"
    else
        str 
            |> String.map (\c -> if c/=' ' then c else '_')
            |> (flip String.append) "http://adragon202.no-ip.org/Shadowrun/index.php/SR5:Negative_Qualities#"

type QualityTarget
    = Attribute String
    | Skill String
    | SkillGroup String

