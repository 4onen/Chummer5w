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

--(Name, Max Rating, Karma per rating level)
positive : List (String, Int, Int)
positive = 
    [ ("Ambidextrous",1,4)
    , ("Analytical Mind",1,5)
    , ("Aptitude",1,14)
    , ("Astral Chameleon",1,10)
    , ("Bilingual",1,5)
    , ("Blandness",1,8)
    , ("Catlike",1,7)
    , ("Codeslinger",1,10)
    , ("Double-jointed",1,6)
    , ("Exceptional Attribute",1,14)
    , ("First Impression",1,11)
    , ("Focused Concentration",6,4)
    , ("Gearhead",1,11)
    , ("Guts",1,10)
    , ("High Pain Tolerance",3,7)
    , ("Home Ground",1,10)
    , ("Human-looking",1,6)
    , ("Indomitable",3,8)
    , ("Juryrigger",1,10)
    , ("Lucky",1,12)
    , ("Magic Resistance",4,6)
    , ("Mentor Spirit",1,5)
    , ("Natural Athlete",1,7)
    , ("Natural Hardening",1,10)
    , ("Natural Immunity",2,4)--Special case: 4 OR 10 Karma
    , ("Photographic Memory",1,6)
    , ("Quick Healer",1,3)
    , ("Resistance to Pathogens/Toxins",2,4)
    , ("Spirit Affinity",1,7)
    , ("Toughness",1,9)
    , ("Will To Live",3,3)
    ]

negative : List (String, Int, Int)
negative =
    [ ("Addiction",4,4)--Special case: 4,9,20,25 karma
    , ("Allergy",8,2)--Special case: 5,10,10,15,15,20,20,25 karma
    , ("Astral Beacon",1,10)
    , ("Bad Luck",1,12)
    , ("Bad Rep",1,7)
    , ("Code of Honor",1,15)
    , ("Codeblock",1,10)
    , ("Combat Paralysis",1,12)
    , ("Dependents",3,3)
    , ("Distinctive Style",1,5)
    , ("Elf Poser",1,6)
    , ("Gremlins",4,4)
    , ("Incompetent",1,5)
    , ("Insomnia I",1,10)
    , ("Insomnia II",1,15)
    , ("Loss of Confidence",1,10)
    , ("Low Pain Tolerance",1,9)
    , ("Ork Poser",1,6)
    , ("Prejudiced",6,0)--Special case: 3,5,5,7,8,10 karma
    , ("Scorched",1,10)
    , ("Sensitive System",1,12)
    , ("Simsense Vertigo",1,5)
    , ("SINner (National)",1,5)
    , ("SINner (Criminal)",1,10)
    , ("SINner (Corporate Limited)",1,15)
    , ("SINner (Corporate)",1,25)
    , ("Social Stress",1,8)
    , ("Spirit Bane",1,7)
    , ("Uncouth",1,14)
    , ("Uneducated",1,8)
    , ("Unsteady Hands",1,7)
    , ("Weak Immune System",1,10)
    ]