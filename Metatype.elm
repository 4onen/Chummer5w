module Metatype exposing (..)

import Attributes exposing (AttrObj,attrObj)
import Magicality exposing (Magicality(..))

type Metatype 
    = Dwarf (Maybe DwarfMetavariant)
    | Elf (Maybe ElfMetavariant)
    | Human (Maybe HumanMetavariant)
    | Ork (Maybe OrkMetavariant)
    | Troll (Maybe TrollMetavariant)
    | Metasapient Never

metatypes : List Metatype
metatypes = 
    [ Dwarf Nothing, Elf Nothing
    , Human Nothing, Ork Nothing
    , Troll Nothing
    ]

getAllowed : Int -> List Metatype
getAllowed priorityIdx =
    let
        allOfEm = metatypes
        noTroll = 
            [ Dwarf Nothing, Elf Nothing
            , Human Nothing, Ork Nothing
            ]
        humanOnly = [Human Nothing]
        humanOrElf = (Elf Nothing)::humanOnly
    in
        case priorityIdx of
            0 -> allOfEm
            1 -> allOfEm
            2 -> noTroll
            3 -> humanOrElf
            _ -> humanOnly

getVariants : Metatype -> List Metatype
getVariants metatype =
    case metatype of
        Dwarf _ ->
            List.map (Maybe.Just>>Dwarf) dwarfMetavariants
        Elf _ ->
            List.map (Maybe.Just>>Elf) elfMetavariants
        Human _ ->
            List.map (Maybe.Just>>Human) humanMetavariants
        Ork _ ->
            List.map (Maybe.Just>>Ork) orkMetavariants
        Troll _ ->
            List.map (Maybe.Just>>Troll) trollMetavariants
        _ ->
            []

type DwarfMetavariant
    = Gnome
    | Harumen
    | Koborokuru
    | Menehune
    | Querx

dwarfMetavariants : List DwarfMetavariant
dwarfMetavariants =
    [ Gnome, Harumen
    , Koborokuru
    , Menehune, Querx
    ]

type ElfMetavariant
    = Dalakiton
    | Dryad
    | NightOne
    | Wakyambi
    | XapiriThepe

elfMetavariants : List ElfMetavariant
elfMetavariants =
    [ Dalakiton, Dryad
    , NightOne, Wakyambi
    , XapiriThepe
    ]

type HumanMetavariant 
    = Nartaki

humanMetavariants : List HumanMetavariant
humanMetavariants = [Nartaki]

type OrkMetavariant
    = Hobgoblin
    | Ogre
    | Oni
    | Satyr

orkMetavariants : List OrkMetavariant
orkMetavariants =
    [Hobgoblin, Ogre, Oni, Satyr]


type TrollMetavariant
    = Cyclops
    | Fomori
    | Giants
    | Minotaurs

trollMetavariants : List TrollMetavariant
trollMetavariants =
    [Cyclops,Fomori,Giants,Minotaurs]

toString : Metatype -> String
toString metatype =
    if List.member metatype metatypes then
        metatype
            |> Basics.toString
            |> String.words
            |> List.filter ((/=) "Nothing")
            |> String.join " "
    else
        case metatype of
            Elf (Just NightOne) ->
                "Night One"
            Elf (Just XapiriThepe) ->
                "Xapiri Thepe"
            _ ->
                metatype
                    |> Basics.toString
                    |> String.filter (\c -> c/='(' && c/=')')
                    |> String.words 
                    |> List.drop 1
                    |> List.filter (\w -> w/="Nothing" && w/="Just") 
                    |> String.join " "

getBaseType : Metatype -> Metatype
getBaseType m =
    case m of 
        Dwarf _ ->
            Dwarf Nothing
        Elf _ ->
            Elf Nothing
        Human _ ->
            Human Nothing
        Ork _ ->
            Ork Nothing
        Troll _ ->
            Troll Nothing
        _ ->
            Human Nothing

getBaseStats : Metatype -> (Magicality,Int) -> AttrObj
getBaseStats metatype (magicality,magPriority) =
    let
        mag = Magicality.getBaseMagic magicality magPriority
        res = Magicality.getBaseResonance magicality magPriority
    in
        case metatype of
            Dwarf Nothing ->
                attrObj 3 1 1 3 2 1 1 1 1 mag res
            Elf Nothing ->
                attrObj 1 2 1 1 1 1 1 3 1 mag res
            Human Nothing ->
                attrObj 1 1 1 1 1 1 1 1 2 mag res
            Human (Just Nartaki) ->
                attrObj 1 1 1 1 1 1 1 1 1 mag res
            Ork Nothing ->
                attrObj 4 1 1 3 1 1 1 1 1 mag res
            Troll Nothing ->
                attrObj 5 1 1 5 1 1 1 1 1 mag res
            _ ->
                getBaseStats (Human Nothing) (magicality,magPriority)

getMaxStats : Metatype -> (Magicality,Int) -> AttrObj
getMaxStats metatype (magicality,magPriority) =
    let
        (mag,res)=
            case magicality of 
                Mundane -> (0,0)
                Technomancer -> (0,6)
                _ -> (6,0)
    in
        case metatype of
            Dwarf Nothing ->
                attrObj 8 6 5 8 7 6 6 6 6 mag res
            Elf Nothing ->
                attrObj 6 7 6 6 6 6 6 8 6 mag res
            Human Nothing ->
                attrObj 6 6 6 6 6 6 6 6 7 mag res
            Human (Just Nartaki) ->
                attrObj 6 6 6 6 6 6 6 6 6 mag res
            Ork Nothing ->
                attrObj 9 6 6 8 6 5 6 5 6 mag res
            Troll Nothing ->
                attrObj 10 5 6 10 6 5 5 4 6 mag res
            _ ->
                getMaxStats (Human Nothing) (magicality,magPriority)

getSpecialPoints : Int -> Metatype -> Int
getSpecialPoints metaPriorityIdx metatype =
    case (metaPriorityIdx, metatype) of
        (0,Dwarf Nothing) ->
            7
        (0,Elf Nothing) ->
            8
        (0,Human Nothing) ->
            9
        (0,Human (Just Nartaki)) ->
            8
        (0,Ork Nothing) ->
            7
        (0,Troll Nothing) ->
            5
        
        (1,Dwarf Nothing) ->
            4
        (1,Elf Nothing) ->
            7
        (1,Human Nothing) ->
            6
        (1,Human (Just Nartaki)) ->
            6
        (1,Ork Nothing) ->
            4
        (1,Troll Nothing) ->
            0
        
        (2,Dwarf Nothing) ->
            1
        (2,Elf Nothing) ->
            3
        (2,Human Nothing) ->
            5
        (2,Human (Just Nartaki)) ->
            4
        (2,Ork Nothing) ->
            0
        
        (3,Elf Nothing) ->
            0
        (3,Human Nothing) ->
            3
        (3,Human (Just Nartaki)) ->
            2
        
        (_,Human Nothing) ->
            1
        (_,Human (Just Nartaki)) ->
            0
        _ ->
            0
