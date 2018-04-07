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

type DwarfMetavariant
    = Gnomes
    | Harumen
    | Koborokuru
    | Menehune
    | Querx

type ElfMetavariant
    = Dalakiton
    | Dryad
    | NightOne
    | Wakyambi
    | XapiriThepe

type HumanMetavariant 
    = Nartaki

type OrkMetavariant
    = Hobgoblins
    | Ogres
    | Oni
    | Satyr

type TrollMetavariant
    = Cyclops
    | Fomori
    | Giants
    | Minotaurs

getBaseStats : Metatype -> (Magicality,Int) -> AttrObj
getBaseStats metatype (magicality,rating) =
    let
        (mag,res) = 
            if rating < 1 then
                (0,0)
            else
                case magicality of
                    Magician -> 
                        (rating,0)
                    MysticAdept -> 
                        (rating,0) 
                    Technomancer -> 
                        (0,rating) 
                    Adept -> 
                        (rating,0) 
                    AspectedMagician -> 
                        (rating,0) 
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
                attrObj 5  1 1 5  1 1 1 1 1 mag res
            _ ->
                getBaseStats (Human Nothing) (magicality,rating)

getMaxStats : Metatype -> (Magicality,Int) -> AttrObj
getMaxStats metatype (magicality,rating) =
    let
        (mag,res) = 
            if rating < 1 then
                (0,0)
            else
                case magicality of
                    Magician ->
                        (6,0)
                    MysticAdept ->
                        (6,0)
                    Technomancer ->
                        (0,6)
                    Adept ->
                        (6,0)
                    AspectedMagician ->
                        (6,0)
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
                getMaxStats (Human Nothing) (magicality,rating)