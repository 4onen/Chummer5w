module Metatype exposing (..)

import Priorities exposing (PriorityChar(..))
import Attributes exposing (AttributeObject)

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



getBaseStats : Metatype -> PriorityChar -> AttributeObject
getBaseStats metatype magicPriority =
    let
        magres = 
            case magicPriority of
                APriority ->5
                BPriority ->4
                CPriority ->3
                DPriority ->2
                EPriority ->0
    in
        case metatype of
            Dwarf Nothing ->
                AttributeObject 3 1 1 3 2 1 1 1 1 magres
            Elf Nothing ->
                AttributeObject 1 2 1 1 1 1 1 3 1 magres
            Human Nothing ->
                AttributeObject 1 1 1 1 1 1 1 1 2 magres
            Human (Just Nartaki) ->
                AttributeObject 1 1 1 1 1 1 1 1 1 magres
            Ork Nothing ->
                AttributeObject 4 1 1 3 1 1 1 1 1 magres
            Troll Nothing ->
                AttributeObject 5  1 1 5  1 1 1 1 1 magres
            _ ->
                getBaseStats (Human Nothing) magicPriority

getMaxStats : Metatype -> PriorityChar -> AttributeObject
getMaxStats metatype magicPriority =
    case metatype of
        Dwarf Nothing ->
            AttributeObject 8 6 5 8 7 6 6 6 6 6
        Elf Nothing ->
            AttributeObject 6 7 6 6 6 6 6 8 6 6
        Human Nothing ->
            AttributeObject 6 6 6 6 6 6 6 6 7 6
        Human (Just Nartaki) ->
            AttributeObject 6 6 6 6 6 6 6 6 6 6
        Ork Nothing ->
            AttributeObject 9 6 6 8 6 5 6 5 6 6
        Troll Nothing ->
            AttributeObject 10 5 6 10 6 5 5 4 6 6
        _ ->
            getMaxStats (Human Nothing) magicPriority