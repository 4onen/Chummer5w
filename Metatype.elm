module Metatype exposing (..)

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



getBaseStats : Metatype -> (AttributeObject, AttributeObject)
getBaseStats metatype =
    case metatype of
        Dwarf Nothing ->
            ( AttributeObject 3 1 1 3 2 1 1 1 1 1 1
            , AttributeObject 8 6 5 8 7 6 6 6 6 6 6
            )
        Elf Nothing ->
            ( AttributeObject 1 2 1 1 1 1 1 3 1 1 1
            , AttributeObject 6 7 6 6 6 6 6 8 6 6 6
            )
        Human Nothing ->
            ( AttributeObject 1 1 1 1 1 1 1 1 2 1 1
            , AttributeObject 6 6 6 6 6 6 6 6 7 6 6
            )
        Human (Just Nartaki) ->
            ( AttributeObject 1 1 1 1 1 1 1 1 1 1 1
            , AttributeObject 6 6 6 6 6 6 6 6 6 6 6
            )
        Ork Nothing ->
            ( AttributeObject 4 1 1 3 1 1 1 1 1 1 1
            , AttributeObject 9 6 6 8 6 5 6 5 6 6 6
            )
        Troll Nothing ->
            ( AttributeObject 5 1 1 5 1 1 1 1 1 1 1
            , AttributeObject 10 5 6 10 6 5 5 4 6 6 6
            )
        _ ->
            getBaseStats (Human Nothing)
