module SkillGroups exposing (..)

import Magicality exposing (Magicality)
import Skills exposing (Skill(..))

acting : List Skill
acting = 
    [ Con
    , Impersonation
    , Performance
    ]

athletics : List Skill
athletics = 
    [ Gymnastics
    , Running
    , Swimming
    ]

biotech : List Skill
biotech = 
    [ Biotechnology
    , Cybertechnology
    , FirstAid
    , Medicine
    ]

closeCombat : List Skill
closeCombat = 
    [ Blades
    , Clubs
    , UnarmedCombat
    ]

conjuring : List Skill
conjuring = 
    [ Banishing
    , Binding
    , Summoning
    ]

cracking : List Skill
cracking =
    [ Cybercombat
    , ElectronicWarfare
    , Hacking
    ]

electronics : List Skill
electronics =
    [ Computer
    , Hardware
    , Software
    ]

enchanting : List Skill
enchanting =
    [ Alchemy
    , Artificing
    , Disenchanting
    ]

engineering : List Skill
engineering = 
    [ AeronauticMechanic
    , AutomotiveMechanic
    , IndustrialMechanic
    , NauticalMechanic
    ]

firearms : List Skill
firearms =
    [ Automatics
    , Longarms
    , Pistols
    ]

influence : List Skill
influence =
    [ Etiquette
    , Leadership
    , Negotiation
    ]

outdoors : List Skill
outdoors =
    [ Navigation
    , Survival
    , Tracking
    ]

sorcery : List Skill
sorcery =
    [ Counterspelling
    , RitualSpellcasting
    , Spellcasting
    ]

stealth : List Skill
stealth =
    [ Disguise
    , Palming
    , Sneaking
    ]

tasking : List Skill
tasking =
    [ Compiling
    , Decompiling
    , Registering
    ]

getGroup : Skill -> Maybe String
getGroup skill =
         if List.member skill acting then
        Just "Acting"
    else if List.member skill athletics then
        Just "Athletics"
    else if List.member skill biotech then
        Just "Biotech"
    else if List.member skill closeCombat then
        Just "Close Combat"
    else if List.member skill conjuring then
        Just "Conjuring"
    else if List.member skill cracking then
        Just "Cracking"
    else if List.member skill electronics then
        Just "Electronics"
    else if List.member skill enchanting then
        Just "Enchanting"
    else if List.member skill engineering then
        Just "Engineering"
    else if List.member skill firearms then
        Just "Firearms"
    else if List.member skill influence then
        Just "Influence"
    else if List.member skill outdoors then
        Just "Outdoors"
    else if List.member skill sorcery then
        Just "Sorcery"
    else if List.member skill stealth then
        Just "Stealth"
    else if List.member skill tasking then
        Just "Tasking"
    else 
        Nothing

getGroupSkillList : String -> List Skill
getGroupSkillList group =
    case group of
        "Acting" -> 
            acting
        "Athletics" -> 
            athletics
        "Biotech" -> 
            biotech
        "Close Combat" ->
            closeCombat
        "Conjuring" -> 
            conjuring
        "Cracking" -> 
            cracking
        "Electronics" -> 
            electronics
        "Enchanting" -> 
            enchanting
        "Engineering" -> 
            engineering
        "Firearms" -> 
            firearms
        "Influence" -> 
            influence
        "Outdoors" -> 
            outdoors
        "Sorcery" -> 
            sorcery
        "Stealth" -> 
            stealth
        "Tasking" -> 
            tasking
        _ -> 
            []

getCompleteGroupList : List String
getCompleteGroupList =
    [ "Acting"
    , "Athletics"
    , "Biotech"
    , "Close Combat"
    , "Conjuring"
    , "Cracking"
    , "Electronics"
    , "Enchanting"
    , "Engineering"
    , "Firearms"
    , "Influence"
    , "Outdoors"
    , "Sorcery"
    , "Stealth"
    , "Tasking"
    ]
    

getGroupList : Magicality -> List String
getGroupList magicality =
    [ "Acting"
    , "Athletics"
    , "Biotech"
    , "Close Combat"
    , "Conjuring"
    , "Cracking"
    , "Electronics"
    , "Engineering"
    , "Firearms"
    , "Influence"
    , "Outdoors"
    , "Stealth"
    ] ++
    if magicality==Magicality.Mundane then
        []
    else if magicality==Magicality.Technomancer then
        ["Tasking"]
    else if magicality==Magicality.Adept then
        []
    else
        ["Enchanting","Sorcery"]
