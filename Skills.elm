module Skills exposing (..)

import Priorities exposing (Priorities)
import Attributes exposing (Attribute(..))

type alias Skill = String

body : List Skill
body = 
    [ "Diving"
    , "Freefall"
    ]

agility : List Skill
agility = 
    [ "Automatics"
    , "Archery"
    , "Clubs"
    , "Blades"
    , "EscapeArtist"
    , "Gunnery"
    , "HeavyWeapons"
    , "Gymnastics"
    , "Locksmith"
    , "Longarms"
    , "Pistols"
    , "Palming"
    , "Sneaking"
    , "ThrowingWeapon"
    , "UnarmedCombat"
    ]

reaction : List Skill
reaction =
    [ "PilotAerospace"
    , "PilotWalker"
    , "PilotGroundcraft"
    , "PilotWatercraft"
    ]

strength : List Skill
strength = 
    [ "Running"
    , "Swimming"
    ]

willpower : List Skill
willpower =
    [ "AstralCombat"
    , "Survival"
    ]

logic : List Skill
logic = 
    [ "Arcana"
    , "AutomotiveMechanic"
    , "Chemistry"
    , "Cybertechnology"
    , "Demolitions"
    , "FirstAid"
    , "Hacking"
    , "Medicine"
    , "Forgery"
    , "AeronauticMechanic"
    , "Armorer"
    , "Biotechnology"
    , "Computer"
    , "Cybercombat"
    , "ElectronicWarfare"
    , "IndustrialMechanic"
    , "Hardware"
    , "NauticalMechanic"
    , "Software"
    ]

intuition : List Skill
intuition =
    [ "Artisan"
    , "Disguise"
    , "Perception"
    , "Tracking"
    , "Assensing"
    , "Navigation"
    ]

charisma : List Skill
charisma =
    [ "Con"
    , "Instruction"
    , "Leadership"
    , "Performance"
    , "AnimalHandling"
    , "Etiquette"
    , "Intimidation"
    , "Negotiation"
    , "Impersonation"
    ]

magic : List Skill
magic =
    [ "Alchemy"
    , "Artificing"
    , "Binding"
    , "RitualSpellcasting"
    , "Summoning"
    , "Disenchanting"
    , "Banishing"
    , "Counterspelling"
    , "Spellcasting"
    , "Enchanting"
    ]

resonance : List Skill
resonance = 
    [ "Compiling"
    , "Registering"
    , "Decompiling"
    ]

all : List Skill
all = 
    body++
    agility++
    reaction++
    strength++
    willpower++
    logic++
    intuition++
    charisma++
    magic++
    resonance

getSkillAttribute : Skill -> Attribute
getSkillAttribute skill =
         if List.member skill body then
        BOD
    else if List.member skill agility then
        AGI
    else if List.member skill reaction then
        REA
    else if List.member skill strength then
        STR
    else if List.member skill willpower then
        WIL
    else if List.member skill logic then
        LOG
    else if List.member skill intuition then
        INT
    else if List.member skill charisma then
        CHA
    else if List.member skill magic then
        MAG
    else if List.member skill resonance then
        RES
    else --Must be exotic
        AGI --NOPE. Not writing this case right now.

getAttributeSkills : Attribute -> List Skill
getAttributeSkills attr = 
    case attr of
        BOD ->
            body
        AGI ->
            agility
        REA ->
            reaction
        STR ->
            strength
        WIL -> 
            willpower
        LOG ->
            logic
        INT ->
            intuition
        CHA ->
            charisma
        MAG ->
            magic
        RES ->
            resonance
        _ ->
            []


getSkillAndGroupPointCount : Priorities -> (Int,Int)
getSkillAndGroupPointCount ps =
    case Priorities.getPriorityIndex Priorities.Skills ps of
        0 -> (46,10)
        1 -> (36,5)
        2 -> (28,2)
        3 -> (22,0)
        _ -> (18,0)

canDefault : List Skill
canDefault =
    [ "Archery"
    , "Automatics"
    , "Blades"
    , "Clubs"
    , "HeavyWeapons"
    , "Longarms"
    , "Pistols"
    , "ThrowingWeapon"
    , "UnarmedCombat"
    , "Disguise"
    , "Diving"
    , "EscapeArtist"
    , "Freefall"
    , "Gymnastics"
    , "Perception"
    , "Running"
    , "Sneaking"
    , "Survival"
    , "Swimming"
    , "Tracking"
    , "Con"
    , "Etiquette"
    , "Impersonation"
    , "Instruction"
    , "Intimidation"
    , "Leadership"
    , "Negotiation"
    , "Performance"
    , "AnimalHandling"
    , "Armorer"
    , "Computer"
    , "Cybercombat"
    , "Demolitions"
    , "FirstAid"
    , "Forgery"
    , "Hacking"
    , "Navigation"
    , "Gunnery"
    , "PilotGroundcraft"
    , "PilotWatercraft"
    ]