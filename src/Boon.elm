module Boon exposing (..)

import Ability exposing (Ability)
import Character exposing (Domain(..), Resistance(..), Skill(..), Stats)
import TypedDict


type Boon
    = ResistanceUp Resistance Int
    | NewDomain Domain
    | NewSkill Skill
    | NewAbility String
    | NewEquipment String
    | NewRefresh String


apply : Boon -> Stats -> Stats
apply boon character =
    case boon of
        ResistanceUp resistance bonus ->
            { character | resistances = TypedDict.update resistance ((+) bonus >> clamp 0 5) character.resistances }

        NewDomain domain ->
            { character | domains = TypedDict.set domain True character.domains }

        NewSkill skill ->
            { character | skills = TypedDict.set skill True character.skills }

        NewAbility ability ->
            { character
                | abilities =
                    character.abilities
                        ++ (if character.abilities == "" then
                                ""

                            else
                                "\n\n"
                           )
                        ++ ability
            }

        NewEquipment equipment ->
            { character | equipment = character.equipment ++ "\n\n" ++ equipment }

        NewRefresh refresh ->
            { character | refresh = character.refresh ++ "\n\n" ++ refresh }


applyAll : List Boon -> Stats -> Stats
applyAll boons character =
    List.foldl apply character boons


type alias Class =
    List Boon


doc : Class
doc =
    [ ResistanceUp Resolve 2
    , ResistanceUp Reputation 2
    , NewRefresh "Help those who cannot help themselves."
    , NewSkill Patch
    , NewSkill Compel
    , NewDomain Science
    , NewDomain LowSociety
    , NewEquipment "The common red medic outfit bearing the official white medic seal of the Hegemony."
    , NewEquipment "A less than ideally stocked medkit."
    , NewAbility "Medical Attention. You take some time to check on your allies and stitch them up if need be. Once per session, help your allies heal, physically and mentally. All allies present may restore 3 stress from Body or Resolve."
    , NewAbility "Bedside Manner. People are grateful for your help. When you heal someone, gain a bond with them until the end of the next day."
    ]
