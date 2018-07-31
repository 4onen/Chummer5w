module C5WCharPersonal exposing (..)

type Sex 
    = Male
    | Female

type alias Model =
    { name: String
    , metatype: String
    , ethnicity: String
    , age: Result String Int
    , sex: Result String Sex
    , height: String
    , weight: String
    }

init : Model
init = Model
    ""
    ""
    ""
    (Err "")
    (Err "")
    ""
    ""