module C5WChar exposing (..)

import C5WCharPersonal
import C5WCharAttributes

type alias Model =
    { personal: C5WCharPersonal.Model
    , attributes : C5WCharAttributes.Model
    }

