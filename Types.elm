module Types exposing (..)

import Dict exposing (Dict)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3)
import Math.Vector4 as Vec4 exposing (Vec4)
import Time exposing (Time)
import Window


type UQ
    = Rest
    | Slow
    | Fast


type SM
    = Active
    | Inactive


type SB
    = Escape
    | Avoid
    | Free


type alias Species =
    { tMU : Time
    , uMin : Float
    , uIPW : Float
    , uSPR : Float
    , uMax : Float
    , pa : Float
    , dGap : Float
    }


type alias Fish =
    { species : Species
    , uQ : UQ
    , pt : Vec3
    , pt1 : Vec3
    , sM : SM
    , sB : SB
    , tr : Time
    , spine : Vec4
    , pectoral : Vec2
    }


type alias Model =
    { windowSize : Window.Size
    , input : Dict Int Float
    , fish : List Fish
    }


type Msg
    = Notify String
    | Resize Window.Size
    | Input Int String
