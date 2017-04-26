module Types exposing (..)

import Dict exposing (Dict)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3)
import Math.Vector4 as Vec4 exposing (Vec4)
import Random
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


type alias Fish =
    { tMU : Float
    , uMin : Float
    , uIPW : Float
    , uSPR : Float
    , uMax : Float
    , pa : Float
    , dGap : Float
    , uQ : UQ
    , pt : Vec3
    , qt : Vec3
    , sM : SM
    , sB : SB
    , tr : Float
    , spine : Vec4
    , pectoral : Vec2
    , tube : List Vec3
    }


type alias Model =
    { windowSize : Window.Size
    , input : Dict Int Float
    , fish : List Fish
    , seed : Random.Seed
    }


type Msg
    = Notify String
    | Resize Window.Size
    | Diffs Time
    | Input Int String
