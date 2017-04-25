module Types exposing (..)

import Dict exposing (Dict)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3)
import Math.Vector4 as Vec4 exposing (Vec4)
import Window


type alias Attributes =
    { position : Vec3
    , normal : Vec3
    , color : Vec4
    }


type alias Uniforms =
    { perspective : Mat4
    , camera : Mat4
    , light : Vec3
    }


type alias Model =
    { windowSize : Window.Size
    , input : Dict Int Float
    }


type Msg
    = Notify String
    | Resize Window.Size
    | Input Int String
