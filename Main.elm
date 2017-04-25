module Main exposing (..)

import Dict exposing (Dict)
import Fish
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Ports
import Result
import String
import Task
import Types exposing (..)
import Walls
import WebGL exposing (Mesh, Shader)
import Window


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }


init : ( Model, Cmd Msg )
init =
    { windowSize = Window.Size 100 100
    , input = Dict.empty
    , fish = [ Fish.fish1 ]
    }
        ! [ Task.perform Resize Window.size ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes Resize


view : Model -> Html Msg
view model =
    let
        f x =
            Dict.get x model.input |> Maybe.withDefault 0

        input123 =
            vec3 (f 1) (f 2) (f 3) |> Vec3.scale 500

        input456 =
            vec3 (f 4) (f 5) (f 6) |> Vec3.scale 500

        perspective =
            Mat4.makePerspective 45 (16 / 9) 0.01 1000

        location =
            vec3 130 80 340

        center =
            vec3 110 10 -210

        camera =
            Mat4.makeLookAt location center (vec3 0 1 0)

        light =
            Vec3.normalize (vec3 0.34 0.405 0.484)

        wallsEntity =
            Walls.entity
                (Walls.Uniforms
                    perspective
                    camera
                    light
                )

        fisherize f =
            Fish.entity
                (Fish.Uniforms
                    perspective
                    camera
                    light
                    f.pt
                    f.pt1
                    f.spine
                    f.pectoral
                )

        fishEntities =
            List.map fisherize model.fish

        webgl =
            WebGL.toHtml
                [ Attr.width model.windowSize.width
                , Attr.height (model.windowSize.width * 9 // 16)
                ]
                (wallsEntity :: fishEntities)

        sliderize i =
            Html.div []
                [ Html.input
                    [ Attr.type_ "range"
                    , Attr.min "-1"
                    , Attr.max "1"
                    , Attr.step "any"
                    , Attr.defaultValue "0"
                    , Events.onInput (Input i)
                    ]
                    []
                , Html.input
                    [ Attr.type_ "text"
                    , Attr.value (Dict.get i model.input |> Maybe.withDefault 0 |> toString)
                    ]
                    []
                ]

        sliders =
            List.map sliderize [ 1, 2, 3, 4, 5, 6 ]
    in
        Html.div
            [ Attr.class "" ]
            [ Html.div
                []
                [ webgl ]
            , Html.div
                []
                sliders
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Notify s ->
            model ! [ Ports.notify s ]

        Resize s ->
            { model
                | windowSize = s
            }
                ! []

        Input i s ->
            { model
                | input = Dict.insert i (String.toFloat s |> Result.withDefault 0) model.input
            }
                ! []
