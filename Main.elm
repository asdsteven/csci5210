module Main exposing (..)

import AnimationFrame
import Dict exposing (Dict)
import Fish
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Ports
import Random
import Result
import String
import Task
import Time exposing (Time)
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
    , seed = Random.initialSeed 23475
    }
        ! [ Task.perform Resize Window.size ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        , AnimationFrame.diffs Diffs
        ]


view : Model -> Html Msg
view model =
    let
        f x =
            Dict.get x model.input |> Maybe.withDefault 0

        input123 =
            vec3 (f 1) (f 2) (f 3) |> Vec3.scale 1

        input456 =
            vec3 (f 4) (f 5) (f 6)

        perspective =
            Mat4.makePerspective 45 (16 / 9) 0.01 1000

        location =
            Vec3.add (Vec3.scale 100 input456) (vec3 130 80 340)

        center =
            vec3 110 10 -210

        camera =
            Mat4.makeLookAt location center (vec3 0 1 0)

        perspectiveCamera =
            Mat4.mul perspective camera

        light =
            Vec3.normalize (Vec3.add input123 (vec3 0.34 0.405 0.484))

        wallsEntity =
            Walls.entity
                (Walls.Uniforms
                    perspectiveCamera
                    light
                )

        fisherize f =
            let
                translate =
                    Mat4.makeTranslate f.pt

                v1 =
                    vec3 0 (Vec3.dot Vec3.j f.qt) 0
                        |> Vec3.sub f.qt
                        |> Vec3.normalize

                rotate1 =
                    Mat4.rotate (acos (Vec3.dot v1 (vec3 -1 0 0))) (Vec3.cross (vec3 -1 0 0) v1) translate

                v2 =
                    Mat4.makeRotate (negate (acos (Vec3.dot v1 (vec3 -1 0 0)))) (Vec3.cross (vec3 -1 0 0) v1)
                        |> flip Mat4.transform f.qt
                        |> Vec3.normalize

                rotate2 =
                    Mat4.rotate (acos (Vec3.dot v2 (vec3 -1 0 0))) (Vec3.cross (vec3 -1 0 0) v2) rotate1
            in
                Fish.entity
                    (Fish.Uniforms
                        (Mat4.mul perspectiveCamera rotate2)
                        rotate2
                        f.spine
                        f.pectoral
                        light
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
            []
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

        Diffs dt ->
            let
                f fish ( seed, fishes ) =
                    let
                        ( seed_, fish_ ) =
                            Fish.update (Time.inSeconds dt) seed fish
                    in
                        ( seed_, fish_ :: fishes )

                ( seed, fish ) =
                    List.foldl f ( model.seed, [] ) model.fish
            in
                { model
                    | fish = fish
                    , seed = seed
                }
                    ! []
