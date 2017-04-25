module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Math.Vector4 as Vec4 exposing (vec4, Vec4)
import Ports
import Result
import String
import Task
import Types exposing (..)
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
    }
        ! [ Task.perform Resize Window.size ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes Resize


view : Model -> Html Msg
view model =
    let
        input123 =
            vec3 (f 1) (f 2) (f 3) |> Vec3.scale 500

        input456 =
            vec3 (f 4) (f 5) (f 6) |> Vec3.scale 500

        location =
            vec3 130 80 340

        center =
            vec3 110 10 -210

        light =
            Vec3.normalize (vec3 0.34 0.405 0.484)

        f x =
            Dict.get x model.input |> Maybe.withDefault 0

        wallsEntity =
            WebGL.entity
                checkerVertexShader
                checkerFragmentShader
                walls
                (Uniforms
                    (Mat4.makePerspective 45 (16 / 9) 0.01 1000)
                    (Mat4.makeLookAt location center (vec3 0 1 0))
                    light
                )

        webgl =
            WebGL.toHtml
                [ Attr.width model.windowSize.width
                , Attr.height (model.windowSize.width * 9 // 16)
                ]
                [ wallsEntity
                ]

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


walls : Mesh Attributes
walls =
    let
        colorxy =
            vec4 0.01 0.6 1 1

        colorxz =
            colorxy

        coloryz =
            colorxy

        s =
            500

        attributes =
            [ ( Attributes (vec3 0 0 0) (vec3 0 0 1) colorxy
              , Attributes (vec3 s 0 0) (vec3 0 0 1) colorxy
              , Attributes (vec3 s s 0) (vec3 0 0 1) colorxy
              )
            , ( Attributes (vec3 0 0 0) (vec3 0 0 1) colorxy
              , Attributes (vec3 s s 0) (vec3 0 0 1) colorxy
              , Attributes (vec3 0 s 0) (vec3 0 0 1) colorxy
              )
            , ( Attributes (vec3 0 0 0) (vec3 0 1 0) colorxz
              , Attributes (vec3 s 0 0) (vec3 0 1 0) colorxz
              , Attributes (vec3 s 0 s) (vec3 0 1 0) colorxz
              )
            , ( Attributes (vec3 0 0 0) (vec3 0 1 0) colorxz
              , Attributes (vec3 s 0 s) (vec3 0 1 0) colorxz
              , Attributes (vec3 0 0 s) (vec3 0 1 0) colorxz
              )
            , ( Attributes (vec3 0 0 0) (vec3 1 0 0) coloryz
              , Attributes (vec3 0 s 0) (vec3 1 0 0) coloryz
              , Attributes (vec3 0 s s) (vec3 1 0 0) coloryz
              )
            , ( Attributes (vec3 0 0 0) (vec3 1 0 0) coloryz
              , Attributes (vec3 0 s s) (vec3 1 0 0) coloryz
              , Attributes (vec3 0 0 s) (vec3 1 0 0) coloryz
              )
            ]
    in
        WebGL.triangles attributes


vertexShader : Shader Attributes Uniforms { vnormal : Vec3, vcolor : Vec4 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        attribute vec4 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        varying vec3 vnormal;
        varying vec4 vcolor;
        void main () {
            gl_Position = perspective * camera * vec4(position, 1.0);
            vnormal = normal;
            vcolor = color;
        }
    |]


fragmentShader : Shader {} Uniforms { vnormal : Vec3, vcolor : Vec4 }
fragmentShader =
    [glsl|
        precision mediump float;
        uniform vec3 light;
        varying vec3 vnormal;
        varying vec4 vcolor;
        void main () {
            gl_FragColor = vcolor;
            gl_FragColor.rgb *= dot(vnormal, normalize(light));
        }
    |]


checkerVertexShader : Shader Attributes Uniforms { vposition : Vec3, vnormal : Vec3, vcolor : Vec4 }
checkerVertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        attribute vec4 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        varying vec3 vposition;
        varying vec3 vnormal;
        varying vec4 vcolor;
        void main () {
            gl_Position = perspective * camera * vec4(position, 1.0);
            vposition = position;
            vnormal = normal;
            vcolor = color;
        }
    |]


checkerFragmentShader : Shader {} Uniforms { vposition : Vec3, vnormal : Vec3, vcolor : Vec4 }
checkerFragmentShader =
    [glsl|
        precision mediump float;
        uniform vec3 light;
        varying vec3 vposition;
        varying vec3 vnormal;
        varying vec4 vcolor;
        void main () {
            gl_FragColor = vcolor;
            gl_FragColor.rgb *= dot(vnormal, normalize(light));
            if (vnormal.y == 1.0 && mod(floor(vposition.x / 10.0), 2.0) != mod(floor(vposition.z / 10.0), 2.0)) {
                gl_FragColor.rgb *= 0.8;
            }
        }
    |]
