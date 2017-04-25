module Walls exposing (..)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Math.Vector4 as Vec4 exposing (vec4, Vec4)
import WebGL


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


type alias Varyings =
    { vposition : Vec3
    , vnormal : Vec3
    , vcolor : Vec4
    }


entity : Uniforms -> WebGL.Entity
entity uniforms =
    WebGL.entity vertexShader fragmentShader mesh uniforms


mesh : WebGL.Mesh Attributes
mesh =
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


vertexShader : WebGL.Shader Attributes Uniforms Varyings
vertexShader =
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


fragmentShader : WebGL.Shader {} Uniforms Varyings
fragmentShader =
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
