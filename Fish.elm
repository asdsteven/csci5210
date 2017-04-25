module Fish exposing (..)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (vec2, Vec2)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Math.Vector4 as Vec4 exposing (vec4, Vec4)
import Types exposing (..)
import WebGL


type alias Attributes =
    { position : Vec3
    , normal : Vec3
    , color : Vec4
    , joint : Float
    }


type alias Uniforms =
    { perspective : Mat4
    , camera : Mat4
    , light : Vec3
    , pt : Vec3
    , pt1 : Vec3
    , spine : Vec4
    , pectoral : Vec2
    }


type alias Varyings =
    { vnormal : Vec3
    , vcolor : Vec4
    }


species1 : Species
species1 =
    { tMU = 0
    , uMin = 0
    , uIPW = 0
    , uSPR = 0
    , uMax = 0
    , pa = 0
    , dGap = 0
    }


fish1 : Fish
fish1 =
    { species = species1
    , uQ = Rest
    , pt = vec3 0 0 0
    , pt1 = vec3 0 0 0
    , sM = Inactive
    , sB = Free
    , tr = 0
    , spine = vec4 0 0 0 0
    , pectoral = vec2 0 0
    }


entity : Uniforms -> WebGL.Entity
entity uniforms =
    WebGL.entity vertexShader fragmentShader mesh uniforms


mesh : WebGL.Mesh Attributes
mesh =
    let
        color =
            vec4 0.98 0.79 0.29 1

        belly a b c d e f g h i j k =
            [ vec3 0 0 0
            , vec3 a 0 e
            , vec3 a h 0
            , vec3 b 0 f
            , vec3 b i 0
            , vec3 c 0 g
            , vec3 c j 0
            , vec3 d 0 0
            , vec3 d k 0
            ]

        bellies =
            belly 10 20 30 38 2.5 2 0.5 6.5 6 2 4

        joint =
            [ 1, 1, 1, 2, 2, 3, 3, 0, 0 ]

        strip z y x =
            case ( y, x ) of
                ( i :: j, a :: b :: c :: d ) ->
                    Vec3.cross (Vec3.sub a b) (Vec3.sub c b)
                        |> Vec3.normalize
                        |> Vec3.scale z
                        |> (\k ->
                                ( Attributes a k color i
                                , Attributes b k color i
                                , Attributes c k color i
                                )
                                    :: strip -z j (b :: c :: d)
                           )

                _ ->
                    []

        attributes = List.concat
            [ bellies
                |> List.map (Mat4.transform (Mat4.makeScale3 1 1 1))
                |> strip -1 joint
            , bellies
                |> List.map (Mat4.transform (Mat4.makeScale3 1 -1 1))
                |> strip 1 joint
            , bellies
                |> List.map (Mat4.transform (Mat4.makeScale3 1 1 -1))
                |> strip 1 joint
            , bellies
                |> List.map (Mat4.transform (Mat4.makeScale3 1 -1 -1))
                |> strip -1 joint
            ]

        {-
           attributes =
               [ ( Attributes (vec3 0 0 0) (vec3 0 0 -1) color 0
                 , Attributes (vec3 s 0 0) (vec3 0 0 -1) color 0
                 , Attributes (vec3 s s 0) (vec3 0 0 -1) color 0
                 )
               , ( Attributes (vec3 0 0 0) (vec3 0 0 -1) color 0
                 , Attributes (vec3 s s 0) (vec3 0 0 -1) color 0
                 , Attributes (vec3 0 s 0) (vec3 0 0 -1) color 0
                 )
               , ( Attributes (vec3 0 0 0) (vec3 0 -1 0) color 0
                 , Attributes (vec3 s 0 0) (vec3 0 -1 0) color 0
                 , Attributes (vec3 s 0 s) (vec3 0 -1 0) color 0
                 )
               , ( Attributes (vec3 0 0 0) (vec3 0 -1 0) color 0
                 , Attributes (vec3 s 0 s) (vec3 0 -1 0) color 0
                 , Attributes (vec3 0 0 s) (vec3 0 -1 0) color 0
                 )
               , ( Attributes (vec3 0 0 0) (vec3 -1 0 0) color 0
                 , Attributes (vec3 0 s 0) (vec3 -1 0 0) color 0
                 , Attributes (vec3 0 s s) (vec3 -1 0 0) color 0
                 )
               , ( Attributes (vec3 0 0 0) (vec3 -1 0 0) color 0
                 , Attributes (vec3 0 s s) (vec3 -1 0 0) color 0
                 , Attributes (vec3 0 0 s) (vec3 -1 0 0) color 0
                 )
               , ( Attributes (vec3 0 0 s) (vec3 0 0 1) color 0
                 , Attributes (vec3 s 0 s) (vec3 0 0 1) color 0
                 , Attributes (vec3 s s s) (vec3 0 0 1) color 0
                 )
               , ( Attributes (vec3 0 0 s) (vec3 0 0 1) color 0
                 , Attributes (vec3 s s s) (vec3 0 0 1) color 0
                 , Attributes (vec3 0 s s) (vec3 0 0 1) color 0
                 )
               , ( Attributes (vec3 0 s 0) (vec3 0 1 0) color 0
                 , Attributes (vec3 s s 0) (vec3 0 1 0) color 0
                 , Attributes (vec3 s s s) (vec3 0 1 0) color 0
                 )
               , ( Attributes (vec3 0 s 0) (vec3 0 1 0) color 0
                 , Attributes (vec3 s s s) (vec3 0 1 0) color 0
                 , Attributes (vec3 0 s s) (vec3 0 1 0) color 0
                 )
               , ( Attributes (vec3 s 0 0) (vec3 1 0 0) color 0
                 , Attributes (vec3 s s 0) (vec3 1 0 0) color 0
                 , Attributes (vec3 s s s) (vec3 1 0 0) color 0
                 )
               , ( Attributes (vec3 s 0 0) (vec3 1 0 0) color 0
                 , Attributes (vec3 s s s) (vec3 1 0 0) color 0
                 , Attributes (vec3 s 0 s) (vec3 1 0 0) color 0
                 )
               ]
        -}
        translate =
            Mat4.makeTranslate (vec3 50 50 200)

        map3 f ( a, b, c ) =
            ( f a, f b, f c )

        mapPosition f x =
            { x | position = f x.position }

        translated =
            List.map (map3 (mapPosition (Mat4.transform translate))) attributes
    in
        WebGL.triangles translated


vertexShader : WebGL.Shader Attributes Uniforms Varyings
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        attribute vec4 color;
        attribute float joint;

        uniform mat4 perspective;
        uniform mat4 camera;
        uniform vec3 pt;
        uniform vec3 pt1;
        uniform vec4 spine;
        uniform vec2 pectoral;

        varying vec3 vnormal;
        varying vec4 vcolor;
        void main () {
            gl_Position = perspective * camera * vec4(position, 1.0);
            vnormal = normal;
            vcolor = color;
        }
    |]


fragmentShader : WebGL.Shader {} Uniforms Varyings
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
