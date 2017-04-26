module Fish exposing (..)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (vec2, Vec2)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Math.Vector4 as Vec4 exposing (vec4, Vec4)
import Random
import Time
import Time exposing (Time)
import Types exposing (..)
import WebGL


type alias Attributes =
    { position : Vec3
    , normal : Vec3
    , color : Vec4
    , joint : Float
    }


type alias Uniforms =
    { mvp : Mat4
    , world : Mat4
    , spine : Vec4
    , pectoral : Vec2
    , light : Vec3
    }


type alias Varyings =
    { vnormal : Vec3
    , vcolor : Vec4
    }


update : Time -> Random.Seed -> Fish -> ( Random.Seed, Fish )
update dt seed fish =
    let
        ( random1, seed1 ) =
            Random.step (Random.float 0 1) seed

        {-
           sM_ = if random1 <= fish.pa then Active else Inactive
           pt1 = fish.qt
           rt1 = Vec3.getX pt1
           uR = if Vec3.length fish.qt < fish.uSPR then pt1 else pt1 - vec3 fish.dGap 0 0
           ruR = Vec3.getX uR
           uW = if Vec3.length fish.qt >= fish.uSPR then pt1 else pt1 + vec3 fish.dGap 0 0
           ruW = Vec3.getX uW
           wR = fish.drR * fish.tr * fish.tr
           wW = fish.drW * fish.tr * fish.tr
           wT = fish.dtheta * fish.tr * rt1 / (fish.uMax - fish.uMin)
           wP = fish.dphi * fish.tr * rt1 / (fish.uMax - fish.uMin)

           map f (a, b) = (f a, f b)


           dR = map (clamp (fish.uMin * fish.tr) (fish.uSPR * fish.tr)) (ruR - wR, ruR + wR)
           dW = map (clamp (fish.uMin * fish.tr) (fish.uSPR * fish.tr)) (ruW - wW, ruW + wW)
           dtheta = (-wT, wT)
           dphi = (-wP, wP)
           domain_r = if fish.sB == Escape then Tuple.mapFirst (max (Tuple.first dW + Tuple.second dW / 2)) dW
                      else dR
        -}
        pt_ = Vec3.add fish.pt (Vec3.scale (Time.inSeconds dt) fish.qt)
        tube_ =
            case fish.tube of
                (a::b::c) ->
                    if Vec3.distance b fish.pt < 10 then b::c ++ [a] else fish.tube
                _ ->
                    fish.tube
        fish_ =
            { fish | pt = pt_, tube = tube_ }
    in
        ( seed, fish_ )


tube1 : List Vec3
tube1 =
    [ vec3 50 50 50
    , vec3 50 10 200
    , vec3 70 10 200
    , vec3 70 50 50
    ]


fish1 : Fish
fish1 =
    { tMU = 0.5 * Time.second
    , uMin = 1
    , uIPW = 10
    , uSPR = 20
    , uMax = 60
    , pa = 0.8
    , dGap = 0
    , uQ = Rest
    , pt = vec3 50 50 50
    , qt = Vec3.scale 20 (Vec3.normalize (vec3 0 -40 150))
    , sM = Inactive
    , sB = Free
    , tr = 0
    , spine = vec4 0 0 0 0
    , pectoral = vec2 0 0
    , tube = tube1
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

        attributes =
            List.concat
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
    in
        WebGL.triangles attributes


vertexShader : WebGL.Shader Attributes Uniforms Varyings
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        attribute vec4 color;
        attribute float joint;

        uniform mat4 mvp;
        uniform mat4 world;
        uniform vec4 spine;
        uniform vec2 pectoral;

        varying vec3 vnormal;
        varying vec4 vcolor;
        void main () {
            gl_Position = mvp * vec4(position, 1.0);
            vnormal = mat3(world) * normal;
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
