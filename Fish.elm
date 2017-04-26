module Fish exposing (..)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (vec2, Vec2)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Math.Vector4 as Vec4 exposing (vec4, Vec4)
import Random
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


update : Float -> Random.Seed -> Fish -> ( Random.Seed, Fish )
update dt seed fish =
    let
        ( random1, seed1 ) =
            Random.step (Random.float 0 1) seed

        toPolar3 v =
            let
                ( x, y, z ) =
                    Vec3.toTuple v
            in
                vec3 (Vec3.length v) (atan2 y x) (acos (sqrt ((x^2+y^2)/(x^2+y^2+z^2))))

        fromPolar3 v =
            let
                ( r, theta, phi ) =
                    Vec3.toTuple v
            in
                vec3 (r * cos phi * cos theta) (r * cos phi * sin theta) (r * sin phi)

        sM =
            if random1 <= fish.pa then
                Active
            else
                Inactive

        pt1 =
            Vec3.add fish.pt (Vec3.scale fish.tr fish.qt)

        ( r, theta, phi ) =
            Vec3.toTuple (toPolar3 fish.pt)

        ( r1, theta1, phi1 ) =
            Vec3.toTuple (toPolar3 pt1)

        ( drmax, dthetamax, dphimax ) =
            ( 10, degrees 10, degrees 10 )

        uR =
            vec3 r1 theta1 phi1

        wrR =
            drmax * (fish.tr ^ 2)

        wtheta =
            dthetamax * fish.tr * r1 / (fish.uMax - fish.uMin)

        wphi =
            dphimax * fish.tr * r1 / (fish.uMax - fish.uMin)

        domainrR =
            ( r1 - wrR, r1 + wrR )

        domaintheta =
            ( -wtheta, wtheta )

        domainphi =
            ( -wphi, wphi )

        clip ( l, r ) ( ll, rr ) =
            if r <= ll then
                ( r, r )
            else if rr <= l then
                ( l, l )
            else
                ( max l ll, min r rr )

        domainrR2 =
            clip domainrR ( fish.uMin * fish.tr, fish.uSPR * fish.tr )

        dtube =
            case fish.tube of
                a :: b :: c ->
                    let
                        ab =
                            Vec3.normalize (Vec3.sub b a)

                        ptb =
                            Vec3.sub b fish.pt

                        l =
                            Vec3.dot ab ptb
                                |> flip Vec3.scale ab
                                |> Vec3.sub ptb
                                |> Vec3.length
                    in
                        if l <= 10 then
                            ab
                        else
                            ptb

                _ ->
                    vec3 0 0 0

        ( thetatube, phitube ) =
            let
                ( x, y, z ) =
                    Vec3.toTuple (toPolar3 dtube)
            in
                ( y - theta, z - phi )

        ( wthetatube, wphitube ) =
            ( degrees 10, degrees 10 )

        domaintheta2 =
            clip domaintheta ( thetatube - wthetatube, thetatube + wthetatube )

        domainphi2 =
            clip domainphi ( phitube - wphitube, phitube - wphitube )

        boxmuller seed =
            let
                ( u1, seed1 ) =
                    Random.step (Random.float 0 1) seed
                ( u2, seed2 ) =
                    Random.step (Random.float 0 1) seed1
            in
                ( sqrt (-2 * logBase e u1) * cos (2 * pi * u2)
                , sqrt (-2 * logBase e u1) * sin (2 * pi * u2)
                , seed2
                )

        ( g1, g2, seed2 ) =
            boxmuller seed1

        ( g3, g4, seed3 ) =
            boxmuller seed2

        sample g (l, r) =
            let
                gg = g * 1 + (l + r) / 2
            in
                clamp l r gg

        pt =
            fromPolar3 (vec3 r1 theta1 phi1)
--            fromPolar3 (vec3 (sample g1 domainrR2) (sample g2 domaintheta2) (sample g3 domainphi2))

        (tube, qt) =
            case fish.tube of
                a :: b :: c :: d ->
                    if Vec3.distance b fish.pt < 10 then
                        (b :: c :: d ++ [ a ], Vec3.scale 2 (Vec3.normalize (Vec3.sub c b)))
                    else
                        (fish.tube, fish.qt)

                _ ->
                    (fish.tube, fish.qt)

        tr =
            if fish.tr <= dt then
                fish.tMU
            else
                fish.tr - dt

        fish_ =
            { fish
                | sM = sM
                , pt = pt
                , qt = qt
                , tube = tube
                , tr = tr
            }
    in
        ( seed3, fish_ )


tube1 : List Vec3
tube1 =
    [ vec3 50 50 50
    , vec3 51 51 150
    , vec3 150 50 151
    , vec3 201 51 51
    ]


fish1 : Fish
fish1 =
    { tMU = 0.5
    , uMin = 1
    , uIPW = 10
    , uSPR = 20
    , uMax = 60
    , pa = 0.95
    , dGap = 0
    , uQ = Rest
    , pt = vec3 50 50 50
    , qt = Vec3.scale 2 (Vec3.normalize (vec3 1 1 100))
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
