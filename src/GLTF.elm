module GLTF exposing (..)

import Buffer exposing (Accessor, Buffer, BufferView)
import Bytes exposing (Bytes)
import Bytes.Extra as BE
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (vec3)
import Math.Vector4 as Vec4 exposing (vec4)
import Mesh exposing (Attributes, Mesh)
import Set
import Task exposing (Task)
import Util exposing (defaultDecoder, listGetAt, maybeSequence)
import WebGL
import WebGL.Texture as Texture


type GLTF
    = GLTF
        { version : String
        , defaultScene : Int
        , scenes : List Scene
        , cameras : List Camera
        , nodes : List Node
        , meshes : List Mesh
        , materials : List Material
        , textures : List Texture
        , images : List Image
        , samplers : List Sampler
        , accessors : List Accessor
        , bufferViews : List BufferView
        , buffers : List Buffer
        }


type Scene
    = Scene (List Int)


type Camera
    = Perspective { aspectRatio : Float, yfov : Float, zfar : Maybe Float, znear : Float }



--| Orthographic { xmag : Float, ymag : Float, zfar : Float, znear : Float }


type Node
    = MeshNode Mat4.Mat4 Int
    | CameraNode Mat4.Mat4 Int
    | Group Mat4.Mat4 (List Int)


type alias Material =
    { pbrMetallicRoughness : PBRMR
    , emissiveFactor : Vec3.Vec3
    , alphaMode : AlphaMode
    , doubleSided : Bool
    }


type alias PBRMR =
    { baseColorFactor : Vec4.Vec4
    , baseColorTexture : TextureInfo
    , metallicFactor : Float
    , roughnessFactor : Float
    }


type alias TextureInfo =
    { index : Int
    , texCoord : Int
    }


type alias Texture =
    { sampler : Maybe Int
    , source : Int
    }


type AlphaMode
    = Opaque
    | Mask Float
    | Blend



-- getters


getDefaultScene : GLTF -> Maybe Scene
getDefaultScene (GLTF data) =
    listGetAt data.defaultScene data.scenes


getScenes : GLTF -> List Scene
getScenes (GLTF data) =
    data.scenes


getNodes : GLTF -> List Node
getNodes (GLTF gltf) =
    gltf.nodes


getCameras : GLTF -> List Camera
getCameras (GLTF data) =
    data.cameras


getMeshes : GLTF -> List Mesh
getMeshes (GLTF data) =
    data.meshes



-- Decoders


matrixDecoder : JD.Decoder Mat4.Mat4
matrixDecoder =
    JD.list JD.float
        |> JD.andThen
            (\list ->
                case list of
                    [ m11, m21, m31, m41, m12, m22, m32, m42, m13, m23, m33, m43, m14, m24, m34, m44 ] ->
                        JD.succeed <|
                            Mat4.fromRecord
                                { m11 = m11
                                , m21 = m21
                                , m31 = m31
                                , m41 = m41
                                , m12 = m12
                                , m22 = m22
                                , m32 = m32
                                , m42 = m42
                                , m13 = m13
                                , m23 = m23
                                , m33 = m33
                                , m43 = m43
                                , m14 = m14
                                , m24 = m24
                                , m34 = m34
                                , m44 = m44
                                }

                    _ ->
                        JD.fail "Matrix did not have the correct number of 16 entries"
            )


minMaxDecoder : JD.Decoder ( List Float, List Float )
minMaxDecoder =
    JD.map2 Tuple.pair
        (JD.field "min" (JD.list JD.float))
        (JD.field "max" (JD.list JD.float))


vec3Decoder : JD.Decoder Vec3.Vec3
vec3Decoder =
    JD.list JD.float
        |> JD.andThen
            (\list ->
                case list of
                    [ x, y, z ] ->
                        JD.succeed (vec3 x y z)

                    _ ->
                        JD.fail "Could not read vector 3."
            )


vec4Decoder : JD.Decoder Vec4.Vec4
vec4Decoder =
    JD.list JD.float
        |> JD.andThen
            (\list ->
                case list of
                    [ x, y, z, w ] ->
                        JD.succeed (vec4 x y z w)

                    _ ->
                        JD.fail "Could not read vector 4."
            )



-- Scene


defaultSceneDecoder : JD.Decoder Int
defaultSceneDecoder =
    JD.field "scene" JD.int


scenesDecoder : JD.Decoder (List Scene)
scenesDecoder =
    JD.field "scenes"
        (JD.list
            (JD.field "nodes"
                (JD.list JD.int)
                |> JD.map Scene
            )
        )



-- Camera


perspectiveDecoder : JD.Decoder Camera
perspectiveDecoder =
    JD.field "perspective"
        (JD.map4
            (\aspectRatio yfov zfar znear ->
                Perspective
                    { aspectRatio = aspectRatio
                    , yfov = yfov
                    , zfar = zfar
                    , znear = znear
                    }
            )
            (JD.field "aspectRatio" JD.float)
            (JD.field "yfov" JD.float)
            (JD.field "zfar" (JD.maybe JD.float))
            (JD.field "znear" JD.float)
        )


orthographicDecoder : JD.Decoder Camera
orthographicDecoder =
    JD.lazy (\() -> Debug.todo "Add orthographic")


camerasDecoder : JD.Decoder (List Camera)
camerasDecoder =
    JD.field "cameras"
        (JD.list
            (JD.field "type" JD.string
                |> JD.andThen
                    (\string ->
                        case string of
                            "perspective" ->
                                perspectiveDecoder

                            "orthographic" ->
                                orthographicDecoder

                            _ ->
                                JD.fail "Could not decode a camera object"
                    )
            )
        )



-- Nodes


nodesDecoder : JD.Decoder (List Node)
nodesDecoder =
    JD.field "nodes"
        (JD.list
            (defaultDecoder Mat4.identity (JD.field "matrix" matrixDecoder)
                |> JD.andThen
                    (\matrix ->
                        JD.oneOf
                            [ JD.field "children" (JD.list JD.int) |> JD.map (Group matrix)
                            , JD.field "mesh" JD.int |> JD.map (MeshNode matrix)
                            , JD.field "camera" JD.int |> JD.map (CameraNode matrix)
                            ]
                    )
            )
        )



-- Mesh


meshModeDecoder : JD.Decoder Mesh.MeshMode
meshModeDecoder =
    JD.int
        |> JD.andThen
            (\int ->
                case int of
                    4 ->
                        JD.succeed Mesh.Triangles

                    _ ->
                        JD.fail ("Unknown Mesh Mode constant: Got " ++ String.fromInt int)
            )


meshesDecoder : JD.Decoder (List Mesh)
meshesDecoder =
    let
        attributesDecoder =
            JD.map3 Attributes
                (JD.field "NORMAL" JD.int)
                (JD.field "POSITION" JD.int)
                (JD.field "TEXCOORD_0" JD.int)
    in
    JD.field "meshes"
        (JD.list
            (JD.field "primitives"
                (JD.list
                    (JD.map4 Mesh
                        (JD.field "attributes" attributesDecoder)
                        (JD.maybe (JD.field "indices" JD.int))
                        (JD.field "mode" meshModeDecoder)
                        (JD.field "material" JD.int)
                    )
                )
            )
        )
        |> JD.map List.concat


componentTypeDecoder : JD.Decoder Buffer.ComponentType
componentTypeDecoder =
    JD.int
        |> JD.andThen
            (\int ->
                case int of
                    5120 ->
                        JD.succeed Buffer.Byte

                    5121 ->
                        JD.succeed Buffer.UnsignedByte

                    5122 ->
                        JD.succeed Buffer.Short

                    5123 ->
                        JD.succeed Buffer.UnsignedShort

                    5125 ->
                        JD.succeed Buffer.UnsignedInt

                    5126 ->
                        JD.succeed Buffer.Float

                    _ ->
                        JD.fail
                            ("Found unknown component type constant: Got " ++ String.fromInt int)
            )


structureTypeDecoder : JD.Decoder Buffer.StructureType
structureTypeDecoder =
    JD.string
        |> JD.andThen
            (\type_ ->
                case type_ of
                    "SCALAR" ->
                        JD.succeed Buffer.Scalar

                    "VEC2" ->
                        JD.succeed Buffer.Vec2

                    "VEC3" ->
                        JD.succeed Buffer.Vec3

                    "Vec4" ->
                        JD.succeed Buffer.Vec4

                    "MAT2" ->
                        JD.succeed Buffer.Mat2

                    "MAT3" ->
                        JD.succeed Buffer.Mat3

                    "MAT4" ->
                        JD.succeed Buffer.Mat4

                    _ ->
                        JD.fail
                            ("Found unknown numComponet constant: Got " ++ type_)
            )



-- Textures


texturesDecoder : JD.Decoder (List Texture)
texturesDecoder =
    JD.field "textures"
        (JD.list
            (JD.map2 Texture
                (JD.maybe (JD.field "sampler" JD.int))
                (JD.field "source" JD.int)
            )
        )



-- Materials


textureInfoDecoder : JD.Decoder TextureInfo
textureInfoDecoder =
    JD.map2 TextureInfo
        (JD.field "index" JD.int)
        (defaultDecoder 0 (JD.field "texCoord" JD.int))


pbrMRDecoder : JD.Decoder PBRMR
pbrMRDecoder =
    JD.map4
        PBRMR
        (defaultDecoder (vec4 1 1 1 1) (JD.field "baseColorFactor" vec4Decoder))
        (JD.field "baseColorTexture" textureInfoDecoder)
        (defaultDecoder 1 (JD.field "metallicFactor" JD.float))
        (defaultDecoder 1 (JD.field "roughnessFactor" JD.float))


alphaModeDecoder : JD.Decoder AlphaMode
alphaModeDecoder =
    JD.string
        |> JD.andThen
            (\str ->
                case str of
                    "OPAQUE" ->
                        JD.succeed Opaque

                    "MASK" ->
                        JD.field "alphaCutoff" JD.float
                            |> JD.map Mask

                    "BLEND" ->
                        JD.succeed Blend

                    _ ->
                        JD.fail
                            ("Need to see OPAQUE, MASK or BLEND"
                                ++ "but got '"
                                ++ str
                                ++ "'"
                            )
            )


materialsDecoder : JD.Decoder (List Material)
materialsDecoder =
    JD.field "materials"
        (JD.list <|
            JD.map4 Material
                --TODO add default for PBR object
                (JD.field "pbrMetallicRoughness" pbrMRDecoder)
                (defaultDecoder (vec3 0 0 0) (JD.field "emissiveFactor" vec3Decoder))
                (defaultDecoder Opaque (JD.field "alphaMode" alphaModeDecoder))
                (defaultDecoder False (JD.field "doubleSided" JD.bool))
        )



-- Images


type Image
    = ImageUri String
    | ImageBuffer String String


imagesDecoder : JD.Decoder (List Image)
imagesDecoder =
    JD.field "images" (JD.list imageDecoder)


imageDecoder : JD.Decoder Image
imageDecoder =
    JD.oneOf
        [ JD.map ImageUri (JD.field "uri" JD.string)
        , JD.map2
            ImageBuffer
            (JD.field "mimeType" JD.string)
            (JD.field "bufferView" JD.string)
        ]



-- Samplers


type alias Sampler =
    { magFilter : ResizeBigger
    , minFilter : ResizeSmaller
    , wrapS : Wrap
    , wrapT : Wrap
    }


defaultSampler =
    { magFilter = LinearBigger
    , minFilter = NearestMipMapLinear
    , wrapS = Repeat
    , wrapT = Repeat
    }


samplerDec : a -> String -> (Int -> Maybe a) -> JD.Decoder a
samplerDec default prop intToSamplerValue =
    let
        mapper =
            intToSamplerValue >> Maybe.withDefault default
    in
    -- TODO this should fail if intToSamplerValue returns nothing
    -- hard to get nested decoder to fail if non-existing prop should
    -- give default value
    defaultDecoder default
        (JD.field prop (JD.map mapper JD.int))


samplersDecoder : JD.Decoder (List Sampler)
samplersDecoder =
    JD.field "samplers"
        (JD.list
            (JD.map4 Sampler
                (samplerDec LinearBigger "magFilter" intToResizeBigger)
                (samplerDec NearestMipMapLinear "minFilter" intToResizeSmaller)
                (samplerDec Repeat "wrapS" intToWrap)
                (samplerDec Repeat "wrapT" intToWrap)
            )
        )


type ResizeBigger
    = NearestBigger
    | LinearBigger


intToResizeBigger : Int -> Maybe ResizeBigger
intToResizeBigger int =
    case int of
        9728 ->
            Just NearestBigger

        9729 ->
            Just LinearBigger

        _ ->
            Nothing


type ResizeSmaller
    = NearestSmaller
    | LinearSmaller
    | NearestMipMapNearest
    | LinearMipMapNearest
    | NearestMipMapLinear
    | LinearMipMapLinear


intToResizeSmaller : Int -> Maybe ResizeSmaller
intToResizeSmaller int =
    case int of
        9728 ->
            Just NearestSmaller

        9729 ->
            Just LinearSmaller

        9984 ->
            Just NearestMipMapNearest

        9985 ->
            Just LinearMipMapNearest

        9986 ->
            Just NearestMipMapLinear

        9987 ->
            Just LinearMipMapLinear

        _ ->
            Nothing


intToWrap : Int -> Maybe Wrap
intToWrap int =
    case int of
        10497 ->
            Just Repeat

        33071 ->
            Just ClampToEdge

        33648 ->
            Just MirroredRepeat

        _ ->
            Nothing


{-| Sets the wrap parameter for texture coordinate.
-}
type Wrap
    = Repeat
    | ClampToEdge
    | MirroredRepeat



-- Accessors


accessorsDecoder : JD.Decoder (List Accessor)
accessorsDecoder =
    JD.field "accessors"
        (JD.list
            (JD.map6 Accessor
                (JD.field "bufferView" JD.int)
                (JD.field "byteOffset" JD.int)
                (JD.field "componentType" componentTypeDecoder)
                (JD.field "count" JD.int)
                minMaxDecoder
                (JD.field "type" structureTypeDecoder)
            )
        )



-- buffer views


targetDecoder : JD.Decoder Buffer.BufferType
targetDecoder =
    JD.int
        |> JD.andThen
            (\int ->
                case int of
                    34962 ->
                        JD.succeed Buffer.ArrayBuffer

                    34963 ->
                        JD.succeed Buffer.ElementArrayBuffer

                    _ ->
                        JD.fail
                            ("Invalid buffer type constant: Got " ++ String.fromInt int)
            )


bufferViewsDecoder : JD.Decoder (List BufferView)
bufferViewsDecoder =
    JD.field "bufferViews"
        (JD.list
            (JD.map5 BufferView
                (JD.field "buffer" JD.int)
                (JD.field "byteOffset" (defaultDecoder 0 JD.int))
                (JD.field "byteLength" JD.int)
                (JD.maybe (JD.field "byteStride" JD.int))
                (JD.maybe (JD.field "target" targetDecoder))
            )
        )



-- buffers


uriDecoder : JD.Decoder Buffer.Uri
uriDecoder =
    JD.string
        |> JD.map
            (\uri ->
                if String.startsWith "data:" uri then
                    Buffer.DataUri uri

                else
                    Buffer.RemoteUri uri
            )


buffersDecoder : JD.Decoder (List Buffer)
buffersDecoder =
    JD.field "buffers"
        (JD.list
            (JD.map2
                Buffer
                (JD.field "byteLength" JD.int)
                (JD.field "uri"
                    uriDecoder
                )
            )
        )


gltfEmbeddedDecoder : JD.Decoder GLTF
gltfEmbeddedDecoder =
    JD.succeed
        (\version scene scenes cameras nodes meshes materials textures images samplers accessors bufferViews buffers ->
            GLTF
                { version = version
                , defaultScene = scene
                , scenes = scenes
                , cameras = cameras
                , nodes = nodes
                , meshes = meshes
                , materials = materials
                , textures = textures
                , images = images
                , samplers = samplers
                , accessors = accessors
                , bufferViews = bufferViews
                , buffers = buffers
                }
        )
        |> JDP.required "asset" (JD.field "version" JD.string)
        |> JDP.custom defaultSceneDecoder
        |> JDP.custom scenesDecoder
        |> JDP.custom camerasDecoder
        |> JDP.custom nodesDecoder
        |> JDP.custom meshesDecoder
        |> JDP.custom materialsDecoder
        |> JDP.custom texturesDecoder
        |> JDP.custom imagesDecoder
        |> JDP.custom samplersDecoder
        |> JDP.custom accessorsDecoder
        |> JDP.custom bufferViewsDecoder
        |> JDP.custom buffersDecoder


resolveAccessors : GLTF -> Maybe (List Buffer.ResolvedAccessor)
resolveAccessors (GLTF gltf) =
    let
        maybeBuffers =
            List.foldr
                (\current seq ->
                    Maybe.map2 (::)
                        (Buffer.toBytes current.uri)
                        seq
                )
                (Just [])
                gltf.buffers

        toResolvedAccessor ( accessor, bufferView, buffer ) =
            { viewOffset = bufferView.byteOffset
            , componentType = accessor.componentType
            , count = accessor.count
            , minMax = accessor.minMax
            , type_ = accessor.type_
            , buffer = buffer
            , accessorOffset = accessor.byteOffset
            , byteStride = Maybe.withDefault 0 bufferView.byteStride
            , target = bufferView.target
            }

        collectData buffers accessor =
            listGetAt accessor.bufferView gltf.bufferViews
                |> Maybe.andThen
                    (\bufferView ->
                        listGetAt bufferView.buffer buffers
                            |> Maybe.map (\buffer -> ( accessor, bufferView, buffer ))
                    )
                |> Maybe.map toResolvedAccessor
    in
    maybeBuffers
        |> Maybe.andThen
            (\buffers ->
                List.map (collectData buffers) gltf.accessors
                    |> maybeSequence
            )


resolveMaterials : GLTF -> Maybe (Task Texture.Error (List Texture.Texture))
resolveMaterials (GLTF gltf) =
    let
        imageUris =
            List.map
                (\image ->
                    case image of
                        ImageUri uri ->
                            uri

                        ImageBuffer mimeType bufferViewIndex ->
                            Debug.todo "Implement creation of data url from buffer"
                )
                gltf.images

        maybeTextures =
            List.map
                (\tex ->
                    let
                        sampler =
                            case tex.sampler of
                                Just index ->
                                    listGetAt index gltf.samplers

                                Nothing ->
                                    Just defaultSampler
                    in
                    -- TODO replace defaultOptions with proper options derived from sampler options
                    listGetAt tex.source imageUris
                        |> Maybe.map (Texture.loadWith Texture.defaultOptions)
                )
                gltf.textures
    in
    Maybe.andThen
        (\textures ->
            List.map
                (\material ->
                    let
                        index =
                            material.pbrMetallicRoughness.baseColorTexture.index
                    in
                    listGetAt index textures
                )
                gltf.materials
                |> maybeSequence
                |> Maybe.map Task.sequence
        )
        (maybeSequence maybeTextures)

