module Buffer exposing (..)

import Base64
import Bytes exposing (Bytes)
import Bytes.Decode as BD
import Bytes.Decode.Extra as BDE
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)


type alias Buffer =
    { byteLength : Int
    , uri : Uri
    }



-- Accessors


type ComponentType
    = Byte
    | UnsignedByte
    | Short
    | UnsignedShort
    | UnsignedInt
    | Float


componentTypeSize : ComponentType -> Int
componentTypeSize type_ =
    case type_ of
        Byte ->
            1

        UnsignedByte ->
            1

        Short ->
            2

        UnsignedShort ->
            2

        UnsignedInt ->
            4

        Float ->
            4


type StructureType
    = Scalar
    | Vec2
    | Vec3
    | Vec4
    | Mat2
    | Mat3
    | Mat4


numComponents : StructureType -> Int
numComponents type_ =
    case type_ of
        Scalar ->
            1

        Vec2 ->
            2

        Vec3 ->
            3

        Vec4 ->
            4

        Mat2 ->
            4

        Mat3 ->
            9

        Mat4 ->
            16


type alias Accessor =
    { bufferView : Int
    , byteOffset : Int
    , componentType : ComponentType
    , count : Int
    , minMax : ( List Float, List Float )
    , type_ : StructureType
    }



-- buffer views


type alias BufferView =
    { buffer : Int
    , byteOffset : Int
    , byteLength : Int
    , byteStride : Maybe Int
    , target : Maybe BufferType
    }


type BufferType
    = ArrayBuffer
    | ElementArrayBuffer


type Uri
    = DataUri String
    | RemoteUri String



-- resolving attributes


type alias ResolvedAccessor =
    { viewOffset : Int
    , componentType : ComponentType
    , count : Int
    , minMax : ( List Float, List Float )
    , type_ : StructureType
    , buffer : Bytes
    , accessorOffset : Int
    , byteStride : Int
    , target : Maybe BufferType
    }


toBytes : Uri -> Maybe Bytes
toBytes uri =
    let
        uriString =
            case uri of
                DataUri str ->
                    str

                RemoteUri _ ->
                    Debug.todo "Implement Me"
    in
    uriString
        |> String.dropLeft (String.length "data:application/octet-stream;base64,")
        |> Base64.toBytes


intTriplesDecoder : ResolvedAccessor -> BD.Decoder (List ( Int, Int, Int ))
intTriplesDecoder { accessorOffset, viewOffset, count } =
    BDE.withOffset (accessorOffset + viewOffset)
        (BDE.list (count // 3)
            (BD.map3 (\a b c -> ( a, b, c ))
                (BD.unsignedInt16 Bytes.LE)
                (BD.unsignedInt16 Bytes.LE)
                (BD.unsignedInt16 Bytes.LE)
            )
        )


vec3ListDecoder : ResolvedAccessor -> BD.Decoder (List Vec3)
vec3ListDecoder { accessorOffset, viewOffset, count, byteStride } =
    BDE.withOffset (accessorOffset + viewOffset)
        (BDE.list count
            (BD.map4 (\x y z _ -> vec3 x y z)
                (BD.float32 Bytes.LE)
                (BD.float32 Bytes.LE)
                (BD.float32 Bytes.LE)
                -- skip till number of bytes in bytestride is reached
                (BD.bytes (max 0 (byteStride - 12)))
            )
        )


vec2ListDecoder : ResolvedAccessor -> BD.Decoder (List Vec2)
vec2ListDecoder { accessorOffset, viewOffset, count, byteStride } =
    BDE.withOffset (accessorOffset + viewOffset)
        (BDE.list count
            (BD.map3 (\x y _ -> vec2 x y)
                (BD.float32 Bytes.LE)
                (BD.float32 Bytes.LE)
                -- skip till number of bytes in bytestride is reached
                (BD.bytes (max 0 (byteStride - 8)))
            )
        )
