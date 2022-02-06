module Basic exposing (main)

import Browser
import Browser.Events
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Embedded
import GLTF exposing (..)
import Html exposing (Html, br, button, div, input, label, option, select, text)
import Html.Attributes exposing (height, selected, style, type_, value, width)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Json.Decode as JD
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Mesh
import Scene
import Task
import Url.Builder exposing (crossOrigin)
import Util exposing (Uri(..), listGetAt, unwrapUri)
import WebGL
import WebGL.Texture as Texture exposing (Texture)


type alias Model =
    { meshes : List ( Mat4, WebGL.Mesh Mesh.PositionNormalTexCoordsAttributes )
    , cameras : List ( Mat4, Scene.Camera )
    , texture : Maybe Texture
    , currentCamera : Maybe ( Mat4, GLTF.Camera )
    , elapsedTime : Float
    , rotationActive : Bool
    , gltfScene : Maybe GLTF.GLTF
    , linkedAssets : List Util.Uri
    , linkedAssetValues : Dict String Bytes
    , images : Dict String Texture
    }


canvas =
    { width = 600, height = 400 }


mulMatrices : List Mat4 -> Mat4
mulMatrices =
    List.foldr Mat4.mul Mat4.identity


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { meshes = []
      , cameras = []
      , texture = Nothing
      , currentCamera = Nothing
      , elapsedTime = 0
      , rotationActive = False
      , gltfScene = Nothing
      , linkedAssets = []
      , linkedAssetValues = Dict.empty
      , images = Dict.empty
      }
    , loadGltf
    )



{- let
       sceneResult =
           JD.decodeString GLTF.gltfEmbeddedDecoder Embedded.duckEmbedded
               |> Result.mapError JD.errorToString
               |> Result.map
                   (\gltf ->
                       let
                           _ =
                               Debug.log "GLTF" (GLTF.getNodes gltf)
                       in
                       gltf
                   )
               |> Result.andThen (Scene.fromGLTF >> Result.fromMaybe "Could not make scene")
               |> Result.map
                   (\scene ->
                       ( Debug.log "Meshes" <| Scene.getDrawables scene
                       , Debug.log "Cameras " <| Scene.getCameras scene
                       )
                   )
   in
   case sceneResult of
       Ok ( meshes, cameras ) ->
           ( { meshes = meshes
             , cameras = cameras
             , texture = Nothing
             , currentCamera = defaultCamera
             , elapsedTime = 0
             , rotationActive = False
             , gltfScene = Loading
             }
           , Task.attempt GotTexture <|
               -- TODO make this dynamic instead of hardcoded
               Texture.loadWith
                   { magnify = Texture.linear
                   , minify = Texture.nearestMipmapLinear
                   , horizontalWrap = Texture.repeat
                   , verticalWrap = Texture.repeat
                   , flipY = False
                   }
                   Embedded.textureEmbedded
           )

       Err err ->
           Debug.todo err
-}


type Msg
    = GotTexture (Result Texture.Error Texture)
    | CameraChanged String
    | Tick Float
    | RotateChange Bool
    | LoadGLTF
    | GotGLTF (Result Http.Error GLTF.GLTF)
    | GotBytes (Result Http.Error ( String, Bytes.Bytes ))
    | ParseBuffers
    | LoadImages


expectRawBytes : (Result Http.Error ( String, Bytes.Bytes ) -> msg) -> Http.Expect msg
expectRawBytes toMsg =
    Http.expectBytesResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    Ok ( metadata.url, body )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTexture (Ok texture) ->
            ( { model | texture = Just texture }, Cmd.none )

        GotTexture (Err error) ->
            ( model, Cmd.none )

        GotGLTF result ->
            case result of
                Ok scene ->
                    ( { model | gltfScene = Just scene }, linkedAssets scene )

                Err _ ->
                    ( { model | gltfScene = Nothing }, Cmd.none )

        CameraChanged value ->
            let
                camera =
                    String.toInt
                        value
                        |> Maybe.andThen (\index -> listGetAt index model.cameras)
                        |> Maybe.withDefault defaultCamera
                        |> Debug.log "default camera"
            in
            ( { model | currentCamera = Just camera }, Cmd.none )

        Tick millis ->
            ( { model | elapsedTime = model.elapsedTime + millis }, Cmd.none )

        RotateChange active ->
            ( { model | rotationActive = active }, Cmd.none )

        LoadGLTF ->
            ( model, loadGltf )

        LoadImages ->
            ( model, Cmd.none )

        ParseBuffers ->
            ( model, Cmd.none )

        GotBytes result ->
            case result of
                Ok ( url, bytes ) ->
                    ( { model | linkedAssetValues = Dict.insert url bytes model.linkedAssetValues }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


type alias Attributes =
    { position : Vec3
    , normal : Vec3
    , texCoords : Vec2
    }


type alias Varyings =
    { vCoords : Vec2 }


type alias Uniforms =
    { transform : Mat4
    , texture : Texture
    }


loadGltf : Cmd Msg
loadGltf =
    Http.get
        { url = crossOrigin "http://localhost:8080" [ "Duck.gltf" ] []
        , expect = Http.expectJson GotGLTF GLTF.gltfEmbeddedDecoder
        }


linkedAssets : GLTF.GLTF -> Cmd Msg
linkedAssets (GLTF.GLTF gltf) =
    gltf.buffers
        |> List.map
            (\current ->
                case current.uri of
                    RemoteUri uri ->
                        Http.get { url = crossOrigin "http://localhost:8080" [ uri ] [], expect = expectRawBytes GotBytes }

                    DataUri uri ->
                        Http.get { url = uri, expect = expectRawBytes GotBytes }
            )
        |> Cmd.batch


loadImages : GLTF.GLTF -> Cmd Msg
loadImages (GLTF.GLTF gltf) =
    Cmd.none


perspectiveMatrix : GLTF.Camera -> Mat4
perspectiveMatrix (GLTF.Perspective { yfov, aspectRatio, znear, zfar }) =
    Mat4.makePerspective
        -- turn radians into degrees
        (yfov / pi * 180)
        aspectRatio
        znear
        (zfar |> Maybe.withDefault 10000)


rotate : Float -> Mat4 -> Mat4
rotate elapsedTime matrix =
    let
        turnsPerSecond =
            0.2

        rotation =
            Mat4.makeRotate (turns (turnsPerSecond * elapsedTime / 1000))
                (vec3 0 1 0)
    in
    Mat4.mul rotation matrix


defaultCamera : ( Mat4, GLTF.Camera )
defaultCamera =
    ( Mat4.makeTranslate (vec3 0 0 10)
    , GLTF.Perspective { yfov = 0.66, aspectRatio = 1.5, znear = 1, zfar = Just 10000 }
    )


cameraEntity : Mat4 -> Mat4 -> GLTF.Camera -> WebGL.Entity
cameraEntity worldTransform cameraTransform camera =
    let
        vShader =
            [glsl|
                attribute vec3 pos;
                uniform mat4 transform;

                void main () {
                  gl_Position = transform * vec4(pos, 1.0);
                }
              |]

        fShader =
            [glsl|
                precision mediump float;

                void main () {
                  gl_FragColor =  vec4(0, 0, 0, 1);
                }
              |]
    in
    WebGL.entity vShader
        fShader
        (WebGL.lines
            [ ( { pos = vec3 0 0 0 }, { pos = vec3 1 1 -5 } )
            , ( { pos = vec3 0 0 0 }, { pos = vec3 1 -1 -5 } )
            , ( { pos = vec3 0 0 0 }, { pos = vec3 -1 1 -5 } )
            , ( { pos = vec3 0 0 0 }, { pos = vec3 -1 -1 -5 } )
            , ( { pos = vec3 1 1 -5 }, { pos = vec3 1 -1 -5 } )
            , ( { pos = vec3 1 1 -5 }, { pos = vec3 -1 1 -5 } )
            , ( { pos = vec3 -1 -1 -5 }, { pos = vec3 1 -1 -5 } )
            , ( { pos = vec3 -1 -1 -5 }, { pos = vec3 -1 1 -5 } )
            ]
        )
        { transform =
            mulMatrices
                [ perspectiveMatrix camera
                , viewMatrix cameraTransform
                , worldTransform
                , Mat4.makeScale (vec3 20 20 20)
                ]
        }


{-| This is a bit unsafe, because it won't handle the case properly
when a matrix is not inverible
-}
viewMatrix : Mat4 -> Mat4
viewMatrix cameraMatrix =
    Mat4.inverse cameraMatrix |> Maybe.withDefault Mat4.identity


uniforms : Texture -> Mat4.Mat4 -> Mat4 -> GLTF.Camera -> Uniforms
uniforms texture worldTransform cameraTransform camera =
    { transform =
        mulMatrices
            [ perspectiveMatrix camera
            , viewMatrix cameraTransform
            , worldTransform
            ]
    , texture = texture
    }


viewCanvas :
    Float
    -> Texture.Texture
    -> List ( Mat4, GLTF.Camera )
    -> List ( Mat4, WebGL.Mesh Mesh.PositionNormalTexCoordsAttributes )
    -> ( Mat4, GLTF.Camera )
    -> Html Msg
viewCanvas elapsedTime texture cameras meshes ( cameraMatrix, camera ) =
    div [ style "display" "flex" ]
        [ WebGL.toHtml
            [ width canvas.width
            , height canvas.height
            , style "border" "1px dashed gray"
            ]
          <|
            List.concat
                [ List.map
                    (\( transform, mesh ) ->
                        WebGL.entity vertexShader
                            fragmentShader
                            mesh
                            (uniforms
                                texture
                                (rotate elapsedTime transform)
                                cameraMatrix
                                camera
                            )
                    )
                    meshes
                , List.map
                    (\( matrix, _ ) ->
                        cameraEntity matrix cameraMatrix camera
                    )
                    cameras
                ]
        , div []
            [ select [ onInput CameraChanged ] <|
                option [ value "default" ] [ text "default" ]
                    :: List.indexedMap
                        (\index _ ->
                            option [ value (String.fromInt index) ]
                                [ text ("camera" ++ String.fromInt index) ]
                        )
                        cameras
            , br [] []
            , label []
                [ text "Rotate"
                , input [ type_ "checkbox", onCheck RotateChange ] []
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text "GLTF! What the 🦆!" ]
        , case model.texture of
            Just texture ->
                viewCanvas
                    model.elapsedTime
                    texture
                    model.cameras
                    model.meshes
                    defaultCamera

            Nothing ->
                div [] [ text "Waiting for texture ..." ]
        ]


vertexShader : WebGL.Shader Attributes Uniforms Varyings
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        attribute vec2 texCoords;
        uniform mat4 transform;
        varying vec2 vCoords;

        void main () {
          gl_Position = transform * vec4(position, 1.0);
          vCoords = texCoords;
        }
  |]


fragmentShader : WebGL.Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;
        uniform sampler2D texture;
        varying vec2 vCoords;

        void main () {
          gl_FragColor =  texture2D(texture, vCoords);
        }
  |]


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.rotationActive then
        Browser.Events.onAnimationFrameDelta Tick

    else
        Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
