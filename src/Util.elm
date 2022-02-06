module Util exposing
    ( Uri(..)
    , defaultDecoder
    , listGetAt
    , listToTriples
    , maybeSequence
    , toIndexedList
    , unwrapUri
    , uriDecoder
    )

import Json.Decode as JD


type Uri
    = DataUri String
    | RemoteUri String


unwrapUri : Uri -> String
unwrapUri uri =
    case uri of
        RemoteUri str ->
            str

        DataUri str ->
            str


uriDecoder : JD.Decoder Uri
uriDecoder =
    JD.string
        |> JD.map
            (\uri ->
                if String.startsWith "data:" uri then
                    DataUri uri

                else
                    RemoteUri uri
            )


toIndexedList : List a -> List ( Int, a )
toIndexedList =
    List.indexedMap Tuple.pair


defaultDecoder : a -> JD.Decoder a -> JD.Decoder a
defaultDecoder default decoder =
    JD.oneOf [ decoder, JD.succeed default ]


maybeSequence : List (Maybe a) -> Maybe (List a)
maybeSequence =
    List.foldr (Maybe.map2 (::)) (Just [])



-- list find helper


listGetAt : Int -> List a -> Maybe a
listGetAt index list =
    listGetAtHelp index list 0


listGetAtHelp : Int -> List a -> Int -> Maybe a
listGetAtHelp index list currentIndex =
    case list of
        head :: tail ->
            if currentIndex == index then
                Just head

            else
                listGetAtHelp index tail (currentIndex + 1)

        [] ->
            Nothing


listToTriples : List a -> Maybe (List ( a, a, a ))
listToTriples list =
    listToTriplesHelp list []


listToTriplesHelp : List a -> List ( a, a, a ) -> Maybe (List ( a, a, a ))
listToTriplesHelp list triples =
    case list of
        x :: y :: z :: rest ->
            listToTriplesHelp rest (( x, y, z ) :: triples)

        [] ->
            Just (List.reverse triples)

        _ ->
            Nothing
