module Canopy.Node exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


{-| A node, basically a [Rose Tree](https://en.wikipedia.org/wiki/Rose_tree).
-}
type Node a
    = Node a (List (Node a))


{-| Decode a Node. You must specify a datum decoder.

    case Decode.decodeString (decodeNode Decode.string) node of
        Ok decoded ->
            ...
        Err _ ->
            ...

-}
decode : Decoder a -> Decoder (Node a)
decode decodeDatum =
    Decode.map2 Node
        (Decode.field "value" decodeDatum)
        (Decode.field "children" (Decode.list (Decode.lazy (\_ -> decode decodeDatum))))


{-| Encode a Node. You must provide an encoder for the datum type.

    import Json.Encode as Encode

        node "foo" [ leaf "bar" ]
            |> encode Encode.string
            |> Encode.encode 0
            -- {"value":"foo","children":[{"value":"bar",children:[]}]}

-}
encode : (a -> Encode.Value) -> Node a -> Encode.Value
encode datumEncoder (Node datum children) =
    Encode.object
        [ ( "value", datumEncoder datum )
        , ( "children", children |> List.map (encode datumEncoder) |> Encode.list )
        ]


{-| Extracts children from a Node.
-}
children : Node a -> List (Node a)
children (Node _ children) =
    children


{-| Extracts a datum from a Node.
-}
datum : Node a -> a
datum (Node datum _) =
    datum


flatMap : (Node a -> b) -> Node a -> List b
flatMap mapper node =
    node
        |> children
        |> List.foldl
            (\node acc -> List.concat [ acc, [ mapper node ], node |> children |> List.map mapper ])
            [ mapper node ]


get : a -> Node a -> Maybe (Node a)
get target node =
    if target == datum node then
        Just node
    else
        node
            |> children
            |> List.map (get target)
            |> List.filter ((/=) Nothing)
            |> List.head
            |> Maybe.withDefault Nothing


map : (a -> b) -> Node a -> Node b
map mapper (Node datum children) =
    Node (mapper datum) (children |> List.map (map mapper))


{-| Create a Node.
-}
node : a -> List (Node a) -> Node a
node datum children =
    Node datum children


parent : a -> Node a -> Maybe (Node a)
parent target candidate =
    candidate
        |> children
        |> List.foldl
            (\node acc ->
                case acc of
                    Just found ->
                        Just found

                    Nothing ->
                        if datum node == target then
                            Just candidate
                        else
                            parent target node
            )
            Nothing


path : a -> Node a -> List a
path target rootNode =
    let
        path__ target node =
            case parent target node of
                Just parentNode ->
                    path__ (datum parentNode) rootNode ++ [ datum parentNode ]

                Nothing ->
                    []
    in
        case parent target rootNode of
            Nothing ->
                if target == datum rootNode then
                    [ target ]
                else
                    []

            Just rootNode ->
                path__ target rootNode ++ [ target ]


seek : (a -> Bool) -> Node a -> List a
seek test node =
    node
        |> flatMap identity
        |> List.filter (datum >> test)
        |> List.map datum


replace : a -> Node a -> Node a -> Node a
replace target replacement root =
    if datum root == target then
        replacement
    else
        let
            newChildren =
                root |> children |> List.map (replace target replacement)
        in
            root |> updateChildren newChildren


{-| Update a Node's children.
-}
updateChildren : List (Node a) -> Node a -> Node a
updateChildren children (Node datum _) =
    Node datum children


{-| Update a Node's datum.
-}
updateDatum : a -> Node a -> Node a
updateDatum datum (Node _ children) =
    Node datum children
