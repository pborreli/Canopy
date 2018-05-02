module Canopy
    exposing
        ( Node(..)
        , appendChild
        , datum
        , decode
        , encode
        , filter
        , get
        , flatMap
        , flatten
        , leaf
        , map
        , node
        , parent
        , path
        , prependChild
        , remove
        , replace
        , seek
        , siblings
        , toList
        , tuple
        , updateChildren
        , updateDatum
        )

{-| A generic [Rose Tree](https://en.wikipedia.org/wiki/Rose_tree).

TODO:

  - rename datum to value
  - reorg docs, code and tests using the same segmentation
  - replaceChildren
  - deal with non-unique nodes resiliently:
      - remove all nodes matching the provided datum
      - append/prepend a value to each nodes matching the provided datum
      - replace all nodes matching the provided datum
      - get -> first match from left
      - parent -> first match from left
      - siblings -> first match from left


# Basics

@docs Node


# Building and manipulating a Tree

@docs node, leaf, appendChild, prependChild, remove, updateChildren, updateDatum


# Manipulating a Tree

@docs replace, filter, flatMap, flatten, map, tuple


# Querying a Tree

@docs datum, get, parent, path, seek, siblings


# Importing and exporting

@docs decode, encode, toList

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


{-| A tree node.
-}
type Node a
    = Node a (List (Node a))


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


{-| Create a Node.
-}
node : a -> List (Node a) -> Node a
node datum children =
    Node datum children



-- Decode


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



-- Encode


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



-- Getters


{-| Turn a Node into a tuple containing the datum and the parent datum, if any.
-}
tuple : Node a -> Node a -> ( a, Maybe a )
tuple root node =
    ( datum node, root |> parent (datum node) |> Maybe.map datum )



-- Manipulation


{-| Append a new datum to a Node identified by its datum in a Tree.

    tree "foo" [ leaf "bar"]
        |> appendChild "foo" "baz"
        == tree "foo" [ leaf "bar", leaf "qux" ]

-}
appendChild : a -> a -> Node a -> Node a
appendChild target child node =
    if target == datum node then
        node |> updateChildren (children node ++ [ leaf child ])
    else
        node |> updateChildren (node |> children |> List.map (appendChild target child))


{-| Deletes a Node from a Tree, referenced by its attached datum. Noop when
attempting to delete the tree itself.
-}
remove : a -> Node a -> Node a
remove target tree =
    case tree |> parent target of
        Just parentNode ->
            let
                newChildren =
                    parentNode |> children |> List.filter (\node -> datum node /= target)

                newParent =
                    parentNode |> updateChildren newChildren
            in
                tree |> replace (datum parentNode) newParent

        Nothing ->
            tree


{-| Filter a Tree, keeping only nodes which attached datum satisfies the
provided test, and their ancestors up to the tree root.
-}
filter : (a -> Bool) -> Node a -> Node a
filter test tree =
    let
        toDelete =
            tree |> seek (not << test)

        toPreserve =
            tree |> seek test |> List.map (\datum -> path datum tree) |> List.concat
    in
        toDelete
            |> List.filter (\datum -> List.member datum toPreserve |> not)
            |> List.foldl remove tree


{-| Map each node using a mapping function then flatten the result into a new list.
-}
flatMap : (Node a -> b) -> Node a -> List b
flatMap mapper node =
    List.foldl
        (\node acc -> List.concat [ acc, [ mapper node ], node |> children |> List.map mapper ])
        [ mapper node ]
        (children node)


{-| Flatten a Tree.
-}
flatten : Node a -> List (Node a)
flatten node =
    node |> flatMap identity


{-| Find a Node in a Tree, identified by its datum.
-}
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


{-| Create a Tree leaf node, a.k.a singleton.
-}
leaf : a -> Node a
leaf datum =
    Node datum []


{-| Map all nodes data in a Tree.
-}
map : (a -> b) -> Node a -> Node b
map mapper (Node datum children) =
    Node (mapper datum) (children |> List.map (map mapper))


{-| Retrieve the parent of a given node in a Tree, identified by its datum.
-}
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


{-| Compute the path to a Node from the root. Returns an empty list when
the target Node doesn't exist in the tree.
-}
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


{-| Prepend a new datum to a Node identified by its datum in a Tree.

    tree "foo" [ leaf "bar"]
        |> prependChild "foo" "baz"
        == tree "foo" [ leaf "baz", leaf "bar" ]

-}
prependChild : a -> a -> Node a -> Node a
prependChild target child node =
    if target == datum node then
        node |> updateChildren (leaf child :: children node)
    else
        updateChildren (node |> children |> List.map (prependChild target child)) node


{-| Retrieve all data from nodes containing a datum satisfying a provided condition.
-}
seek : (a -> Bool) -> Node a -> List a
seek test node =
    node
        |> flatMap identity
        |> List.filter (datum >> test)
        |> List.map datum


{-| Retrieve a Node siblings identified by its datum in a Tree.
-}
siblings : a -> Node a -> List a
siblings target tree =
    case parent target tree of
        Just (Node _ children) ->
            children
                |> List.filter (\node -> datum node /= target)
                |> List.map datum

        Nothing ->
            []


{-| Turn a tree of node into a list of tuples.
-}
toList : Node a -> List ( a, Maybe a )
toList node =
    node |> flatMap (tuple node)



-- Update


{-| Replace a Node in a Tree.
-}
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
