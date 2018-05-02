module Canopy
    exposing
        ( Node(..)
        , append
        , value
        , decode
        , encode
        , filter
        , get
        , flatMap
        , flatten
        , fromList
        , leaf
        , leaves
        , map
        , node
        , parent
        , path
        , prepend
        , remove
        , replaceNode
        , replaceValue
        , seek
        , siblings
        , toList
        , tuple
        , updateChildren
        , updateValue
        )

{-| A generic [Rose Tree](https://en.wikipedia.org/wiki/Rose_tree).

TODO:

  - levels?
  - code examples for public API
  - deal with non-unique nodes resiliently:
      - remove all nodes matching the provided value
      - append/prepend a value to each nodes matching the provided value
      - replace all nodes matching the provided value
      - get -> first match from left
      - parent -> first match from left
      - siblings -> first match from left


# Basics

@docs Node


# Building and manipulating a Tree

@docs node, leaf, append, prepend, remove, updateChildren, updateValue


# Manipulating a Tree

@docs replaceNode, replaceValue, filter, flatMap, flatten, map, tuple


# Querying a Tree

@docs value, get, leaves, parent, path, seek, siblings


# Importing and exporting

@docs decode, encode, fromList, toList

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


{-| A [Rose Tree](https://en.wikipedia.org/wiki/Rose_tree).
-}
type Node a
    = Node a (List (Node a))


{-| Append a new value to a Node identified by its value in a Tree.

    node "foo" [ leaf "bar"]
        |> append "foo" "baz"
        == node "foo" [ leaf "bar", leaf "qux" ]

-}
append : a -> a -> Node a -> Node a
append target child node =
    if target == value node then
        node |> updateChildren (children node ++ [ leaf child ])
    else
        node |> updateChildren (node |> children |> List.map (append target child))


{-| Extracts the children of a Node.

    node "foo" [ leaf "bar" ]
        |> children
        == [ leaf "bar" ]

-}
children : Node a -> List (Node a)
children (Node _ children) =
    children


{-| Decode a Node. You must specify a value decoder.

    import Json.Decode exposing (decodeString)

    case decodeString (decodeNode Decode.string) node of
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


{-| Encode a Node. You must provide an encoder for the value type.

    import Json.Encode as Encode

        node "foo" [ leaf "bar" ]
            |> encode Encode.string
            |> Encode.encode 0
            -- {"value":"foo","children":[{"value":"bar",children:[]}]}

-}
encode : (a -> Encode.Value) -> Node a -> Encode.Value
encode valueEncoder (Node value children) =
    Encode.object
        [ ( "value", valueEncoder value )
        , ( "children", children |> List.map (encode valueEncoder) |> Encode.list )
        ]


{-| Filter a Tree, keeping only nodes which attached value satisfies the
provided test and their ancestors, up to the tree root.

    node 1 [ node 2 [ leaf 3, node 4 [ leaf 5, leaf 6 ] ]]

-}
filter : (a -> Bool) -> Node a -> Node a
filter test tree =
    let
        toDelete =
            tree |> seek (not << test)

        toPreserve =
            tree |> seek test |> List.map (\value -> path value tree) |> List.concat
    in
        toDelete
            |> List.filter (\value -> List.member value toPreserve |> not)
            |> List.foldl remove tree


{-| Map each node using a mapping function then flatten the result into a new list.
-}
flatMap : (Node a -> b) -> Node a -> List b
flatMap mapper tree =
    List.foldl
        (\node acc ->
            acc
                ++ [ mapper node ]
                ++ (node |> children |> List.map (flatMap mapper) |> List.concat)
        )
        [ mapper tree ]
        (children tree)


{-| Flatten a Tree, from top to bottom and leftmost nodes to rightmost ones.

    node "root" [ node "foo" [ leaf "bar", leaf "baz" ] ]
        |> flatten
        == [ "root", "foo", "bar", "baz" ]

-}
flatten : Node a -> List (Node a)
flatten node =
    node |> flatMap identity


{-| Build a tree from a list of hierarchy descriptors, which are tuples of value
and parent value, starting with the root.

    [ ( "root", Nothing )
    , ( "foo", Just "root" )
    , ( "bar", Just "foo" )
    ]
        |> fromList
        == node "foo" [ node "foo" [ leaf "bar" ] ]

-}
fromList : List ( a, Maybe a ) -> Maybe (Node a)
fromList nodes =
    case List.head nodes of
        Just ( root, Nothing ) ->
            nodes
                |> List.foldl
                    (\( value, maybeParent ) acc ->
                        case maybeParent of
                            Just parent ->
                                acc |> append parent value

                            Nothing ->
                                acc
                    )
                    (leaf root)
                |> Just

        _ ->
            Nothing


{-| Get a Node from a tree of Nodes, identified by its value.
-}
get : a -> Node a -> Maybe (Node a)
get target node =
    if target == value node then
        Just node
    else
        node
            |> children
            |> List.map (get target)
            |> List.filter ((/=) Nothing)
            |> List.head
            |> Maybe.withDefault Nothing


{-| Create a node having no children (singleton).
-}
leaf : a -> Node a
leaf value =
    Node value []


{-| Retrieve all leaves (singletons) from a tree.

    node "root"
        [ leaf "a leaf"
        , node "branch"
            [ leaf "another leaf" ]
        ]
        |> leaves
        == [ "a leaf", "another leaf" ]

-}
leaves : Node a -> List a
leaves tree =
    tree
        |> flatten
        |> List.filter (\node -> children node == [])
        |> List.map value


{-| Map all nodes data in a Tree.

    node "root" [ leaf "foo", node "bar" [ leaf "baz" ] ]
        |> map String.toUpper
        == node "ROOT" [ leaf "FOO", node "BAR" [ leaf "BAZ" ] ]

-}
map : (a -> b) -> Node a -> Node b
map mapper (Node value children) =
    Node (mapper value) (children |> List.map (map mapper))


{-| Create a Node.
-}
node : a -> List (Node a) -> Node a
node value children =
    Node value children


{-| Retrieve the parent of a given node in a Tree, identified by its value.
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
                        if value node == target then
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
                    path__ (value parentNode) rootNode ++ [ value parentNode ]

                Nothing ->
                    []
    in
        case parent target rootNode of
            Nothing ->
                if target == value rootNode then
                    [ target ]
                else
                    []

            Just rootNode ->
                path__ target rootNode ++ [ target ]


{-| Prepend a new value to a Node identified by its value in a Tree.

    node "foo" [ leaf "bar"]
        |> prepend "foo" "baz"
        == node "foo" [ leaf "baz", leaf "bar" ]

-}
prepend : a -> a -> Node a -> Node a
prepend target child node =
    if target == value node then
        node |> updateChildren (leaf child :: children node)
    else
        updateChildren (node |> children |> List.map (prepend target child)) node


{-| Deletes a Node from a tree, referenced by its attached value.

Noop when the target doesn't exist in the tree or when attempting to delete the
tree itself.

-}
remove : a -> Node a -> Node a
remove target tree =
    case tree |> parent target of
        Just parentNode ->
            let
                newChildren =
                    parentNode |> children |> List.filter (\node -> value node /= target)

                newParent =
                    parentNode |> updateChildren newChildren
            in
                tree |> replaceNode (value parentNode) newParent

        Nothing ->
            tree


{-| Replace a Node in a Tree.
-}
replaceNode : a -> Node a -> Node a -> Node a
replaceNode target replacement root =
    if value root == target then
        replacement
    else
        let
            newChildren =
                root |> children |> List.map (replaceNode target replacement)
        in
            root |> updateChildren newChildren


{-| Replace a Node value in a Tree.
-}
replaceValue : a -> a -> Node a -> Node a
replaceValue target replacement root =
    case get target root of
        Just node ->
            root |> replaceNode target (updateValue replacement node)

        Nothing ->
            root


{-| Retrieve all data from nodes containing a value satisfying a provided condition.
-}
seek : (a -> Bool) -> Node a -> List a
seek test node =
    node
        |> flatMap identity
        |> List.filter (value >> test)
        |> List.map value


{-| Retrieve a Node siblings identified by its value in a Tree.
-}
siblings : a -> Node a -> List a
siblings target tree =
    case parent target tree of
        Just (Node _ children) ->
            children
                |> List.filter (\node -> value node /= target)
                |> List.map value

        Nothing ->
            []


{-| Turn a tree of node into a list of tuples.
-}
toList : Node a -> List ( a, Maybe a )
toList node =
    node |> flatMap (tuple node)


{-| Turn a Node into a tuple containing the value and the parent value, if any.
-}
tuple : Node a -> Node a -> ( a, Maybe a )
tuple root node =
    ( value node, root |> parent (value node) |> Maybe.map value )


{-| Update a Node's children.
-}
updateChildren : List (Node a) -> Node a -> Node a
updateChildren children (Node value _) =
    Node value children


{-| Update a Node value.
-}
updateValue : a -> Node a -> Node a
updateValue value (Node _ children) =
    Node value children


{-| Extracts the value of a Node.
-}
value : Node a -> a
value (Node value _) =
    value
