module Canopy
    exposing
        ( Node(..)
        , append
        , children
        , count
        , decode
        , encode
        , filter
        , get
        , flatMap
        , flatten
        , foldl
        , foldr
        , fromList
        , leaf
        , leaves
        , level
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
        , value
        )

{-| A generic [Rose Tree](https://en.wikipedia.org/wiki/Rose_tree).

TODO:

  - move
  - appendNode, append -> appendValue?
      - more generally, distinguish between working with nodes & values


# Basics

@docs Node


# Building and manipulating a Tree

@docs node, leaf, append, prepend, remove, updateChildren, updateValue


# Manipulating a Tree

@docs replaceNode, replaceValue, filter, flatMap, flatten, foldl, foldr, map, tuple


# Querying a Tree

@docs value, children, count, get, leaves, level, parent, path, seek, siblings


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
    --> node "foo" [ leaf "bar", leaf "baz" ]

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
    --> [ leaf "bar" ]

-}
children : Node a -> List (Node a)
children (Node _ children) =
    children


{-| Count nodes in a tree.

    node 1 [node 2 [ node 3 [ leaf 4 ] ] ]
    |> count
    --> 4

-}
count : Node a -> Int
count node =
    foldl (\_ x -> x + 1) 0 node


{-| Decode a Node. You must specify a value decoder.

    import Json.Decode as Decode

    json : String
    json = "{\"value\":\"foo\",\"children\":[{\"value\":\"bar\",\"children\":[]}]}"

    Decode.decodeString (decode Decode.string) json
    --> Ok (node "foo" [ leaf "bar" ])

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
    --> "{\"value\":\"foo\",\"children\":[{\"value\":\"bar\",\"children\":[]}]}"

-}
encode : (a -> Encode.Value) -> Node a -> Encode.Value
encode valueEncoder (Node value children) =
    Encode.object
        [ ( "value", valueEncoder value )
        , ( "children", children |> List.map (encode valueEncoder) |> Encode.list )
        ]


{-| Filter a Tree, keeping only nodes which attached value satisfies the
provided test and their ancestors, up to the tree root.

    node 1
        [ node 2
            [ leaf 3
            , node 4
                [ leaf 5
                , leaf 6
                , leaf 7
                , leaf 8
                ]
            ]
        ]
        |> filter (\x -> x % 2 == 0)
    --> node 1
            [ node 2
                [ node 4
                    [ leaf 6
                    , leaf 8
                    ]
                ]
            ]

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

    node "foo" [ node "bar" [ leaf "baz" ] ]
        |> flatMap (value >> String.toUpper)
    --> [ "FOO", "BAR", "BAZ" ]

-}
flatMap : (Node a -> b) -> Node a -> List b
flatMap mapper tree =
    List.foldl
        (\node acc ->
            acc
                ++ mapper node
                :: (children node |> List.map (flatMap mapper) |> List.concat)
        )
        [ mapper tree ]
        (children tree)


{-| Flatten a Tree, from top to bottom and leftmost nodes to rightmost ones.

    node "root" [ node "foo" [ leaf "bar", leaf "baz" ] ]
        |> flatten
    --> [ node "root" [ node "foo" [ leaf "bar", leaf "baz" ] ]
        , node "foo" [ leaf "bar", leaf "baz" ]
        , leaf "bar"
        , leaf "baz"
        ]

-}
flatten : Node a -> List (Node a)
flatten node =
    node |> flatMap identity


{-| Reduce all tree values from top to bottom, left to right.

    node 1 [ node 2 [ leaf 3 ] ]
        |> foldl (+) 0
    --> 6

    node "a"
        [ node "b" [ leaf "c" ]
        , node "d" [ node "e" [ leaf "f" ] ]
        , leaf "g"
        ]
        |> foldl (\value acc -> acc ++ value) ""
    --> "abcdefg"

-}
foldl : (a -> b -> b) -> b -> Node a -> b
foldl fn acc node =
    node
        |> toList
        |> List.map Tuple.first
        |> List.foldl fn acc


{-| Reduce all tree values from top to bottom, right to left.

    node "a"
        [ node "b" [ leaf "c" ]
        , node "d" [ node "e" [ leaf "f" ] ]
        , leaf "g"
        ]
        |> foldr (\value acc -> acc ++ value) ""
    --> "gfedcba"

-}
foldr : (a -> b -> b) -> b -> Node a -> b
foldr fn acc node =
    node
        |> toList
        |> List.map Tuple.first
        |> List.foldr fn acc


{-| Build a tree from a list of hierarchy descriptors, which are tuples of value
and parent value, starting with the root.

    [ ( "root", Nothing )
    , ( "foo", Just "root" )
    , ( "bar", Just "foo" )
    ]
        |> fromList
    --> Just (node "root" [ node "foo" [ leaf "bar" ] ])

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

    node "root" [ leaf "bar" ]
        |> get "bar"
    --> Just (leaf "bar")

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

    leaf "foo"
    --> Node "foo" []

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
    --> [ "a leaf", "another leaf" ]

-}
leaves : Node a -> List a
leaves tree =
    tree
        |> flatten
        |> List.filter (\node -> children node == [])
        |> List.map value


{-| Retrieve all nodes at a given level in the tree.

    node "root"
        [ node "1" [ node "1.1" [ leaf "1.1.1" ] ]
        , node "2" [ node "2.1" [ leaf "2.1.1" ] ]
        ]
        |> level 3

    node "root"
        [ node "1" [ node "1.1" [ leaf "1.1.1" ] ]
        , node "2" [ node "2.1" [ leaf "2.1.1" ] ]
        ]
        |> level 2
    --> [ node "1.1" [ leaf "1.1.1" ], node "2.1" [ leaf "2.1.1" ] ]

-}
level : Int -> Node a -> List (Node a)
level lvl node =
    if lvl <= 0 then
        [ node ]
    else
        node |> children |> List.map (level (lvl - 1)) |> List.concat


{-| Map all node values in a Tree.

    node "root" [ leaf "foo", node "bar" [ leaf "baz" ] ]
        |> map String.toUpper
    --> node "ROOT" [ leaf "FOO", node "BAR" [ leaf "BAZ" ] ]

-}
map : (a -> b) -> Node a -> Node b
map mapper (Node value children) =
    Node (mapper value) (children |> List.map (map mapper))


{-| Create a Node. Basically just an alias for the `Node` constructor.
-}
node : a -> List (Node a) -> Node a
node value children =
    Node value children


{-| Retrieve the parent of a given node in a Tree, identified by its value.

    node "root" [ node "foo" [ leaf "bar" ] ]
        |> parent "bar"
    --> Just (node "foo" [ leaf "bar" ])

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

    node "root" [ node "foo" [ node "bar" [ leaf "baz" ] ] ]
        |> path "baz"
    --> [ "root", "foo", "bar", "baz" ]

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
    --> node "foo" [ leaf "baz", leaf "bar" ]

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

    node "root" [ node "foo" [ leaf "bar" ] ]
        |> remove "bar"
    --> node "root" [ leaf "foo" ]

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

    node "root" [ node "foo" [ leaf "bar" ] ]
        |> replaceNode "foo" (leaf "bar")
        == node "root" [ leaf "bar" ]

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

    node "root" [ node "foo" [ leaf "bar" ] ]
        |> replaceValue "foo" "baz"
    --> node "root" [ node "baz" [ leaf "bar" ] ]

-}
replaceValue : a -> a -> Node a -> Node a
replaceValue target replacement root =
    case get target root of
        Just node ->
            root |> replaceNode target (updateValue replacement node)

        Nothing ->
            root


{-| Retrieve all data from nodes containing a value satisfying a provided condition.

    node 1 [ node 2 [ leaf 3, leaf 4, leaf 5 ] ]
        |> seek (\x -> x > 3)
    --> [ 4, 5 ]

-}
seek : (a -> Bool) -> Node a -> List a
seek test node =
    node
        |> flatten
        |> List.filter (value >> test)
        |> List.map value


{-| Retrieve siblings of a Node identified by its value in a Tree.

    node "foo" [ leaf "a", node "b" [ leaf "x" ], leaf "c" ]
        |> siblings "c"
    --> [ "a", "b" ]

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

    node "root" [ node "foo" [ leaf "bar" ], leaf "baz" ]
        |> toList
    --> [ ( "root", Nothing )
        , ( "foo", Just "root")
        , ( "bar", Just "foo")
        , ( "baz", Just "root")
        ]

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
