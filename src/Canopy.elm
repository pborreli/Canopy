module Canopy
    exposing
        ( Node(..)
        , all
        , any
        , append
        , children
        , decode
        , encode
        , filter
        , flatMap
        , flatten
        , foldl
        , foldr
        , fromList
        , get
        , getAll
        , leaf
        , leaves
        , length
        , level
        , map
        , mapChildren
        , maximum
        , minimum
        , member
        , node
        , parent
        , path
        , prepend
        , remove
        , refine
        , replaceNode
        , replaceValue
        , seed
        , seek
        , siblings
        , sortBy
        , sortWith
        , toList
        , tuple
        , updateChildren
        , updateValue
        , value
        , values
        )

{-| A generic [Rose Tree](https://en.wikipedia.org/wiki/Rose_tree).

@docs Node


# Building a Tree

@docs node, leaf, append, prepend, remove, seed


# Querying a Tree

@docs value, values, children, length, get, getAll, leaves, level, maximum, minimum, parent, path, seek, siblings


# Manipulating a Tree

@docs filter, flatMap, flatten, foldl, foldr, map, mapChildren, refine, replaceNode, replaceValue, sortBy, sortWith, updateChildren, updateValue, tuple


# Checking a Tree

@docs all, any, member


# Importing and exporting

@docs fromList, toList, decode, encode

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


{-| A [Rose Tree](https://en.wikipedia.org/wiki/Rose_tree).
-}
type Node a
    = Node a (List (Node a))


{-| Check that all values satisfy a test in a tree.

    node 1 [ leaf 2 ]
        |> all (\x -> x > 0)
    --> True

    node 1 [leaf -2]
        |> all (\x -> x > 0)
    --> False

-}
all : (a -> Bool) -> Node a -> Bool
all test node =
    node |> values |> List.all test


{-| Check that any value satisfy a test in a tree.

    node 1 [ leaf -2 ]
        |> any (\x -> x > 0)
    --> True

    node -1 [ leaf -2 ]
        |> any (\x -> x > 0)
    --> False

-}
any : (a -> Bool) -> Node a -> Bool
any test node =
    node |> values |> List.any test


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
        node |> mapChildren (append target child)


{-| Extracts the children of a Node.

    node "foo" [ leaf "bar" ]
        |> children
    --> [ leaf "bar" ]

-}
children : Node a -> List (Node a)
children (Node _ children) =
    children


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


{-| Filter a Tree strictly, removing all nodes failing the provided value test,
including root, hence the resulting Maybe.

    node 0 [ leaf 1 ]
        |> filter (\x -> x > 0)
    --> Nothing

    node 2 [ leaf 3, leaf 4 ]
        |> filter (\x -> x % 2 == 0)
    --> Just (node 2 [ leaf 4 ])

-}
filter : (a -> Bool) -> Node a -> Maybe (Node a)
filter test tree =
    if test (value tree) then
        let
            newChildren =
                tree
                    |> children
                    |> List.filter (value >> test)
                    |> List.filterMap (filter test)
        in
            tree |> updateChildren newChildren |> Just
    else
        Nothing


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
    --> , node "foo" [ leaf "bar", leaf "baz" ]
    --> , leaf "bar"
    --> , leaf "baz"
    --> ]

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
    node |> values |> List.foldl fn acc


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
    node |> values |> List.foldr fn acc


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
                        maybeParent
                            |> Maybe.map (\parent -> append parent value acc)
                            |> Maybe.withDefault acc
                    )
                    (leaf root)
                |> Just

        _ ->
            Nothing


{-| Get a Node holding a value from a tree, picking the first node found starting
from the left.

    node "root" [ leaf "bar" ]
        |> get "bar"
    --> Just (leaf "bar")

-}
get : a -> Node a -> Maybe (Node a)
get target node =
    node |> getAll target |> List.head


{-| Get all nodes containing the provided value.

    node 1 [ leaf 1 ]
        |> getAll 1
    --> [ node 1 [ leaf 1 ], leaf 1 ]

-}
getAll : a -> Node a -> List (Node a)
getAll target node =
    node |> flatten |> List.filter (value >> (==) target)


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
    tree |> flatten |> List.filter (children >> (==) []) |> List.map value


{-| Count nodes in a tree.

    node 1 [node 2 [ node 3 [ leaf 4 ] ] ]
    |> length
    --> 4

-}
length : Node a -> Int
length node =
    foldl (\_ x -> x + 1) 0 node


{-| Retrieve all nodes at a given level in the tree.

    node "root"
        [ node "1" [ node "1.1" [ leaf "1.1.1" ] ]
        , node "2" [ node "2.1" [ leaf "2.1.1" ] ]
        ]
        |> level 3
    --> [ leaf "1.1.1"
    --> , leaf "2.1.1"
    --> ]

    node "root"
        [ node "1" [ node "1.1" [ leaf "1.1.1" ] ]
        , node "2" [ node "2.1" [ leaf "2.1.1" ] ]
        ]
        |> level 2
    --> [ node "1.1" [ leaf "1.1.1" ]
    --> , node "2.1" [ leaf "2.1.1" ]
    --> ]

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


{-| Map a node's children.

    node "root" [ leaf "foo", leaf "bar" ]
        |> mapChildren (map String.toUpper)
    --> node "root" [ leaf "FOO", leaf "BAR" ]

-}
mapChildren : (Node a -> Node a) -> Node a -> Node a
mapChildren mapper (Node value children) =
    Node value (List.map mapper children)


{-| Compute the maximum value appearing in a tree.

    node 1 [ leaf 100, node 2 [ leaf 3 ] ]
        |> maximum
    --> 100

-}
maximum : Node comparable -> comparable
maximum node =
    node |> values |> List.maximum |> Maybe.withDefault (value node)


{-| Compute the minimum value appearing in a tree.

    node 100 [ leaf 99, node 1 [ leaf 98 ] ]
        |> minimum
    --> 1

-}
minimum : Node comparable -> comparable
minimum node =
    node |> values |> List.minimum |> Maybe.withDefault (value node)


{-| Check if a tree contains a value.

    node "foo" [ node "bar" [ leaf "baz" ] ]
        |> member "baz"
    --> True

    leaf "no"
        |> member "yes"
    --> False

-}
member : a -> Node a -> Bool
member target node =
    node |> get target |> Maybe.map (always True) |> Maybe.withDefault False


{-| Create a Node. Basically just an alias for the `Node` constructor.
-}
node : a -> List (Node a) -> Node a
node =
    Node


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
        node |> mapChildren (prepend target child)


{-| Deletes all occurences of a value from a tree.

    node "root" [ node "foo" [ leaf "bar" ] ]
        |> remove "bar"
    --> node "root" [ leaf "foo" ]

Noop when the value doesn't exist in the tree:

    node "root" [ leaf "foo" ]
        |> remove "non-existent"
    --> node "root" [ leaf "foo" ]

Or when attempting to delete the tree itself:

    leaf "root"
        |> remove "root"
    --> leaf "root"

-}
remove : a -> Node a -> Node a
remove target tree =
    tree |> filter ((/=) target) |> Maybe.withDefault tree


{-| Filter a Tree, keeping only nodes which attached leaves satisfy the
provided test and preserving their ancestors, up to the tree root, which is
always kept.

    node 2
        [ node 4
            [ leaf 6
            , leaf 7
            , node 8
                [ leaf 10
                , leaf 11
                , leaf 12
                , leaf 13
                ]
            ]
        ]
        |> refine (\x -> x % 2 == 0)
    --> node 2
    -->    [ node 4
    -->        [ leaf 6
    -->        , node 8
    -->            [ leaf 10
    -->            , leaf 12
    -->            ]
    -->        ]
    -->    ]

-}
refine : (a -> Bool) -> Node a -> Node a
refine test tree =
    let
        toDelete =
            tree |> seek (not << test)

        toPreserve =
            tree |> seek test |> List.map (\value -> path value tree) |> List.concat
    in
        toDelete
            |> List.filter (\value -> List.member value toPreserve |> not)
            |> List.foldl remove tree


{-| Replace a Node in a Tree, if it exists.

    node "root" [ node "foo" [ leaf "bar" ] ]
        |> replaceNode "foo" (leaf "bar")
    --> node "root" [ leaf "bar" ]

-}
replaceNode : a -> Node a -> Node a -> Node a
replaceNode target replacement root =
    if value root == target then
        replacement
    else
        root |> mapChildren (replaceNode target replacement)


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


{-| Retrieve all values from nodes containing those satisfying a provided condition.

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


{-| Seed a tree.

    seed (\x -> List.range 1 (x - 1)) 4
    --> node 4
    -->   [ leaf 1
    -->   , node 2 [ leaf 1 ]
    -->   , node 3
    -->       [ leaf 1
    -->       , node 2 [ leaf 1 ]
    -->       ]
    -->   ]

-}
seed : (a -> List a) -> a -> Node a
seed seeder init =
    Node init (seeder init |> List.map (seed seeder))


{-| Retrieve siblings of a node identified by its value in a Tree.

    node "foo" [ leaf "a", node "b" [ leaf "x" ], leaf "c" ]
        |> siblings "c"
    --> [ "a", "b" ]

-}
siblings : a -> Node a -> List a
siblings target tree =
    case parent target tree of
        Just (Node _ children) ->
            children |> List.filter (value >> (/=) target) |> List.map value

        Nothing ->
            []


{-| Recursively sort node children from a tree using a sorter.

    node 0 [ leaf 3, leaf 1, leaf 2 ]
        |> sortBy identity
    --> node 0 [ leaf 1, leaf 2, leaf 3 ]

-}
sortBy : (a -> comparable) -> Node a -> Node a
sortBy sorter (Node val children) =
    children
        |> List.sortBy (value >> sorter)
        |> List.map (sortBy sorter)
        |> Node val


{-| Recursively sort node children from a tree using a comparator.

    node 0 [ leaf 3, leaf 1, leaf 2 ]
        |> sortWith (\a b -> if a == b then EQ else if a < b then GT else LT)
    --> node 0 [ leaf 3, leaf 2, leaf 1 ]

-}
sortWith : (a -> a -> Order) -> Node a -> Node a
sortWith comparator (Node val children) =
    children
        |> List.sortWith (\a b -> comparator (value a) (value b))
        |> List.map (sortWith comparator)
        |> Node val


{-| Turn a tree of node into a list of tuples.

    node "root" [ node "foo" [ leaf "bar" ], leaf "baz" ]
        |> toList
    --> [ ( "root", Nothing )
    --> , ( "foo", Just "root")
    --> , ( "bar", Just "foo")
    --> , ( "baz", Just "root")
    --> ]

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


{-| List all the values in a tree.

    node 1 [ node 2 [ leaf 3 ] ]
        |> values
    --> [ 1, 2, 3 ]

-}
values : Node a -> List a
values node =
    node |> toList |> List.map Tuple.first
