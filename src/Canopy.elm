module Canopy
    exposing
        ( Node(..)
        , Tree(..)
        , appendChild
        , children
        , datum
        , decode
        , deleteNode
        , empty
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
        , replace
        , root
        , rootMap
        , seek
        , siblings
        , seeded
        , tree
        , tuple
        , updateChildren
        , updateDatum
        )

{-| A generic Tree.

TODO:

  - rename datum to value
  - expose underscore functions for nodes
  - write a test for:
    tree "foo" |> appendChild "foo" "bar"
  - we could also add checks to ensure any added datum is unique to the tree (using a Set for instance)
      - though would end in using Result much everywhere
  - reorg docs, code and tests using the same segmentation


# Basics

@docs Tree, Node


# Building and manipulating a Tree

@docs tree, seeded, empty, node, leaf, appendChild, prependChild, deleteNode


# Manipulating a Tree

@docs replace, filter, flatMap, flatten, map, tuple, rootMap, updateChildren, updateDatum


# Querying a Tree

@docs children, datum, get, parent, path, root, seek, siblings


# Importing and exporting

@docs decode, encode

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


{-| A generic tree, that can either be `Empty` or `Seeded` with a root Node.
-}
type Tree a
    = Empty
    | Seeded (Node a)


{-| A tree node, basically a [Rose Tree](https://en.wikipedia.org/wiki/Rose_tree).
-}
type Node a
    = Node a (List (Node a))



-- Decode


{-| Decode a Tree. You must specify a datum decoder.

    case Decode.decodeString (decode Decode.string) tree of
        Ok decoded ->
            ...
        Err _ ->
            ...

-}
decode : Decoder a -> Decoder (Tree a)
decode decodeDatum =
    Decode.oneOf
        [ Decode.null Empty
        , Decode.map Seeded (decodeNode decodeDatum)
        ]


decodeNode : Decoder a -> Decoder (Node a)
decodeNode decodeDatum =
    Decode.map2 Node
        (Decode.field "value" decodeDatum)
        (Decode.field "children" (Decode.list (Decode.lazy (\_ -> decodeNode decodeDatum))))



-- Encode


{-| Encode a Tree. You must provide an encoder for the datum type.

    import Json.Encode as Encode

    encodeTree : Tree String -> String
    encodeTree tree =
        tree
            |> encode Encode.string
            |> Encode.encode 2

-}
encode : (a -> Encode.Value) -> Tree a -> Encode.Value
encode datumEncoder tree =
    let
        encodeNode (Node datum children) =
            Encode.object
                [ ( "value", datumEncoder datum )
                , ( "children", children |> List.map encodeNode |> Encode.list )
                ]
    in
        tree |> rootMap_ encodeNode (Encode.object [])



-- Getters


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


{-| Retrieve the root Node of a Tree, if it's not empty.

    root (tree "foo") == Node (Id 0) "foo" []
    root (Empty) == Nothing

-}
root : Tree a -> Maybe (Node a)
root tree =
    case tree of
        Empty ->
            Nothing

        Seeded root ->
            Just root


{-| Turn a Node into a tuple containing the datum and the parent datum, if any.
-}
tuple : Tree a -> Node a -> ( a, Maybe a )
tuple tree node =
    ( datum node, tree |> parent (datum node) |> Maybe.map datum )



-- Manipulation


{-| Append a new datum to a Node identified by its datum in a Tree.

    tree "foo" [ leaf "bar"]
        |> appendChild "foo" "baz"
        == tree "foo" [ leaf "bar", leaf "qux" ]

-}
appendChild : a -> a -> Tree a -> Tree a
appendChild target child tree =
    let
        appendChild_ node =
            case node |> get_ target of
                Just node ->
                    if target == datum node then
                        node |> updateChildren (children node ++ [ leaf child ])
                    else
                        updateChildren (node |> children |> List.map appendChild_) node

                Nothing ->
                    node
    in
        tree |> rootMap appendChild_


{-| Deletes a Node from a Tree, referenced by its attached datum. Will silently
refuse to delete a Node if it doesn't exist in the Tree.
-}
deleteNode : a -> Tree a -> Tree a
deleteNode target tree =
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
            if (tree |> root |> Maybe.map datum) == Just target then
                Empty
            else
                tree


{-| Create an empty tree.
-}
empty : Tree a
empty =
    Empty


{-| Filter a Tree, keeping only nodes which attached datum satisfies the
provided test, and their ancestors up to the tree root.
-}
filter : (a -> Bool) -> Tree a -> Tree a
filter test tree =
    let
        toDelete =
            tree |> seek (not << test)

        toPreserve =
            tree |> seek test |> List.map (\id -> path id tree) |> List.concat
    in
        toDelete
            |> List.filter (\id -> List.member id toPreserve |> not)
            |> List.foldl deleteNode tree


flatMap_ : (Node a -> b) -> Node a -> List b
flatMap_ mapper node =
    node
        |> children
        |> List.foldl
            (\node acc -> List.concat [ acc, [ mapper node ], node |> children |> List.map mapper ])
            [ mapper node ]


{-| Map each node using a mapping function then flatten the result into a new list.
-}
flatMap : (Node a -> b) -> Tree a -> List b
flatMap mapper tree =
    tree |> rootMap_ (flatMap_ mapper) []


{-| Flatten a Tree.
-}
flatten : Tree a -> List (Node a)
flatten tree =
    tree |> flatMap identity


get_ : a -> Node a -> Maybe (Node a)
get_ target node =
    if target == datum node then
        Just node
    else
        node
            |> children
            |> List.map (get_ target)
            |> List.filter ((/=) Nothing)
            |> List.head
            |> Maybe.withDefault Nothing


{-| Find a Node in a Tree, identified by its datum.
-}
get : a -> Tree a -> Maybe (Node a)
get target tree =
    tree |> rootMap_ (get_ target) Nothing


{-| Create a Tree leaf node, a.k.a singleton.
-}
leaf : a -> Node a
leaf datum =
    Node datum []


map_ : (a -> b) -> Node a -> Node b
map_ mapper (Node datum children) =
    Node (mapper datum) (children |> List.map (map_ mapper))


{-| Map all nodes data in a Tree.
-}
map : (a -> b) -> Tree a -> Tree b
map mapper tree =
    tree |> rootMap (map_ mapper)


{-| Create a Node.
-}
node : a -> List (Node a) -> Node a
node datum children =
    Node datum children


parent_ : a -> Node a -> Maybe (Node a)
parent_ target candidate =
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
                            parent_ target node
            )
            Nothing


{-| Retrieve the parent of a given node in a Tree, by its Id.
-}
parent : a -> Tree a -> Maybe (Node a)
parent target tree =
    tree |> rootMap_ (parent_ target) Nothing


path_ : a -> Node a -> List a
path_ target rootNode =
    let
        path__ target node =
            case parent_ target node of
                Just parentNode ->
                    path__ (datum parentNode) rootNode ++ [ datum parentNode ]

                Nothing ->
                    []
    in
        case parent_ target rootNode of
            Nothing ->
                if target == datum rootNode then
                    [ target ]
                else
                    []

            Just rootNode ->
                path__ target rootNode ++ [ target ]


{-| Compute the path to access a Node from the root. Returns an empty list when
the target Node doesn't exist in the tree.
-}
path : a -> Tree a -> List a
path target tree =
    tree |> rootMap_ (path_ target) []


{-| Prepend a new datum to a Node identified by its datum in a Tree.

    tree "foo" [ leaf "bar"]
        |> prependChild "foo" "baz"
        == tree "foo" [ leaf "baz", leaf "bar" ]

-}
prependChild : a -> a -> Tree a -> Tree a
prependChild target child tree =
    let
        prependChild_ node =
            case node |> get_ target of
                Just node ->
                    if target == datum node then
                        node |> updateChildren (leaf child :: children node)
                    else
                        updateChildren (node |> children |> List.map prependChild_) node

                Nothing ->
                    node
    in
        tree |> rootMap prependChild_


{-| Map the tree root node.
-}
rootMap : (Node a -> Node b) -> Tree a -> Tree b
rootMap mapper tree =
    tree |> rootMap_ (mapper >> Seeded) Empty


rootMap_ : (Node a -> b) -> b -> Tree a -> b
rootMap_ mapper default tree =
    tree |> root |> Maybe.map mapper |> Maybe.withDefault default


{-| Create a seeded Tree, with a root node having the provided datum attached.

    tree "root" == Seeded (leaf "root")

-}
seeded : a -> Tree a
seeded datum =
    Seeded (leaf datum)


seek_ : (a -> Bool) -> Node a -> List a
seek_ test node =
    node
        |> flatMap_ identity
        |> List.filter (datum >> test)
        |> List.map datum


{-| Retrieve all Ids from nodes containing a datum satisfying a provided condition.
-}
seek : (a -> Bool) -> Tree a -> List a
seek test tree =
    tree |> rootMap_ (seek_ test) []


{-| Retrieve a Node siblings identified by its datum in a Tree.
-}
siblings : a -> Tree a -> List a
siblings target tree =
    case parent target tree of
        Just (Node _ children) ->
            children
                |> List.filter (\node -> datum node /= target)
                |> List.map datum

        Nothing ->
            []


{-| Create a seeded tree.
-}
tree : a -> List (Node a) -> Tree a
tree datum children =
    Seeded (Node datum children)



-- Update


replace_ : a -> Node a -> Node a -> Node a
replace_ target replacement root =
    if datum root == target then
        replacement
    else
        let
            newChildren =
                root |> children |> List.map (replace_ target replacement)
        in
            root |> updateChildren newChildren


{-| Replace a Node in a Tree.
-}
replace : a -> Node a -> Tree a -> Tree a
replace target replacement tree =
    tree |> rootMap (replace_ target replacement)


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
