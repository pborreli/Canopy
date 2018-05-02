module Canopy
    exposing
        ( Tree(..)
        , appendChild
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
        )

{-| A generic Tree, which nodes are designed to hold a value unique to the whole tree.

TODO:

  - rename datum to value
  - expose underscore functions for nodes
  - write a test for:
    tree "foo" |> appendChild "foo" "bar"
  - we could also add checks to ensure any added datum is unique to the tree (using a Set for instance)
      - though would end in using Result much everywhere
  - reorg docs, code and tests using the same segmentation
  - do we actually need Tree? I think not
  - deal with non-unique nodes resiliently:
      - delete all nodes matching the provided datum
      - append/prepend a value to each nodes matching the provided datum
      - replace all nodes matching the provided datum
      - get -> first match from left
      - parent -> first match from left
      - siblings -> first match from left


# Basics

@docs Tree


# Building and manipulating a Tree

@docs tree, seeded, empty, leaf, appendChild, prependChild, deleteNode


# Manipulating a Tree

@docs replace, filter, flatMap, flatten, map, tuple, rootMap


# Querying a Tree

@docs get, parent, path, root, seek, siblings


# Importing and exporting

@docs decode, encode

-}

import Canopy.Node as Node exposing (Node(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


{-| A generic tree, that can either be `Empty` or `Seeded` with a root Node.
-}
type Tree a
    = Empty
    | Seeded (Node a)



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
        , Decode.map Seeded (Node.decode decodeDatum)
        ]



-- Encode


{-| Encode a Tree. You must provide an encoder for the datum type.

    import Json.Encode as Encode

    tree "foo" [ leaf "bar" ]
        |> encode Encode.string
        |> Encode.encode 0
        -- {"value":"foo","children":[{"value":"bar",children:[]}]}

-}
encode : (a -> Encode.Value) -> Tree a -> Encode.Value
encode datumEncoder tree =
    tree |> rootMap_ (Node.encode datumEncoder) (Encode.object [])



-- Getters


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
    ( Node.datum node, tree |> parent (Node.datum node) |> Maybe.map Node.datum )



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
            if target == Node.datum node then
                node |> Node.updateChildren (Node.children node ++ [ leaf child ])
            else
                Node.updateChildren (node |> Node.children |> List.map appendChild_) node
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
                    parentNode |> Node.children |> List.filter (\node -> Node.datum node /= target)

                newParent =
                    parentNode |> Node.updateChildren newChildren
            in
                tree |> replace (Node.datum parentNode) newParent

        Nothing ->
            if (tree |> root |> Maybe.map Node.datum) == Just target then
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


{-| Map each node using a mapping function then flatten the result into a new list.
-}
flatMap : (Node a -> b) -> Tree a -> List b
flatMap mapper tree =
    tree |> rootMap_ (Node.flatMap mapper) []


{-| Flatten a Tree.
-}
flatten : Tree a -> List (Node a)
flatten tree =
    tree |> flatMap identity


{-| Find a Node in a Tree, identified by its datum.
-}
get : a -> Tree a -> Maybe (Node a)
get target tree =
    tree |> rootMap_ (Node.get target) Nothing


{-| Create a Tree leaf node, a.k.a singleton.
-}
leaf : a -> Node a
leaf datum =
    Node datum []


{-| Map all nodes data in a Tree.
-}
map : (a -> b) -> Tree a -> Tree b
map mapper tree =
    tree |> rootMap (Node.map mapper)


{-| Retrieve the parent of a given node in a Tree, by its Id.
-}
parent : a -> Tree a -> Maybe (Node a)
parent target tree =
    tree |> rootMap_ (Node.parent target) Nothing


{-| Compute the path to a Node from the root. Returns an empty list when
the target Node doesn't exist in the tree.
-}
path : a -> Tree a -> List a
path target tree =
    tree |> rootMap_ (Node.path target) []


{-| Prepend a new datum to a Node identified by its datum in a Tree.

    tree "foo" [ leaf "bar"]
        |> prependChild "foo" "baz"
        == tree "foo" [ leaf "baz", leaf "bar" ]

-}
prependChild : a -> a -> Tree a -> Tree a
prependChild target child tree =
    let
        prependChild_ node =
            if target == Node.datum node then
                node |> Node.updateChildren (leaf child :: Node.children node)
            else
                Node.updateChildren (node |> Node.children |> List.map prependChild_) node
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


{-| Retrieve all Ids from nodes containing a datum satisfying a provided condition.
-}
seek : (a -> Bool) -> Tree a -> List a
seek test tree =
    tree |> rootMap_ (Node.seek test) []


{-| Retrieve a Node siblings identified by its datum in a Tree.
-}
siblings : a -> Tree a -> List a
siblings target tree =
    case parent target tree of
        Just (Node _ children) ->
            children
                |> List.filter (\node -> Node.datum node /= target)
                |> List.map Node.datum

        Nothing ->
            []


{-| Create a seeded tree.
-}
tree : a -> List (Node a) -> Tree a
tree datum children =
    Seeded (Node datum children)



-- Update


{-| Replace a Node in a Tree.
-}
replace : a -> Node a -> Tree a -> Tree a
replace target replacement tree =
    tree |> rootMap (Node.replace target replacement)
