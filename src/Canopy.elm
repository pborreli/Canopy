module Canopy
    exposing
        ( Id(..)
        , Node(..)
        , Tree(..)
        , appendChild
        , createNode
        , createTree
        , children
        , datum
        , decode
        , deleteNode
        , encode
        , filter
        , findNode
        , findNodes
        , flatMap
        , flatten
        , id
        , idint
        , map
        , nextId
        , parent
        , path
        , replace
        , root
        , rootMap
        , seek
        , siblings
        , triplet
        , tuple
        , updateChildren
        , updateDatum
        )

{-| A representation of a Classification Tree.


# Basics

@docs Tree, Node, Id


# Building and manipulating a Tree

@docs createTree, createNode, appendChild, deleteNode


# Manipulating a Tree

@docs replace, filter, flatMap, flatten, map, triplet, tuple, rootMap, updateChildren, updateDatum


# Querying a Tree

@docs id, idint, children, datum, findNode, findNodes, nextId, parent, path, root, seek, siblings


# Importing and exporting

@docs decode, encode

TODO:

  - if we have createNode, we should be able to attach it to the tree
  - separate tree and node in distinct modules?
  - appendChild should be for appending to a Node and return the created element, ala DOM api

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


{-| A node unique id.

Internally it's basicallky just an auto-incremented `Int`,
though you should never rely on it as a business identifier for attached
data. Rather store your business identifiers within each datum.

-}
type Id
    = Id Int


{-| A generic tree, that can either be `Empty` or `Seeded` with a root Node.
-}
type Tree a
    = Empty
    | Seeded (Node a)


{-| A tree node. Basically a [Rose Tree](https://en.wikipedia.org/wiki/Rose_tree)
though with the notion of unique identifier for a Node added; this allows easy
querying and ensures duplicate data are allowed to live within the tree.
-}
type Node a
    = Node Id a (List (Node a))



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
    Decode.map3 Node
        (Decode.field "id" (Decode.map Id Decode.int))
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
        encodeNode (Node (Id id) datum children) =
            Encode.object
                [ ( "id", Encode.int id )
                , ( "value", datumEncoder datum )
                , ( "children", children |> List.map encodeNode |> Encode.list )
                ]
    in
        tree |> root |> Maybe.map encodeNode |> Maybe.withDefault (Encode.object [])



-- Getters


{-| Extracts children from a Node.
-}
children : Node a -> List (Node a)
children (Node _ _ children) =
    children


{-| Extracts a datum from a Node.
-}
datum : Node a -> a
datum (Node _ datum _) =
    datum


{-| Extract a Node's Id.
-}
id : Node a -> Id
id (Node id _ _) =
    id


{-| Extract the integer value of an Id.

You should never rely on this identifier for business-related data; rather store
unique business identifiers in a Node's datum.

-}
idint : Id -> Int
idint (Id int) =
    int


{-| Retrieve the root Node of a Tree, if it's not empty.
-}
root : Tree a -> Maybe (Node a)
root tree =
    case tree of
        Empty ->
            Nothing

        Seeded root ->
            Just root


{-| Turn a Node into a triplet containing the Id, the datum and the parent Id (if any).
-}
triplet : Tree a -> Node a -> ( Id, a, Maybe Id )
triplet tree node =
    ( id node, datum node, tree |> parent (id node) |> Maybe.map id )


{-| Turn a Node into a tuple containing the Id and the datum.
-}
tuple : Node a -> ( Id, a )
tuple node =
    ( id node, datum node )



-- Manipulation


{-| Append a new child holding a datum to a Node identified by its Id in a Tree.
-}
appendChild : Id -> a -> Tree a -> Tree a
appendChild target datum tree =
    let
        appendChild_ node =
            if target == id node then
                node |> updateChildren (createNode datum tree :: children node)
            else
                updateChildren (node |> children |> List.map appendChild_) node
    in
        tree |> rootMap appendChild_


{-| Create a new Node to be addable to a given Tree.

A new unique identifier is generated for this node, computed against the tree.

-}
createNode : a -> Tree a -> Node a
createNode value tree =
    Node (nextId tree) value []


{-| Create a Tree, with a root node having the provided datum attached. The
created root node always has `(Id 0)`.
-}
createTree : a -> Tree a
createTree datum =
    Seeded (Node (Id 0) datum [])


{-| Deletes a Node from a Tree, by its Id. Will silently refuse to delete a Node:

  - if it's the tree root node
  - if the target node doesn't exist

-}
deleteNode : Id -> Tree a -> Tree a
deleteNode target tree =
    case parent target tree of
        Just parentNode ->
            let
                newChildren =
                    parentNode |> children |> List.filter (\node -> id node /= target)

                newParent =
                    parentNode |> updateChildren newChildren
            in
                tree |> replace (id parentNode) newParent

        Nothing ->
            tree


{-| Filter a Tree.
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


findNode_ : Id -> Node a -> Maybe (Node a)
findNode_ target node =
    if target == id node then
        Just node
    else
        node
            |> children
            |> List.map (findNode_ target)
            |> List.filter ((/=) Nothing)
            |> List.head
            |> Maybe.withDefault Nothing


{-| Find a Node in a Tree by its Id.
-}
findNode : Id -> Tree a -> Maybe (Node a)
findNode target tree =
    tree |> root |> Maybe.map (findNode_ target) |> Maybe.withDefault Nothing


{-| Find nodes in a Tree, by its Id.
-}
findNodes : List Id -> Tree a -> List (Maybe (Node a))
findNodes ids tree =
    ids |> List.map (\id -> findNode id tree)


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
    tree |> root |> Maybe.map (flatMap_ mapper) |> Maybe.withDefault []


{-| Flatten a Tree.
-}
flatten : Tree a -> List (Node a)
flatten tree =
    tree |> flatMap identity


map_ : (a -> b) -> Node a -> Node b
map_ mapper (Node id datum children) =
    Node id (mapper datum) (children |> List.map (map_ mapper))


{-| Map all nodes data in a Tree.
-}
map : (a -> b) -> Tree a -> Tree b
map mapper tree =
    tree |> rootMap (map_ mapper)


{-| Computes the next available unique id for a given tree.
-}
nextId : Tree a -> Id
nextId tree =
    tree
        |> flatten
        |> List.map (id >> idint)
        |> List.maximum
        |> Maybe.withDefault 0
        |> (+) 1
        |> Id


parent_ : Id -> Node a -> Maybe (Node a)
parent_ target candidate =
    candidate
        |> children
        |> List.foldl
            (\node acc ->
                case acc of
                    Just found ->
                        Just found

                    Nothing ->
                        if id node == target then
                            Just candidate
                        else
                            parent_ target node
            )
            Nothing


{-| Retrieve the parent of a given node in a Tree, by its Id.
-}
parent : Id -> Tree a -> Maybe (Node a)
parent target tree =
    tree |> root |> Maybe.map (parent_ target) |> Maybe.withDefault Nothing


path_ : Id -> Node a -> List Id
path_ target node =
    let
        rootNode =
            node

        path__ target node =
            case parent_ target node of
                Just parentNode ->
                    path__ (id parentNode) rootNode ++ [ id parentNode ]

                Nothing ->
                    []
    in
        path__ target rootNode ++ [ target ]


{-| Compute the path to access a Node from the root. Returns an empty list when
the target Node doesn't exist in the tree.
-}
path : Id -> Tree a -> List Id
path target tree =
    tree |> root |> Maybe.map (path_ target) |> Maybe.withDefault []


{-| Map the tree root node.
-}
rootMap : (Node a -> Node b) -> Tree a -> Tree b
rootMap mapper tree =
    tree |> root |> Maybe.map (mapper >> Seeded) |> Maybe.withDefault Empty


seek_ : (a -> Bool) -> Node a -> List Id
seek_ test node =
    node
        |> flatMap_ identity
        |> List.filter (datum >> test)
        |> List.map id


{-| Retrieve all Ids from nodes containing a datum satisfying a provided condition.
-}
seek : (a -> Bool) -> Tree a -> List Id
seek test tree =
    tree |> root |> Maybe.map (seek_ test) |> Maybe.withDefault []


{-| Retrieve a Node siblings identified by its Id in a Tree.
-}
siblings : Id -> Tree a -> List (Node a)
siblings target tree =
    case parent target tree of
        Just (Node _ _ children) ->
            children |> List.filter (\node -> id node /= target)

        Nothing ->
            []



-- Update


replace_ : Id -> Node a -> Node a -> Node a
replace_ target node root =
    if id root == target then
        node
    else
        let
            newChildren =
                root |> children |> List.map (replace_ target node)
        in
            root |> updateChildren newChildren


{-| Replace a Node in a Tree.
-}
replace : Id -> Node a -> Tree a -> Tree a
replace target node tree =
    tree |> rootMap (replace_ target node)


{-| Update a Node's children.
-}
updateChildren : List (Node a) -> Node a -> Node a
updateChildren children (Node id datum _) =
    Node id datum children


{-| Update a Node's datum.
-}
updateDatum : a -> Node a -> Node a
updateDatum datum (Node id _ children) =
    Node id datum children
