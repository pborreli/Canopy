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
        , tuple
        , updateChildren
        , updateDatum
        )

{-| A representation of a Classification Tree.


# Basics

@docs Tree, Node, Id


# Building and manipulating a tree

@docs createTree, createNode, appendChild, deleteNode


# Manipulating a tree

@docs replace, filter, flatten, map, tuple, rootMap, updateChildren, updateDatum


# Querying a tree

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


{-| A node id.

**Note:** You should never rely on Id as a business identifier of attached
generics. Rather store your business ids within each datum.

-}
type Id
    = Id Int


{-| A tree.
-}
type Tree a
    = Empty
    | Seeded (Node a)


{-| A tree node.
-}
type Node a
    = Node Id a (List (Node a))



-- Decode


{-| Decode a tree. You must specify a datum decoder.
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


{-| A tree JSON encoder. You must provide an encoder for the datum type.
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


{-| Extracts children from a node.
-}
children : Node a -> List (Node a)
children (Node _ _ children) =
    children


{-| Extracts a datum from a node.
-}
datum : Node a -> a
datum (Node _ datum _) =
    datum


{-| Extract a node's id.
-}
id : Node a -> Id
id (Node id _ _) =
    id


{-| Extract the integer value of an Id.
-}
idint : Id -> Int
idint (Id int) =
    int


{-| Retrieve the root node of a tree.
-}
root : Tree a -> Maybe (Node a)
root tree =
    case tree of
        Empty ->
            Nothing

        Seeded root ->
            Just root


{-| Turn a node into a tuple containing the id, the datum and the children.
-}
tuple : Node a -> ( Id, a )
tuple node =
    ( id node, datum node )



-- Manipulation


{-| Append a new child holding a datum to a node identified by its id in a tree.
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


{-| Create a new node to be addable to a given tree.

A new unique identifier is generated for this node, computed against the tree.

-}
createNode : a -> Tree a -> Node a
createNode value tree =
    Node (nextId tree) value []


{-| Create a tree, with a root node having the provided datum attached. The
created root node always has `(Id 0)`.
-}
createTree : a -> Tree a
createTree datum =
    Seeded (Node (Id 0) datum [])


{-| Deletes a node from a tree, by its id. Will silently refuse to delete a node:

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


{-| Filter a tree.
-}
filter : (a -> Bool) -> Tree a -> Tree a
filter test tree =
    tree
        |> seek (not << test)
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


{-| Find a node in a tree by its id.
-}
findNode : Id -> Tree a -> Maybe (Node a)
findNode target tree =
    tree |> root |> Maybe.map (findNode_ target) |> Maybe.withDefault Nothing


{-| Find nodes in a tree, by its id.
-}
findNodes : List Id -> Tree a -> List (Maybe (Node a))
findNodes ids tree =
    ids |> List.map (\id -> findNode id tree)


flatten_ : Node a -> List ( Id, a )
flatten_ node =
    node
        |> children
        |> List.foldl
            (\node acc ->
                List.concat
                    [ acc
                    , [ tuple node ]
                    , node |> children |> List.map tuple
                    ]
            )
            [ tuple node ]


{-| Flatten a tree.
-}
flatten : Tree a -> List ( Id, a )
flatten tree =
    tree |> root |> Maybe.map flatten_ |> Maybe.withDefault []


map_ : (a -> b) -> Node a -> Node b
map_ mapper (Node id datum children) =
    Node id (mapper datum) (children |> List.map (map_ mapper))


{-| Map all nodes data in a tree.
-}
map : (a -> b) -> Tree a -> Tree b
map mapper tree =
    tree |> rootMap (map_ mapper)


{-| Computes the next available unique id for a tree.
-}
nextId : Tree a -> Id
nextId tree =
    tree
        |> flatten
        |> List.map (Tuple.first >> idint)
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


{-| Retrieve the parent of a given node in a tree, by its id.
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


{-| Compute the path to access a node from the root. Returns an empty list when
the target node doesn't exist in the tree.
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
seek_ test (Node id datum children) =
    List.concat
        [ if test datum then
            [ id ]
          else
            []
        , children |> List.map (seek_ test) |> List.concat
        ]


{-| Retrieve all ids from nodes containing a datum satisfying a provided condition.
-}
seek : (a -> Bool) -> Tree a -> List Id
seek test tree =
    tree |> root |> Maybe.map (seek_ test) |> Maybe.withDefault []


{-| Retrieve a node siblings identified by its id in a tree.
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


{-| Replace a node in a tree.
-}
replace : Id -> Node a -> Tree a -> Tree a
replace target node tree =
    tree |> rootMap (replace_ target node)


{-| Update a node's children.
-}
updateChildren : List (Node a) -> Node a -> Node a
updateChildren children (Node id datum _) =
    Node id datum children


{-| Update a node's datum.
-}
updateDatum : a -> Node a -> Node a
updateDatum datum (Node id _ children) =
    Node id datum children
