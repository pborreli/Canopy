module Canopy
    exposing
        ( Node(..)
        , Tree(..)
        , appendChild
        , createNode
        , children
        , datum
        , deleteNode
        , empty
        , encode
        , findNode
        , findNodes
        , parent
        , siblings
        , id
        , nextId
        , root
        )

{-| A representation of a Classification Tree.


# Basics

@docs Tree, Node


# Building a tree

@docs empty, demoTree, appendChild, createNode, deleteNode, toggleLock, updateLabel, updateShare


# Querying a tree

@docs findNode, findNodes, parent, siblings


# Normalizing a tree

@docs distributeShare, distributeQty, normalize


# Encoding

@docs encode

TODO:

  - generic typing
  - separate unique identifier
  - separate NodeInfo
  - distinguish between Tree and Node
  - move to own tree lib
  - create type NodeId

-}

import Json.Encode as Encode


{-| A tree.
-}
type Tree a
    = Tree (Node a)


{-| A tree node.
-}
type Node a
    = Node Int a (List (Node a))



-- Encoders


{-| A tree JSON encoder. You must provide an encoder for the datum type.
-}
encode : (a -> Encode.Value) -> Tree a -> Encode.Value
encode datumEncoder tree =
    let
        encodeNode (Node id datum children) =
            Encode.object
                [ ( "id", Encode.int id )
                , ( "value", datumEncoder datum )
                , ( "children", children |> List.map encodeNode |> Encode.list )
                ]
    in
        tree |> root |> encodeNode



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
id : Node a -> Int
id (Node id _ _) =
    id


{-| Retrieve the root node of a tree.
-}
root : Tree a -> Node a
root (Tree root) =
    root



-- Manipulation


{-| Append a new child holding a datum to a node identified by its id in a tree.
-}
appendChild : Int -> a -> Tree a -> Tree a
appendChild target newDatum tree =
    let
        appendChild_ node =
            if target == id node then
                node |> updateChildren (createNode newDatum tree :: (children node))
            else
                updateChildren (node |> children |> List.map appendChild_) node
    in
        tree |> root |> appendChild_ |> Tree


{-| Create a new node to be addable to a given tree.

A new unique identifier is generated for this node, computed against the tree.

-}
createNode : a -> Tree a -> Node a
createNode value tree =
    Node (nextId tree) value []


{-| Deletes a node from a tree, by its id.
-}
deleteNode : Int -> Tree a -> Tree a
deleteNode target tree =
    case parent target tree of
        Just (Node id value children) ->
            let
                newChildren =
                    children
                        |> List.filter (\node -> id /= target)
            in
                tree |> root |> updateChildren newChildren |> Tree

        Nothing ->
            tree


{-| Find a node in a tree, by its id.
-}
findNode : Int -> Tree a -> Maybe (Node a)
findNode target tree =
    let
        findNode_ node =
            if target == id node then
                Just node
            else
                node
                    |> children
                    |> List.map findNode_
                    |> List.filter ((/=) Nothing)
                    |> List.head
                    |> Maybe.withDefault Nothing
    in
        tree |> root |> findNode_


{-| Find nodes in a tree, by its id.
-}
findNodes : List Int -> Tree a -> List (Maybe (Node a))
findNodes ids tree =
    ids |> List.map (\id -> findNode id tree)


{-| Flatten a tree.
-}
flatten : Tree a -> List ( Int, a )
flatten (Tree root) =
    let
        flatten_ node =
            node
                |> children
                |> List.foldl
                    (\node acc ->
                        List.concat
                            [ acc
                            , [ ( id node, datum node ) ]
                            , node |> children |> List.map (\c -> ( id c, datum c ))
                            ]
                    )
                    []
    in
        flatten_ root


parent_ : Int -> Node a -> Maybe (Node a)
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
parent : Int -> Tree a -> Maybe (Node a)
parent target tree =
    tree |> root |> parent_ target


{-| Retrieve a node siblings identified by its id in a tree.
-}
siblings : Int -> Tree a -> List (Node a)
siblings target tree =
    case parent target tree of
        Just (Node _ _ children) ->
            children |> List.filter (\node -> id node /= target)

        Nothing ->
            []


{-| Computes the next available unique id for a tree.
-}
nextId : Tree a -> Int
nextId tree =
    tree
        |> flatten
        |> List.map Tuple.first
        |> List.maximum
        |> Maybe.withDefault 0
        |> (+) 1



-- Update


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



-- Demo fixtures


{-| An empty node.
-}
empty : Tree String
empty =
    Tree (Node 1 "root" [])
