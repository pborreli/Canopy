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
        , replace
        , root
        , seek
        , siblings
        )

{-| A representation of a Classification Tree.


# Basics

@docs Tree, Node, Id


# Building and manipulating a tree

@docs createTree, createNode, appendChild, deleteNode


# Manipulating a tree

@docs replace, filter, flatten, map


# Querying a tree

@docs id, idint, children, datum, findNode, findNodes, nextId, parent, root, seek, siblings


# Encoding

@docs encode

TODO:

  - if we have createNode, we should be able to attach it to the tree
  - separate tree and node in distinct modules?
  - appendChild should be for appending to a Node and return the created element

-}

import Json.Encode as Encode


{-| A node id.
-}
type Id
    = Id Int


{-| A tree.
-}
type Tree a
    = Tree (Node a)


{-| A tree node.
-}
type Node a
    = Node Id a (List (Node a))



-- Encoders


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
root : Tree a -> Node a
root (Tree root) =
    root


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
                node |> updateChildren (createNode datum tree :: (children node))
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


{-| Create a tree.
-}
createTree : a -> Tree a
createTree datum =
    Tree (Node (Id 0) datum [])


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
    let
        rootId =
            tree |> root |> id
    in
        tree
            |> seek (not << test)
            |> List.filter (\id -> id /= rootId)
            |> List.foldl (\id acc -> acc |> deleteNode id) tree


{-| Find a node in a tree by its id.
-}
findNode : Id -> Tree a -> Maybe (Node a)
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
    tree |> root |> flatten_


map_ : (a -> b) -> Node a -> Node b
map_ mapper (Node id datum children) =
    Node id (mapper datum) (children |> List.map (map_ mapper))


{-| Map all nodes data in a tree.
-}
map : (a -> b) -> Tree a -> Tree b
map mapper tree =
    tree |> root |> map_ mapper |> Tree


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
    tree |> root |> parent_ target


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
    tree |> root |> seek_ test


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
    tree |> root |> replace_ target node |> Tree


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
