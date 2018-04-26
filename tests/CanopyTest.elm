module CanopyTest exposing (suite)

import Canopy exposing (..)
import Json.Encode as Encode
import Expect exposing (Expectation)
import Test exposing (..)


asTest : String -> Expectation -> Test
asTest label expectation =
    expectation |> always |> test label


testTree : Tree String
testTree =
    Tree
        (Node (Id 0)
            "root"
            [ Node (Id 1) "node 1" []
            , Node (Id 2)
                "node 2"
                [ Node (Id 4) "node 2.1" []
                , Node (Id 5) "node 2.2" []
                , Node (Id 6) "node 2.3" []
                ]
            , Node (Id 3) "node 3" []
            ]
        )


json : String
json =
    """{
  "id": 0,
  "value": "root",
  "children": [
    {
      "id": 1,
      "value": "node 1",
      "children": []
    },
    {
      "id": 2,
      "value": "node 2",
      "children": [
        {
          "id": 4,
          "value": "node 2.1",
          "children": []
        },
        {
          "id": 5,
          "value": "node 2.2",
          "children": []
        },
        {
          "id": 6,
          "value": "node 2.3",
          "children": []
        }
      ]
    },
    {
      "id": 3,
      "value": "node 3",
      "children": []
    }
  ]
}"""


suite : Test
suite =
    describe "Canopy"
        [ describe "appendChild"
            [ testTree
                |> appendChild (Id 2) "node 2.4"
                |> findNode (Id 2)
                |> Maybe.map (\node -> node |> children |> List.map (id >> idint))
                |> Expect.equal (Just [ 7, 4, 5, 6 ])
                |> asTest "should append a child to a node"
            ]
        , describe "deleteNode"
            [ testTree
                |> deleteNode (Id 4)
                |> flatten
                |> List.map (Tuple.first >> idint)
                |> Expect.equal [ 0, 1, 2, 5, 6, 3 ]
                |> asTest "should delete a deeply nested node from a tree"
            , testTree
                |> deleteNode (Id 2)
                |> flatten
                |> List.map (Tuple.first >> idint)
                |> Expect.equal [ 0, 1, 3 ]
                |> asTest "should delete a node from a tree"
            , testTree
                |> deleteNode (Id 0)
                |> Expect.equal testTree
                |> asTest "should silently refuse to delete a tree root node"
            ]
        , describe "encode"
            [ testTree
                |> encode Encode.string
                |> Encode.encode 2
                |> Expect.equal json
                |> asTest "should encode a tree"
            ]
        , describe "filter"
            [ testTree
                |> filter (always True)
                |> Expect.equal testTree
                |> asTest "should noop filter a tree"
            , testTree
                |> filter (\s -> String.length s > 4)
                |> Expect.equal testTree
                |> asTest "should never filter out tree root"
            , testTree
                |> filter (String.contains "2")
                |> flatten
                |> Expect.equal
                    [ ( Id 0, "root" )
                    , ( Id 2, "node 2" )
                    , ( Id 4, "node 2.1" )
                    , ( Id 5, "node 2.2" )
                    , ( Id 6, "node 2.3" )
                    ]
                |> asTest "should selectively filter tree nodes"
            ]
        , describe "flatten"
            [ testTree
                |> flatten
                |> Expect.equal
                    [ ( Id 0, "root" )
                    , ( Id 1, "node 1" )
                    , ( Id 2, "node 2" )
                    , ( Id 4, "node 2.1" )
                    , ( Id 5, "node 2.2" )
                    , ( Id 6, "node 2.3" )
                    , ( Id 3, "node 3" )
                    ]
                |> asTest "should flatten a tree"
            ]
        , describe "findNode"
            [ testTree
                |> findNode (Id 0)
                |> Maybe.map (id >> idint)
                |> Expect.equal (Just 0)
                |> asTest "should find root node"
            , testTree
                |> findNode (Id 6)
                |> Maybe.map (id >> idint)
                |> Expect.equal (Just 6)
                |> asTest "should find a deeply nested node"
            , testTree
                |> findNode (Id 999)
                |> Expect.equal Nothing
                |> asTest "should not find a non-existent node"
            ]
        , describe "map"
            [ testTree
                |> map String.toUpper
                |> findNode (Id 6)
                |> Maybe.map datum
                |> Expect.equal (Just "NODE 2.3")
                |> asTest "should map a tree"
            ]
        , describe "parent"
            [ testTree
                |> parent (Id 4)
                |> Maybe.map (id >> idint)
                |> Expect.equal (Just 2)
                |> asTest "should find the parent of a given node"
            , testTree
                |> parent (Id 2)
                |> Maybe.map (id >> idint)
                |> Expect.equal (Just 0)
                |> asTest "should find the parent when it's root"
            , testTree
                |> parent (Id 0)
                |> Expect.equal Nothing
                |> asTest "should not find any parent for root"
            ]
        , describe "seek"
            [ testTree
                |> seek (String.contains ".")
                |> List.map (\(Id id) -> id)
                |> Expect.equal [ 4, 5, 6 ]
                |> asTest "should seek a tree"
            , testTree
                |> seek (String.startsWith "node")
                |> List.map (\(Id id) -> id)
                |> Expect.equal [ 1, 2, 4, 5, 6, 3 ]
                |> asTest "should seek a tree 2"
            ]
        , describe "siblings"
            [ testTree
                |> siblings (Id 4)
                |> List.map (id >> idint)
                |> Expect.equal [ 5, 6 ]
                |> asTest "should retrieve node siblings across the tree"
            ]
        , describe "replace"
            [ testTree
                |> replace (Id 4) (Node (Id 4) "blah" [])
                |> findNode (Id 4)
                |> Maybe.map datum
                |> Expect.equal (Just "blah")
                |> asTest "should update a node in the tree"
            , testTree
                |> replace (Id 2) (Node (Id 2) "2" [ (Node (Id 4) "blah" []) ])
                |> findNode (Id 2)
                |> Expect.equal (Just (Node (Id 2) "2" [ (Node (Id 4) "blah" []) ]))
                |> asTest "should update a node in the tree 2"
            ]
        ]
