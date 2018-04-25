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
        (Node 0
            "root"
            [ Node 1 "node 1" []
            , Node 2
                "node 2"
                [ Node 4 "node 2.1" []
                , Node 5 "node 2.2" []
                , Node 6 "node 2.3" []
                ]
            , Node 3 "node 3" []
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
        [ describe "findNode"
            [ testTree
                |> findNode 0
                |> Maybe.map id
                |> Expect.equal (Just 0)
                |> asTest "should find root node"
            , testTree
                |> findNode 6
                |> Maybe.map id
                |> Expect.equal (Just 6)
                |> asTest "should find a deeply nested node"
            , testTree
                |> findNode 999
                |> Expect.equal Nothing
                |> asTest "should not find a non-existent node"
            ]
        , describe "parent"
            [ testTree
                |> parent 4
                |> Maybe.map id
                |> Expect.equal (Just 2)
                |> asTest "should find the parent of a given node"
            , testTree
                |> parent 2
                |> Maybe.map id
                |> Expect.equal (Just 0)
                |> asTest "should find the parent when it's root"
            , testTree
                |> parent 0
                |> Expect.equal Nothing
                |> asTest "should not find any parent for root"
            ]
        , describe "siblings"
            [ testTree
                |> siblings 4
                |> List.map id
                |> Expect.equal [ 5, 6 ]
                |> asTest "should retrieve node siblings across the tree"
            ]
        , describe "encode"
            [ testTree
                |> encode Encode.string
                |> Encode.encode 2
                |> Expect.equal json
                |> asTest "should encode a tree"
            ]
        ]
