module CanopyTest exposing (..)

import Canopy exposing (..)
import Canopy.Node exposing (Node(..), node, datum)
import Json.Decode as Decode
import Json.Encode as Encode
import Expect exposing (Expectation)
import Test exposing (..)


asTest : String -> Expectation -> Test
asTest label expectation =
    expectation |> always |> test label


testTree : Tree String
testTree =
    tree
        "root"
        [ leaf "node 1"
        , node
            "node 2"
            [ leaf "node 2.1"
            , leaf "node 2.2"
            , leaf "node 2.3"
            ]
        , leaf "node 3"
        ]


json : String
json =
    """{
  "value": "root",
  "children": [
    {
      "value": "node 1",
      "children": []
    },
    {
      "value": "node 2",
      "children": [
        {
          "value": "node 2.1",
          "children": []
        },
        {
          "value": "node 2.2",
          "children": []
        },
        {
          "value": "node 2.3",
          "children": []
        }
      ]
    },
    {
      "value": "node 3",
      "children": []
    }
  ]
}"""


testAppendChild : Test
testAppendChild =
    describe "appendChild"
        [ tree "foo" [ leaf "bar" ]
            |> appendChild "foo" "baz"
            |> Expect.equal (tree "foo" [ leaf "bar", leaf "baz" ])
            |> asTest "should append a child to a node"
        , tree "foo" [ leaf "bar", node "baz" [ leaf "qux" ] ]
            |> appendChild "qux" "boo"
            |> Expect.equal (tree "foo" [ leaf "bar", node "baz" [ node "qux" [ leaf "boo" ] ] ])
            |> asTest "should deeply append a child to a node"
        , tree "foo" [ leaf "bar" ]
            |> appendChild "non-existent" "baz"
            |> Expect.equal (tree "foo" [ leaf "bar" ])
            |> asTest "should not append a node to a non-existent parent"
        ]


testDecode : Test
testDecode =
    describe "decode"
        [ "null"
            |> Decode.decodeString (decode Decode.string)
            |> Expect.equal (Ok Empty)
            |> asTest "should decode an empty tree"
        , json
            |> Decode.decodeString (decode Decode.string)
            |> Expect.equal (Ok testTree)
            |> asTest "should decode an seeded tree"
        ]


testDeleteNode : Test
testDeleteNode =
    describe "deleteNode"
        [ testTree
            |> deleteNode "node 2.1"
            |> flatMap datum
            |> Expect.equal [ "root", "node 1", "node 2", "node 2.2", "node 2.3", "node 3" ]
            |> asTest "should delete a deeply nested node from a tree"
        , testTree
            |> deleteNode "node 2"
            |> flatMap datum
            |> Expect.equal [ "root", "node 1", "node 3" ]
            |> asTest "should delete a node from a tree"
        , testTree
            |> deleteNode "root"
            |> Expect.equal Empty
            |> asTest "should delete a tree root node"
        ]


testEncode : Test
testEncode =
    describe "encode"
        [ testTree
            |> encode Encode.string
            |> Encode.encode 2
            |> Expect.equal json
            |> asTest "should encode a tree"
        ]


testFilter : Test
testFilter =
    describe "filter"
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
            |> flatMap datum
            |> Expect.equal [ "root", "node 2", "node 2.1", "node 2.2", "node 2.3" ]
            |> asTest "should selectively filter tree nodes"
        , testTree
            |> filter ((==) "node 2.2")
            |> flatMap datum
            |> Expect.equal [ "root", "node 2", "node 2.2" ]
            |> asTest "should preserve parents"
        ]


testFlatMap : Test
testFlatMap =
    describe "flatMap"
        [ testTree
            |> flatMap (datum >> String.toUpper)
            |> Expect.equal
                [ "ROOT"
                , "NODE 1"
                , "NODE 2"
                , "NODE 2.1"
                , "NODE 2.2"
                , "NODE 2.3"
                , "NODE 3"
                ]
            |> asTest "should flatMap a tree"
        ]


testFlatten : Test
testFlatten =
    describe "flatten"
        [ testTree
            |> flatMap datum
            |> Expect.equal
                [ "root"
                , "node 1"
                , "node 2"
                , "node 2.1"
                , "node 2.2"
                , "node 2.3"
                , "node 3"
                ]
            |> asTest "should flatten a tree"
        ]


testGet : Test
testGet =
    describe "get"
        [ testTree
            |> get "root"
            |> Expect.equal (root testTree)
            |> asTest "should find root node"
        , testTree
            |> get "node 2.3"
            |> Expect.equal (Just (leaf "node 2.3"))
            |> asTest "should find a deeply nested node"
        , testTree
            |> get "non-existent"
            |> Expect.equal Nothing
            |> asTest "should not find a non-existent node"
        , tree "foo" [ leaf "bar", node "baz" [ leaf "qux" ] ]
            |> get "baz"
            |> Expect.equal (Just (node "baz" [ leaf "qux" ]))
            |> asTest "hello"
        , Seeded (Node "foo" ([ Node "bar" [], Node "baz" ([ Node "qux" [] ]) ]))
            |> get "bar"
            |> Expect.equal (Just (leaf "bar"))
            |> asTest "hello 2"
        ]


testTree_ : Test
testTree_ =
    describe "tree"
        [ tree "foo" [ leaf "bar", node "baz" [ leaf "qux" ] ]
            |> Expect.equal (Seeded (Node "foo" ([ Node "bar" [], Node "baz" ([ Node "qux" [] ]) ])))
            |> asTest "hello"
        ]


testMap : Test
testMap =
    describe "map"
        [ testTree
            |> map String.toUpper
            |> Expect.equal
                (tree
                    "ROOT"
                    [ leaf "NODE 1"
                    , node
                        "NODE 2"
                        [ leaf "NODE 2.1"
                        , leaf "NODE 2.2"
                        , leaf "NODE 2.3"
                        ]
                    , leaf "NODE 3"
                    ]
                )
            |> asTest "should map a tree"
        ]


testParent : Test
testParent =
    describe "parent"
        [ testTree
            |> parent "node 2.3"
            |> Maybe.map datum
            |> Expect.equal (Just "node 2")
            |> asTest "should find the parent of a given node"
        , testTree
            |> parent "node 2"
            |> Maybe.map datum
            |> Expect.equal (Just "root")
            |> asTest "should find the parent when it's root"
        , testTree
            |> parent "root"
            |> Expect.equal Nothing
            |> asTest "should not find any parent for root"
        ]


testPath : Test
testPath =
    describe "path"
        [ testTree
            |> path "non-existent"
            |> Expect.equal []
            |> asTest "should return an empty path to a non-existent node"
        , testTree
            |> path "root"
            |> Expect.equal [ "root" ]
            |> asTest "should compute the path to root"
        , testTree
            |> path "node 2"
            |> Expect.equal [ "root", "node 2" ]
            |> asTest "should compute the path to reach a node"
        , testTree
            |> path "node 2.2"
            |> Expect.equal [ "root", "node 2", "node 2.2" ]
            |> asTest "should compute the path to reach a deeply nested node"

        -- handcrafted tree
        , tree "foo" [ leaf "bar", node "baz" [ leaf "qux" ] ]
            |> path "bar"
            |> Expect.equal [ "foo", "bar" ]
            |> asTest "should find the path to a leaf at the first level"
        , tree "foo" [ leaf "bar", node "baz" [ leaf "qux" ] ]
            |> path "baz"
            |> Expect.equal [ "foo", "baz" ]
            |> asTest "should find the path to a node at the first level"
        , tree "foo" [ leaf "bar", node "baz" [ leaf "qux" ] ]
            |> path "qux"
            |> Expect.equal [ "foo", "baz", "qux" ]
            |> asTest "should find the path to a leaf at the second level"
        ]


testPrependChild : Test
testPrependChild =
    describe "prependChild"
        [ tree "foo" [ leaf "bar" ]
            |> prependChild "foo" "baz"
            |> Expect.equal (tree "foo" [ leaf "baz", leaf "bar" ])
            |> asTest "should prepend a child to a node"
        , tree "foo" [ leaf "bar", node "baz" [ leaf "qux" ] ]
            |> prependChild "qux" "boo"
            |> Expect.equal (tree "foo" [ leaf "bar", node "baz" [ node "qux" [ leaf "boo" ] ] ])
            |> asTest "should deeply prepend a child to a node"
        , tree "foo" [ leaf "bar" ]
            |> prependChild "non-existent" "baz"
            |> Expect.equal (tree "foo" [ leaf "bar" ])
            |> asTest "should not prepend a node to a non-existent parent"
        ]


testReplace : Test
testReplace =
    describe "replace"
        [ testTree
            |> replace "node 2.2" (leaf "blah")
            |> get "blah"
            |> Expect.equal (Just (leaf "blah"))
            |> asTest "should replace a node in the tree"
        , testTree
            |> replace "node 2" (node "node 2" [ leaf "blah" ])
            |> get "node 2"
            |> Expect.equal (Just (node "node 2" [ leaf "blah" ]))
            |> asTest "should replace a node and children in the tree"
        ]


testSeek : Test
testSeek =
    describe "seek"
        [ testTree
            |> seek (String.contains ".")
            |> Expect.equal [ "node 2.1", "node 2.2", "node 2.3" ]
            |> asTest "should seek a tree"
        , testTree
            |> seek (String.startsWith "node")
            |> Expect.equal [ "node 1", "node 2", "node 2.1", "node 2.2", "node 2.3", "node 3" ]
            |> asTest "should seek a tree 2"
        ]


testSiblings : Test
testSiblings =
    describe "siblings"
        [ testTree
            |> siblings "node 2.2"
            |> Expect.equal [ "node 2.1", "node 2.3" ]
            |> asTest "should retrieve node siblings across the tree"
        ]
