module CanopyTest exposing (..)

import Canopy exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Expect exposing (Expectation)
import Test exposing (..)


asTest : String -> Expectation -> Test
asTest label expectation =
    expectation |> always |> test label


testTree : Node String
testTree =
    node
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
    describe "append"
        [ node "foo" [ leaf "bar" ]
            |> append "foo" "baz"
            |> Expect.equal (node "foo" [ leaf "bar", leaf "baz" ])
            |> asTest "should append a child to a node"
        , node "foo" [ leaf "bar", node "baz" [ leaf "qux" ] ]
            |> append "qux" "boo"
            |> Expect.equal (node "foo" [ leaf "bar", node "baz" [ node "qux" [ leaf "boo" ] ] ])
            |> asTest "should deeply append a child to a node"
        , node "foo" [ leaf "bar" ]
            |> append "non-existent" "baz"
            |> Expect.equal (node "foo" [ leaf "bar" ])
            |> asTest "should not append a node to a non-existent parent"
        ]


testDecode : Test
testDecode =
    describe "decode"
        [ json
            |> Decode.decodeString (decode Decode.string)
            |> Expect.equal (Ok testTree)
            |> asTest "should decode a tree"
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
            |> flatMap value
            |> Expect.equal [ "root", "node 2", "node 2.1", "node 2.2", "node 2.3" ]
            |> asTest "should selectively filter tree nodes"
        , testTree
            |> filter ((==) "node 2.2")
            |> flatMap value
            |> Expect.equal [ "root", "node 2", "node 2.2" ]
            |> asTest "should preserve parents"
        ]


testFlatMap : Test
testFlatMap =
    describe "flatMap"
        [ testTree
            |> flatMap (value >> String.toUpper)
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
            |> flatMap value
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
            |> Expect.equal (Just testTree)
            |> asTest "should find root node"
        , testTree
            |> get "node 2.3"
            |> Expect.equal (Just (leaf "node 2.3"))
            |> asTest "should find a deeply nested node"
        , testTree
            |> get "non-existent"
            |> Expect.equal Nothing
            |> asTest "should not find a non-existent node"
        , node "foo" [ leaf "bar", node "baz" [ leaf "qux" ] ]
            |> get "baz"
            |> Expect.equal (Just (node "baz" [ leaf "qux" ]))
            |> asTest "hello"
        , Node "foo" ([ Node "bar" [], Node "baz" ([ Node "qux" [] ]) ])
            |> get "bar"
            |> Expect.equal (Just (leaf "bar"))
            |> asTest "hello 2"
        ]


testMap : Test
testMap =
    describe "map"
        [ testTree
            |> map String.toUpper
            |> Expect.equal
                (node
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


testNode : Test
testNode =
    describe "node"
        [ node "foo" [ leaf "bar", node "baz" [ leaf "qux" ] ]
            |> Expect.equal (Node "foo" ([ Node "bar" [], Node "baz" ([ Node "qux" [] ]) ]))
            |> asTest "hello"
        ]


testParent : Test
testParent =
    describe "parent"
        [ testTree
            |> parent "node 2.3"
            |> Maybe.map value
            |> Expect.equal (Just "node 2")
            |> asTest "should find the parent of a given node"
        , testTree
            |> parent "node 2"
            |> Maybe.map value
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
        , node "foo" [ leaf "bar", node "baz" [ leaf "qux" ] ]
            |> path "bar"
            |> Expect.equal [ "foo", "bar" ]
            |> asTest "should find the path to a leaf at the first level"
        , node "foo" [ leaf "bar", node "baz" [ leaf "qux" ] ]
            |> path "baz"
            |> Expect.equal [ "foo", "baz" ]
            |> asTest "should find the path to a node at the first level"
        , node "foo" [ leaf "bar", node "baz" [ leaf "qux" ] ]
            |> path "qux"
            |> Expect.equal [ "foo", "baz", "qux" ]
            |> asTest "should find the path to a leaf at the second level"
        ]


testPrependChild : Test
testPrependChild =
    describe "prepend"
        [ node "foo" [ leaf "bar" ]
            |> prepend "foo" "baz"
            |> Expect.equal (node "foo" [ leaf "baz", leaf "bar" ])
            |> asTest "should prepend a child to a node"
        , node "foo" [ leaf "bar", node "baz" [ leaf "qux" ] ]
            |> prepend "qux" "boo"
            |> Expect.equal (node "foo" [ leaf "bar", node "baz" [ node "qux" [ leaf "boo" ] ] ])
            |> asTest "should deeply prepend a child to a node"
        , node "foo" [ leaf "bar" ]
            |> prepend "non-existent" "baz"
            |> Expect.equal (node "foo" [ leaf "bar" ])
            |> asTest "should not prepend a node to a non-existent parent"
        ]


testRemove : Test
testRemove =
    describe "remove"
        [ testTree
            |> remove "node 2.1"
            |> flatMap value
            |> Expect.equal [ "root", "node 1", "node 2", "node 2.2", "node 2.3", "node 3" ]
            |> asTest "should delete a deeply nested node from a tree"
        , testTree
            |> remove "node 2"
            |> flatMap value
            |> Expect.equal [ "root", "node 1", "node 3" ]
            |> asTest "should delete a node from a tree"
        , testTree
            |> remove "root"
            |> Expect.equal testTree
            |> asTest "should not delete tree root"
        ]


testReplaceNode : Test
testReplaceNode =
    describe "replaceNode"
        [ testTree
            |> replaceNode "node 2.2" (leaf "blah")
            |> get "blah"
            |> Expect.equal (Just (leaf "blah"))
            |> asTest "should replace a node in the tree"
        , testTree
            |> replaceNode "node 2" (node "node 2" [ leaf "blah" ])
            |> get "node 2"
            |> Expect.equal (Just (node "node 2" [ leaf "blah" ]))
            |> asTest "should replace a node and children in the tree"
        ]


testReplaceValue : Test
testReplaceValue =
    describe "replaceValue"
        [ testTree
            |> replaceValue "node 2.2" "blah"
            |> get "blah"
            |> Expect.equal (Just (leaf "blah"))
            |> asTest "should replace a node in the tree"
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


testToList : Test
testToList =
    describe "toList"
        [ testTree
            |> toList
            |> Expect.equal
                [ ( "root", Nothing )
                , ( "node 1", Just "root" )
                , ( "node 2", Just "root" )
                , ( "node 2.1", Just "node 2" )
                , ( "node 2.2", Just "node 2" )
                , ( "node 2.3", Just "node 2" )
                , ( "node 3", Just "root" )
                ]
            |> asTest "should turn a node into a list of tuples"
        ]


testTuple : Test
testTuple =
    describe "tuple"
        [ testTree
            |> tuple testTree
            |> Expect.equal ( "root", Nothing )
            |> asTest "should map a root node to a tuple"
        , testTree
            |> get "node 2.2"
            |> Maybe.map (tuple testTree)
            |> Expect.equal (Just ( "node 2.2", Just "node 2" ))
            |> asTest "should map a nested node to a tuple"
        ]
