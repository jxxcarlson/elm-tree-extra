module TreeExtraTest exposing (suite)

import Expect exposing (Expectation)
import Test exposing (..)
import Tree
import Tree.Extra exposing (..)


s =
    Tree.singleton


t =
    Tree.tree


a =
    t 1 [ t 2 [ s 3, t 4 [ s 5, s 6 ] ] ]


suite : Test
suite =
    describe "The Tree.Extra module"
        [ test "moveSubTree" <|
            \_ ->
                moveSubTree 4 1 a
                    |> Expect.equal (Just (t 1 [ t 2 [ t 3 [] ], t 4 [ t 5 [], t 6 [] ] ]))
        , test "removeSubTree" <|
            \_ ->
                removeSubTree 3 a
                    |> Maybe.andThen (removeSubTree 5)
                    |> Expect.equal (Just (t 1 [ t 2 [ t 4 [ t 6 [] ] ] ]))
        , test "spanningTree" <|
            \_ ->
                spanningTree [ 3, 5 ] a
                    |> Maybe.andThen (removeSubTree 5)
                    |> Expect.equal (Just (t 2 [ t 3 [], t 4 [ t 5 [], t 6 [] ] ]))
        ]
