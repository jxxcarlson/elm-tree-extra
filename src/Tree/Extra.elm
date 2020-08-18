module Tree.Extra exposing
    ( attachSubtree, removeSubtree, moveSubtree, spanningTree
    , attachSubtreeInOrder
    , depth, nodeCount, tagWithDepth
    )

{-|


## Manipulate Trees

@docs attachSubtree, removeSubtree, moveSubtree, spanningTree


## Manipulate using a partial order on nodes

@docs attachSubtreeInOrder


## Info on Trees

@docs depth, nodeCount, tagWithDepth

-}

import Maybe.Extra
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)


{-|

    > a = t 1 [ t 2 [ s 3, t 4 [s 5, s 6]]]
    > x = t 2 [ s 3, s 4]

    > attachSubtree 1 x a
    Just (Tree 1 [Tree 2 [Tree 3 [],Tree 4 [Tree 5 [],Tree 6 []]],Tree 2 [Tree 3 [],Tree 4 []]])

-}
attachSubtree : a -> Tree a -> Tree a -> Maybe (Tree a)
attachSubtree targetNode subTree tree =
    let
        zipper =
            Zipper.fromTree tree |> setFocus targetNode

        zipper2 =
            Maybe.map2 appendTreeToFocus (Just subTree) zipper
    in
    Maybe.map Zipper.toTree zipper2


{-| The function

    attachSubtreeInOrder bigger targetNode subTree tree

seeks to attach a subtree to a given tree
is such a way that the parent of the subtree
has the proper order with respect to the function

    bigger : a -> a -> Bool

namely,

  - it is greater than the root of the subtree

  - it is greater than or equal to targetNode

  - it is the least such node

Here is an example:

    > a = t 1 [ t 2 [ s 3, t 4 [s 5, s 6]]]
    > Tree 1 [Tree 2 [Tree 3 [],Tree 4 [Tree 5 [],Tree 6 []]]]

    > x = t 3 [ s 4, s 5]
    > Tree 3 [Tree 4 [],Tree 5 []]

    > attachSubtreeInOrder (<) 6 x a
    > Just (Tree 1 [Tree 2 [Tree 3 [],Tree 4 [Tree 5 [],Tree 6 []],Tree 3 [Tree 4 [],Tree 5 []]]])

-}
attachSubtreeInOrder : (a -> a -> Bool) -> a -> Tree a -> Tree a -> Maybe (Tree a)
attachSubtreeInOrder bigger targetNode subTree tree =
    let
        subTreeRoot =
            Zipper.fromTree subTree
                |> Zipper.root
                |> Zipper.label
    in
    case findAttachmentNode bigger subTreeRoot targetNode tree of
        Nothing ->
            Nothing

        Just attachmentNode ->
            let
                zipper =
                    Zipper.fromTree tree |> setFocus attachmentNode

                zipper2 =
                    Maybe.map2 appendTreeToFocus (Just subTree) zipper
            in
            Maybe.map Zipper.toTree zipper2


appendTreeToFocus : Tree a -> Zipper a -> Zipper a
appendTreeToFocus t_ z =
    let
        newTree =
            Tree.appendChild t_ (Zipper.tree z)
    in
    Zipper.replaceTree newTree z


{-|

    > a = t 1 [ t 2 [ s 3, t 4 [s 5, s 6]]]
    Tree 1 [Tree 2 [Tree 3 [],Tree 4 [Tree 5 [],Tree 6 []]]]

    > findAttachmentNode (<) 3  6 a
    Just 2 : Maybe number

-}
findAttachmentNode : (a -> a -> Bool) -> a -> a -> Tree a -> Maybe a
findAttachmentNode bigger_ node targetNode tree =
    let
        zipper =
            Zipper.fromTree tree |> setFocus targetNode

        gte a b =
            case b of
                Nothing ->
                    False

                Just b_ ->
                    bigger_ a b_

        initialState =
            { zipper = zipper, target = Just targetNode, node = node, gte = gte }
    in
    loop initialState nextATState


type alias ATState a =
    { zipper : Maybe (Zipper a)
    , target : Maybe a
    , node : a
    , gte : a -> Maybe a -> Bool
    }


nextATState : ATState a -> Step (ATState a) (Maybe a)
nextATState state =
    case state.gte state.node state.target of
        False ->
            Done state.target

        True ->
            let
                newZipper =
                    Maybe.andThen Zipper.parent state.zipper

                newTarget =
                    Maybe.map Zipper.label newZipper
            in
            Loop { state | zipper = newZipper, target = newTarget }


{-|

    > a = t 1 [ t 2 [ s 3, t 4 [s 5, s 6]]]
    Tree 1 [Tree 2 [Tree 3 [],Tree 4 [Tree 5 [],Tree 6 []]]]

    > removeSubtree 3 a |> Maybe.andThen (removeSubtree 5)
    Just (Tree 1 [Tree 2 [Tree 4 [Tree 6 []]]])

-}
removeSubtree : a -> Tree a -> Maybe (Tree a)
removeSubtree a tree =
    let
        zipper =
            Zipper.fromTree tree
                |> setFocus a
    in
    Maybe.map Zipper.removeTree zipper
        |> Maybe.Extra.join
        |> Maybe.map Zipper.toTree


{-| Compute the smallest subtree of the given tree
which contains all the nodes of the nodeList.

    > a = t 1 [ t 2 [ s 3, t 4 [s 5, s 6]]]
    Tree 1 [Tree 2 [Tree 3 [],Tree 4 [Tree 5 [],Tree 6 []]]]

    > spanningTree [3, 5] a
    Just (Tree 2 [Tree 3 [],Tree 4 [Tree 5 [],Tree 6 []]])

-}
spanningTree : List a -> Tree a -> Maybe (Tree a)
spanningTree nodeList tree =
    let
        root =
            List.head nodeList

        nodeList1 =
            List.drop 1 nodeList

        zipper =
            Maybe.map2 setFocus root (Just (Zipper.fromTree tree)) |> Maybe.Extra.join

        subTree =
            Maybe.map Zipper.tree zipper

        nodeList2 : List a
        nodeList2 =
            Maybe.map2 subtractNodesOfTree subTree (Just nodeList1) |> Maybe.withDefault []

        initialState =
            { nodeList = nodeList2
            , tree = tree
            , zipper = zipper
            , root = root
            }
    in
    loop initialState nextSpanningState


type alias SpanningState a =
    { nodeList : List a
    , zipper : Maybe (Zipper a)
    , tree : Tree a
    , root : Maybe a
    }


nextSpanningState : SpanningState a -> Step (SpanningState a) (Maybe (Tree a))
nextSpanningState state =
    case ( state.root, state.nodeList ) of
        ( Nothing, _ ) ->
            Done Nothing

        ( Just root, [] ) ->
            let
                refocusedZipper : Maybe (Zipper a)
                refocusedZipper =
                    Maybe.map2 setFocus (Just root) state.zipper |> Maybe.Extra.join

                output : Maybe (Tree a)
                output =
                    Maybe.map Zipper.tree refocusedZipper
            in
            Done output

        ( Just _, nodeList ) ->
            let
                newZipper =
                    Maybe.andThen Zipper.parent state.zipper

                newRoot =
                    Maybe.map Zipper.label newZipper

                nodeList1 : List a
                nodeList1 =
                    List.drop 1 nodeList

                zipper =
                    Maybe.map2 setFocus newRoot state.zipper |> Maybe.Extra.join

                subTree =
                    Maybe.map Zipper.tree zipper

                nodeList2 : List a
                nodeList2 =
                    Maybe.map2 subtractNodesOfTree subTree (Just nodeList1) |> Maybe.withDefault []
            in
            Loop { state | nodeList = nodeList2, zipper = zipper, root = newRoot }


{-|

    > s = Tree.singleton
    > t = Tree.tree

    > a = t 1 [ t 2 [t 3 [ s 4]]]
    -->  Tree 1 [Tree 2 [Tree 3 [Tree 4 []]]]

    > subtractNodesOfTree a [3, 4, 8, 9]
    [8,9] : List number

-}
subtractNodesOfTree : Tree a -> List a -> List a
subtractNodesOfTree tree nodeList =
    tree
        |> Tree.flatten
        |> listDifference nodeList


listDifference : List a -> List a -> List a
listDifference list1 list2 =
    let
        folder : List a -> a -> List a -> List a
        folder list1_ item list2_ =
            if not <| List.member item list1_ then
                item :: list2_

            else
                list2_
    in
    List.foldl (folder list2) [] list1
        |> List.reverse


setFocus : a -> Zipper a -> Maybe (Zipper a)
setFocus node zipper =
    Zipper.findFromRoot (\label -> label == node) zipper


{-|

    > a = t 1 [ t 2 [ s 3, t 4 [s 5, s 6]]]
    Tree 1 [Tree 2 [Tree 3 [],Tree 4 [Tree 5 [],Tree 6 []]]]

    > moveSubtree 4 1 a
    Just (Tree 1 [Tree 2 [Tree 3 []],Tree 4 [Tree 5 [],Tree 6 []]])

-}
moveSubtree : a -> a -> Tree a -> Maybe (Tree a)
moveSubtree from to tree =
    moveSubtreeInZipper from to (Zipper.fromTree tree)
        |> Maybe.map Zipper.toTree


moveSubtreeInZipper : a -> a -> Zipper a -> Maybe (Zipper a)
moveSubtreeInZipper from to zipper =
    let
        refocusedZipper =
            setFocus from zipper

        subTree =
            refocusedZipper
                |> Maybe.map Zipper.tree

        prunedZipper =
            refocusedZipper
                |> Maybe.andThen Zipper.removeTree
                |> Maybe.andThen (setFocus to)
    in
    Maybe.map2 appendTreeToFocus subTree prunedZipper


{-| This will terminate if the root of the zipper has blockType Document,
which is greatest in the partial order
-}
findValidParent : (a -> a -> Bool) -> a -> Tree a -> Maybe a
findValidParent gte node tree =
    let
        nextValidParentState : ValidParentState a -> Step (ValidParentState a) (ValidParentState a)
        nextValidParentState state =
            let
                gte_ : a -> Maybe a -> Bool
                gte_ a b_ =
                    case b_ of
                        Nothing ->
                            False

                        Just b ->
                            gte a b
            in
            if gte_ state.node (Maybe.map Zipper.label state.zipper) then
                case Maybe.map Zipper.parent state.zipper |> Maybe.Extra.join of
                    Nothing ->
                        Done state

                    Just z ->
                        Loop { state | zipper = Just z, count = state.count + 1 }

            else
                Done state

        initialZipper =
            Zipper.fromTree tree
                |> setFocus node
                |> Maybe.map Zipper.parent
                |> Maybe.Extra.join
    in
    loop
        { node = node
        , zipper = initialZipper
        , count = 0
        }
        nextValidParentState
        |> .zipper
        |> Maybe.map Zipper.label


type alias ValidParentState a =
    { node : a, zipper : Maybe (Zipper a), count : Int }


{-|

    > a = t 1 [ t 2 [ s 3, t 4 [s 5, s 6]]]

    > depth a
    3

-}
depth : Tree a -> Int
depth t =
    let
        c =
            Tree.children t
    in
    if c == [] then
        0

    else
        1 + maxiumumPositiveInteger (List.map depth c)


{-| Number of notes in a tree

        > a = t 1 [ t 2 [ s 3, t 4 [s 5, s 6]]]

        > nodeCount a
        6

-}
nodeCount : Tree a -> Int
nodeCount t =
    let
        c =
            Tree.children t
    in
    if c == [] then
        1

    else
        1 + List.sum (List.map nodeCount c)


{-| maximum integer in a list of non-negative integers
-}
maxiumumPositiveInteger : List Int -> Int
maxiumumPositiveInteger ints =
    List.foldl (\i acc -> max i acc) 0 ints


{-| Transforms a tree of items into
a tree of tuples of the form `(a, k)`, where `k` is the
depth of `a` in the tree.

    > a = t 1 [ t 2 [ s 3, t 4 [s 5, s 6]]]

    > tagWithDepth a
    > Tree (1,0) [Tree (2,1) [Tree (3,2) [],Tree (4,2) [Tree (5,3) [],Tree (6,3) []]]]

-}
tagWithDepth : Tree a -> Tree ( a, Int )
tagWithDepth t =
    tagWithDepthHelp 0 t


tagWithDepthHelp : Int -> Tree a -> Tree ( a, Int )
tagWithDepthHelp k t =
    let
        c =
            Tree.children t
    in
    Tree.tree ( Tree.label t, k ) (List.map (\t_ -> tagWithDepthHelp (k + 1) t_) c)



-- LOOP


type Step state a
    = Loop state
    | Done a


loop : state -> (state -> Step state a) -> a
loop s nextState =
    case nextState s of
        Loop s_ ->
            loop s_ nextState

        Done b ->
            b
