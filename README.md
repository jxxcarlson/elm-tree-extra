# Tree.Extra

Tree.Extra provides functions for manipulating rose trees as defined
in [zwilias/elm-rosetree](https://package.elm-lang.org/packages/zwilias/elm-rosetree/latest/). 
For example,

    removeSubTree node tree

removes the subtree whose root is `node` from `tree`. The
function call

    moveSubTree subTreeRoot attachmentNode tree

moves a subtree of `tree` given the root node 
of the subtree and the node of the tree to which
it should be re-attached.  Here is an example.


````elm

    > import Tree  -- from zwilias/elm-rosetree
    > import Tree.Extra exposing(..)
    
    > s = Tree.singleton
    > t = Tree.tree 

    > c = t 1 [ t 2 [ s 3, t 4 [s 5, s 6]]]
    Tree 1 [Tree 2 [Tree 3 [],Tree 4 [Tree 5 [],Tree 6 []]]]

   > moveSubTree 4 1 c
    Just (Tree 1 [Tree 2 [Tree 3 []],Tree 4 [Tree 5 [],Tree 6 []]])

````