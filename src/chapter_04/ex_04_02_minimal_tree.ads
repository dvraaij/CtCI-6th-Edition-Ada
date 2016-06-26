with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Ex_04_02_Minimal_Tree is

   -- NOTE: The minimal tree function  is implemented in the CtCI.Tree_Node
   --       package as a `factory function'. Please have a look at
   --
   --          src/code_library/ctci-tree_node.ads
   --          src/code_library/ctci-tree_node.adb

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Minimal_Tree
     (T : in out Test_Cases.Test_Case'Class);

end Ex_04_02_Minimal_Tree;
