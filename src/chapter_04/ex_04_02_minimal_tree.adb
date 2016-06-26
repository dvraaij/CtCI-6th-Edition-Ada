with AUnit.Assertions;      use AUnit.Assertions;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with Ada.Text_IO;           use Ada.Text_IO;

with CtCI.Tree_Node;        use CtCI.Tree_Node;

package body Ex_04_02_Minimal_Tree is

   ----------------------------------------------------------------------------
   procedure Test_Minimal_Tree
     (T : in out Test_Cases.Test_Case'Class)
   is

      T1 : Tree_Node_Access :=
             Create_Minimal_BST ((1 => 0));

      T2 : Tree_Node_Access :=
             Create_Minimal_BST ((0, 1, 2, 3, 4, 5, 6));

      T3 : Tree_Node_Access :=
             Create_Minimal_BST ((0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12));

   begin

      -- Assertions
      Assert ( Height (T1) = 1, "Test 1 failed");
      Assert ( Height (T2) = 3, "Test 2 failed");
      Assert ( Height (T3) = 4, "Test 3 failed");

   end Test_Minimal_Tree;

end Ex_04_02_Minimal_Tree;
