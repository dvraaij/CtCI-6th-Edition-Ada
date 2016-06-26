with AUnit.Assertions;      use AUnit.Assertions;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with Ada.Text_IO;           use Ada.Text_IO;

with CtCI.Tree_Node;        use CtCI.Tree_Node;
with CtCI.Tree_Node.Export; use CtCI.Tree_Node.Export;

package body Ex_04_05_Validate_BST is

   ----------------
   -- Test Cases --
   ----------------

   ----------------------------------------------------------------------------
   procedure Test_Validate_BST
     (T : in out Test_Cases.Test_Case'Class)
   is

      T1 : Tree_Node_Access :=
             Create_Minimal_BST ((0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12));
      T2 : Tree_Node_Access :=
             Create_Minimal_BST ((0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12));

   begin

      -- Alter second subtree such that it is no longer a BST
      Find (T2, 3).Data := 5;
      Find (T2, 5).Data := 3;

      -- Export the test tree for visual inspection when debugging
      To_TikZ (T1, "ex_04_05_validate_bst_T1.tex");
      To_TikZ (T2, "ex_04_05_validate_bst_T2.tex");

      -- Assertions
      Assert (Is_BST (T1) = True , "Test 1 failed");
      Assert (Is_BST (T2) = False, "Test 2 failed");

      -- Dispose trees
      Dispose (T1);
      Dispose (T2);

   end Test_Validate_BST;

end Ex_04_05_Validate_BST;
