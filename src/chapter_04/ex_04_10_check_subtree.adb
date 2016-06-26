with AUnit.Assertions;      use AUnit.Assertions;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with Ada.Text_IO;           use Ada.Text_IO;

with CtCI.Tree_Node.Export; use CtCI.Tree_Node.Export;

package body Ex_04_10_Check_Subtree is

   ----------------------------------------------------------------------------
   function Check_Subtree (TN1, TN2 : Tree_Node_Access) return Boolean is

      -------------------------------------------------------------------------
      function Try_Match (TN1, TN2 : Tree_Node_Access) return Boolean is
      begin

         -- Try to match existance of nodes, then (and if exists) check
         -- equality of data.
         if (TN1 = null) and (TN2 = null) then
            return True;
         elsif (TN1 = null) or (TN2 = null) then
            return False;
         elsif (TN1.Data /= TN2.Data) then

            -- If the current nodes do not match, traverse down tree 1 and see
            -- if either left of right subtree contains the subtree provided.
            return
              Try_Match (TN1.Left , TN2) or else
              Try_Match (TN1.Right, TN2);

         else

            -- If the current nodes match, check of the both left and right
            -- subtrees also match.
            return
              Try_Match (TN1.Left , TN2.Left ) and then
              Try_Match (TN1.Right, TN2.Right);

         end if;

      end Try_Match;

   begin

      -- Input checking, proceed only both are non-null
      if (TN1 = null) or (TN2 = null) then
         return False;
      end if;

      -- Try to match TN2 within TN1
      return Try_Match (TN1, TN2);

   end Check_Subtree;

   ----------------------------------------------------------------------------
   procedure Test_Check_Subtree
     (T : in out Test_Cases.Test_Case'Class)
   is

      T1 : Tree_Node_Access :=
             Create_Minimal_BST ((0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12));
      T2 : Tree_Node_Access :=
             Create_Minimal_BST ((10, 11, 12));

   begin

      -- Export the test tree for visual inspection when debugging
      To_TikZ (T1, "ex_04_10_check_subtree.tex");

      -- Assertions
      Assert (Check_Subtree (T1  , T2  ) = True , "Test 1 failed");
      Assert (Check_Subtree (T1  , null) = False, "Test 2 failed");
      Assert (Check_Subtree (null, T2  ) = False, "Test 3 failed");
      Assert (Check_Subtree (null, null) = False, "Test 4 failed");

      -- Dispose trees
      Dispose (T1);
      Dispose (T2);

   end Test_Check_Subtree;

end Ex_04_10_Check_Subtree;
