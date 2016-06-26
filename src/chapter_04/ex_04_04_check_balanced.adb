with AUnit.Assertions;      use AUnit.Assertions;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with Ada.Text_IO;           use Ada.Text_IO;

with CtCI.Tree_Node.Export; use CtCI.Tree_Node.Export;

package body Ex_04_04_Check_Balanced is

   ----------------------------------------------------------------------------
   function Check_Balanced (TN : Tree_Node_Access) return Boolean is

      Max_Depth : Natural := Natural'First;
      Min_Depth : Natural := Natural'Last;

      -- Tree is balanced until proved otherwise
      Balanced  : Boolean := True;

      -------------------------------------------------------------------------
      procedure Visit (TN : Tree_Node_Access; Depth : Natural) is
      begin

         -- If found not balanced or node is null, do not seek further
         if not Balanced or (TN = null) then
            return;
         end if;

         -- Traverse further downwards
         Visit (TN.Left , Depth + 1);
         Visit (TN.Right, Depth + 1);

         -- If this node has zero or one child(ren), then assert that the max
         -- difference is at most 1.
         if (TN.Left = null) or (TN.Right = null) then

            Max_Depth := Natural'Max (Max_Depth, Depth);
            Min_Depth := Natural'Min (Min_Depth, Depth);

            Balanced := not ((Max_Depth - Min_Depth) > 1);

         end if;

      end Visit;

   begin

      -- Input checking
      if (TN = null) then
         return False;
      end if;

      -- Search tree
      Visit (TN, 0);

      -- Return the result
      return Balanced;

   end Check_Balanced;

   ----------------
   -- Test Cases --
   ----------------

   ----------------------------------------------------------------------------
   procedure Test_Check_Balanced
     (T : in out Test_Cases.Test_Case'Class)
   is

      T1 : Tree_Node_Access :=
             Create_Minimal_BST ((0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12));
      T2 : Tree_Node_Access :=
             Create_Minimal_BST ((0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12));
      T3 : Tree_Node_Access :=
             Create_Minimal_BST ((0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12));

   begin

      -- Unbalance tree by removing a subtree
      declare
         TN : Tree_Node_Access := Find (T2, 9);
      begin
         Dispose (TN);
      end;

      declare
         TN : Tree_Node_Access := Find (T3, 2);
      begin
         Dispose (TN);
      end;

      -- Export the test tree for visual inspection when debugging
      To_TikZ (T1, "ex_04_04_check_balanced_T1.tex");
      To_TikZ (T2, "ex_04_04_check_balanced_T2.tex");
      To_TikZ (T3, "ex_04_04_check_balanced_T3.tex");

      -- Assertions
      Assert (Check_Balanced (T1) = True , "Test 1 failed");
      Assert (Check_Balanced (T2) = False, "Test 2 failed");
      Assert (Check_Balanced (T3) = False, "Test 3 failed");

      -- Dispose trees
      Dispose (T1);
      Dispose (T2);
      Dispose (T3);

   end Test_Check_Balanced;

end Ex_04_04_Check_Balanced;
