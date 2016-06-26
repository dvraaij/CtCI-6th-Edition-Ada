with AUnit.Assertions;      use AUnit.Assertions;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with Ada.Text_IO;           use Ada.Text_IO;

with CtCI.Tree_Node.Export; use CtCI.Tree_Node.Export;

package body Ex_04_06_Successor is

   ----------------------------------------------------------------------------
   function Successor (TN : Tree_Node_Access) return Tree_Node_Access is
   begin

      -- Input checking
      if (TN = null) then
         return null;
      end if;

      -- Return the result
      return R : Tree_Node_Access := TN do

         -- Try to find left-most child in right subtree (if available). All
         -- values in the right subtree are larger than current node, the
         -- left-most value with the right subtree is the smallest value (i.e.
         -- successor) within that set.

         -- If there is no right subtree, try to go up to find the root node of
         -- of the current left subtree we're in. All other values of the node
         -- in this left subtree are smaller than that of the the given node
         -- and will therefore not be a succesor.

         if R.Right /= null then

            R := R.Right;
            while R.Left /= null loop
               R := R.Left;
            end loop;

         else

            while
              (R.Parent       /= null) and then
              (R.Parent.Right  = R   )
            loop
               R := R.Parent;
            end loop;

            -- We've traversed up the left subtree, now seek for its root node.
            -- Note that a root node may not exist if the left subtree appears
            -- to be the whole tree itself. The given node is then the largest
            -- value with in whole tree and R (the Result) will become null.
            R := R.Parent;

         end if;

      end return;

   end Successor;

   ----------------
   -- Test Cases --
   ----------------

   ----------------------------------------------------------------------------
   procedure Test_Successor
     (T : in out Test_Cases.Test_Case'Class)
   is

      T1 : Tree_Node_Access :=
             Create_Minimal_BST ((0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12));

      Result : Tree_Node_Access;

   begin

      -- Export the test tree for visual inspection when debugging
      To_TikZ (T1, "ex_04_06_successor.tex");

      -- Assertions
      Result := Successor (Find (T1,  2));
      Assert (Result /= null and then Result.Data = 3, "Test 1 failed");

      Result := Successor (Find (T1,  5));
      Assert (Result /= null and then Result.Data = 6, "Test 2 failed");

      Result := Successor (Find (T1, 12));
      Assert (Result  = null, "Test 3 failed");

      -- Dispose tree
      Dispose (T1);

   end Test_Successor;

end Ex_04_06_Successor;
