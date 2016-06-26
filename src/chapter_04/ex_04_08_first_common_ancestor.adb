with AUnit.Assertions;      use AUnit.Assertions;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with Ada.Text_IO;           use Ada.Text_IO;

with CtCI.Tree_Node.Export; use CtCI.Tree_Node.Export;

package body Ex_04_08_First_Common_Ancestor is

   ----------------------------------------------------------------------------
   function First_Common_Ancestor
     (Root, TN1, TN2 : Tree_Node_Access) return Tree_Node_Access
   is

      Ancestor_Node : Tree_Node_Access := null;

      type Result is
         record
            TN1 : Boolean;
            TN2 : Boolean;
         end record;

      -------------------------------------------------------------------------
      function Contains (TN : Tree_Node_Access) return Result is

         RL : Result;
         RR : Result;

      begin

         -- If the ancestor has been found or TN is null, then just return
         if (Ancestor_Node /= null) or (TN = null) then
            return Result'(False, False);
         end if;

         -- Check if the left or right subtree contains either
         -- of the given nodes
         RL := Contains (TN.Left );
         RR := Contains (TN.Right);

         -- If
         --
         --    N1 is in the left subtree OR right subtree AND
         --    N2 is in the left subtree OR right subtree
         --
         -- then we've found the common ancestor. Note that both nodes may be
         -- contained in the same subtree.
         if (RL.TN1 or RR.TN1) and (RL.TN2 or RR.TN2) then

            Ancestor_Node := TN;
            return Result'(False, False);

         end if;

         -- Update and propagate result upwards
         return Result'(RL.TN1 or TN = TN1 or RR.TN1,
                        RL.TN2 or TN = TN2 or RR.TN2);

      end Contains;

      Foo : Result;

   begin

      -- Input checking
      if (Root = null) or (TN1 = null) or (TN2 = null) then
         return null;
      end if;

      -- Traverse the tree (post-order traversal)
      Foo := Contains (Root);

      -- Return the result
      return Ancestor_Node;

   end First_Common_Ancestor;

   ----------------
   -- Test Cases --
   ----------------

   ----------------------------------------------------------------------------
   procedure Test_First_Common_Ancestor
     (T : in out Test_Cases.Test_Case'Class)
   is

      T1 : Tree_Node_Access :=
             Create_Minimal_BST ((0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12));

      Result : Tree_Node_Access;

   begin

      -- Export the test tree for visual inspection when debugging
      To_TikZ (T1, "ex_04_08_first_common_ancestor.tex");

      -- Assertions
      Result := First_Common_Ancestor (T1, Find (T1, 7), Find (T1, 12));
      Assert (Result /= null and then Result.Data = 9, "Test 1 failed");

      Result := First_Common_Ancestor (T1, Find (T1, 9), Find (T1, 12));
      Assert (Result /= null and then Result.Data = 6, "Test 2 failed");

      Result := First_Common_Ancestor (T1, Find (T1, 4), Find (T1, 5));
      Assert (Result /= null and then Result.Data = 2, "Test 3 failed");

      Result := First_Common_Ancestor (T1, Find (T1, 7), Find (T1, 7));
      Assert (Result /= null and then Result.Data = 9, "Test 4 failed");

      -- Dispose tree
      Dispose (T1);

   end Test_First_Common_Ancestor;

end Ex_04_08_First_Common_Ancestor;
