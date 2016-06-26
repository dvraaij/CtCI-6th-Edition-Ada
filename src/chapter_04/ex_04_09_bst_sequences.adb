with AUnit.Assertions;      use AUnit.Assertions;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with Ada.Text_IO;           use Ada.Text_IO;

with Ada.Containers.Vectors;

package body Ex_04_09_BST_Sequences is

   ----------------------------------------------------------------------------
   procedure BST_Sequences (TN : Tree_Node_Access) is

      package V_Integer is
        new Ada.Containers.Vectors (Positive, Integer);

      subtype Permutation              is V_Integer.Vector;
      subtype Permutation_Cursor       is V_Integer.Cursor;

      package V_Permutation is
        new Ada.Containers.Vectors (Positive, Permutation, V_Integer."=");

      subtype Permutation_List         is V_Permutation.Vector;
      subtype Permutation_List_Cursor  is V_Permutation.Cursor;

      -- List to contain all perturbations
      PL : Permutation_List := V_Permutation.Empty_Vector;

      -------------------------------------------------------------------------
      procedure Visit (TN : Tree_Node_Access; C : Permutation_List_Cursor) is
      begin

         -- Return immediately if node is null
         if (TN = null) then
            return;
         end if;

         -- Add current node to permutation
         PL (C).Append (TN.Data);

         -- Add children to permutation
         if (TN.Left = null) or (TN.Right = null) then

            -- Visit order doesn't matter, one or more children is null anyway
            Visit (TN.Left , C);
            Visit (TN.Right, C);

         else
            declare
               C_Alt : Permutation_List_Cursor := V_Permutation.No_Element;
            begin

               -- Clone vector, insert before current permutation
               PL.Insert (C, PL (C).Copy, C_Alt);

               -- Visit in order LEFT then RIGHT
               Visit (TN.Left , C);
               Visit (TN.Right, C);

               -- Visit in order RIGHT then LEFT (the alternative order)
               Visit (TN.Right, C_Alt);
               Visit (TN.Left , C_Alt);

            end;
         end if;

      end Visit;

   begin

      -- Initial list
      PL.Append (V_Integer.Empty_Vector);

      -- Traverse the tree
      Visit (TN, PL.First);

   end BST_Sequences;

   ----------------
   -- Test Cases --
   ----------------

   ----------------------------------------------------------------------------
   procedure Test_BST_Sequences
     (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      null;
   end Test_BST_Sequences;

end Ex_04_09_BST_Sequences;
