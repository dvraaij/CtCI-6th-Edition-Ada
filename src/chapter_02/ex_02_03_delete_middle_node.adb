with AUnit.Assertions;      use AUnit.Assertions;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with Ada.Text_IO;           use Ada.Text_IO;

with CtCI.Linked_List_Node; use CtCI.Linked_List_Node;

package body Ex_02_03_Delete_Middle_Node is

   ----------------------------------------------------------------------------
   -- Algorithm 1                                                            --
   --                                                                        --
   --    Comp. complexity : O(1)                                             --
   --    Space complexity : O(1)                                             --
   ----------------------------------------------------------------------------

   procedure Delete_Middle_Node (N : Node_Access) is
   begin

      -- Input checking
      if N.Next = null then
         raise Constraint_Error;
      end if;

      -- Copy content of subsequent node to the given node
      N.Data :=    N.Next.Data;
      Set_Next (N, N.Next.Next);

   end Delete_Middle_Node;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Delete_Middle_Node (T : in out Test_Cases.Test_Case'Class) is

      List : Nodes := Linked_List (8);

   begin

      -- Delete the given node
      Delete_Middle_Node (List (4));

      -- Verify that the node has been removed from the list
      Assert
        (Condition =>
           List (1).Data = 1 and
           List (1).Next.Data = 2 and
           List (1).Next.Next.Data = 3 and
           List (1).Next.Next.Next.Data = 5 and
           List (1).Next.Next.Next.Next.Data = 6,
         Message => "Test 1 failed");

      -- Dispose lists
      Dispose (List);

   end Test_Delete_Middle_Node;

end Ex_02_03_Delete_Middle_Node;
