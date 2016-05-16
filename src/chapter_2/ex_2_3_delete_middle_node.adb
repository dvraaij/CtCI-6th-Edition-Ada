with AUnit.Assertions;      use AUnit.Assertions;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with Ada.Text_IO;           use Ada.Text_IO;
with CtCI.Linked_List_Node; use CtCI.Linked_List_Node;

package body Ex_2_3_Delete_Middle_Node is

   ----------------------------------------------------------------------------
   -- Algorithm 1                                                            --
   --                                                                        --
   --    Comp. complexity : O(1)                                             --
   --    Space complexity : O(1)                                             --
   ----------------------------------------------------------------------------

   procedure Delete_Middle_Node (N : aliased in out Node) is
   begin

      -- Input checking
      if N.Next = null then
         raise Constraint_Error;
      end if;

      -- Copy content of subsequent node to the given node
      N.Data :=    N.Next.Data;
      Set_Next (N, N.Next.Next.all);

   end Delete_Middle_Node;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Delete_Middle_Node (T : in out Test_Cases.Test_Case'Class) is

      Node6 : aliased Node := Linked_List_Node (6);
      Node5 : aliased Node := Linked_List_Node (5, Node6);
      Node4 : aliased Node := Linked_List_Node (4, Node5);
      Node3 : aliased Node := Linked_List_Node (3, Node4);
      Node2 : aliased Node := Linked_List_Node (2, Node3);
      Node1 : aliased Node := Linked_List_Node (1, Node2);

   begin

      -- Delete the given node
      Delete_Middle_Node (Node4);

      -- Verify that the node has been removed from the list
      Assert
        (Condition =>
           Node1.Data = 1 and
           Node1.Next.Data = 2 and
           Node1.Next.Next.Data = 3 and
           Node1.Next.Next.Next.Data = 5 and
           Node1.Next.Next.Next.Next.Data = 6,
         Message => "Test1 failed");

   end Test_Delete_Middle_Node;

end Ex_2_3_Delete_Middle_Node;
