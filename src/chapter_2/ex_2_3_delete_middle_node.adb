with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Containers;   use Ada.Containers;

package body Ex_2_3_Delete_Middle_Node is

   ----------------------------------------------------------------------------
   -- Algorithm 1                                                            --
   --                                                                        --
   --    Comp. complexity : O(1)                                             --
   --    Space complexity : O(1)                                             --
   ----------------------------------------------------------------------------

   procedure Delete_Middle_Node (N : in out Node) is

      N_Next : Node_Ptr;

   begin

      -- Input checking
      if N.Next = null then
         raise Constraint_Error;
      end if;

      -- Node subsequent to the given node
      N_Next := N.Next;

      -- Copy content of subsequent node to the given node
      N.Next    := N_Next.Next;
      N.Element := N_Next.Element;

      -- Deallocate subsequent node
      Dispose (N_Next);

   end Delete_Middle_Node;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Delete_Middle_Node (T : in out Test_Cases.Test_Case'Class) is

      -- A very simple singly linkes-list
      Node6_Ptr : Node_Ptr := new Node'(null, 'f');
      Node5_Ptr : Node_Ptr := new Node'(Node6_Ptr, 'e');
      Node4_Ptr : Node_Ptr := new Node'(Node5_Ptr, 'd');
      Node3_Ptr : Node_Ptr := new Node'(Node4_Ptr, 'c');
      Node2_Ptr : Node_Ptr := new Node'(Node3_Ptr, 'b');
      Node1_Ptr : Node_Ptr := new Node'(Node2_Ptr, 'a');

      -- Pointer to the head (first node) of the list
      List_Ptr : Node_Ptr := Node1_Ptr;

   begin

      -- Delete the given node (with character 'd')
      Delete_Middle_Node (Node4_Ptr.all);

      -- Verify that the node has been removed from the list
      Assert
        (Condition =>
           List_Ptr.Element = 'a' and
           List_Ptr.Next.Element = 'b' and
           List_Ptr.Next.Next.Element = 'c' and
           List_Ptr.Next.Next.Next.Element = 'e' and
           List_Ptr.Next.Next.Next.Next.Element = 'f',
         Message => "Test1 failed");

      -- Deallocate teach node
      Dispose (Node6_Ptr);
      Dispose (Node5_Ptr);
      Dispose (Node4_Ptr);
      Dispose (Node3_Ptr);
      Dispose (Node2_Ptr);
      Dispose (Node1_Ptr);

   end Test_Delete_Middle_Node;

end Ex_2_3_Delete_Middle_Node;
