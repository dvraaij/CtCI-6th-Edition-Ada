with AUnit.Assertions;      use AUnit.Assertions;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with Ada.Text_IO;           use Ada.Text_IO;
with CtCI.Linked_List_Node; use CtCI.Linked_List_Node;

package body Ex_2_7_Intersection is

   ----------------------------------------------------------------------------
   -- Algorithm 1                                                            --
   --                                                                        --
   --    Comp. complexity : O(N*M)                                           --
   --    Space complexity : O(1)                                             --
   ----------------------------------------------------------------------------

   function Intersection1
     (LL1, LL2 : aliased in out Node) return access Node is

      Node_Ptr1 : access Node;
      Node_Ptr2 : access Node;

   begin

      -- Initialize the pointer to the first list
      Node_Ptr1 := LL1'Access;
      loop

         -- Initialize the pointer to the second list
         Node_Ptr2 := LL2'Access;
         loop

            -- Compare references, return current reference is equal
            if Node_Ptr1 = Node_Ptr2 then
               return Node_Ptr1;
            end if;

            -- Exit the loop when at the end of the list
            exit when Node_Ptr2.Next = null;
            Node_Ptr2 := Node_Ptr2.Next;

         end loop;

         -- Exit the loop when at the end of the list
         exit when Node_Ptr1.Next = null;
         Node_Ptr1 := Node_Ptr1.Next;

      end loop;

      return null;

   end Intersection1;

   ----------------------------------------------------------------------------
   -- Algorithm 2                                                            --
   --                                                                        --
   --    Comp. complexity : O(N+M)                                           --
   --    Space complexity : O(1)                                             --
   ----------------------------------------------------------------------------
   function Intersection2
     (LL1, LL2 : aliased in out Node) return access Node is

      Length1 : Natural := 0;
      Length2 : Natural := 0;

      Node_Ptr1 : access Node := LL1'Access;
      Node_Ptr2 : access Node := LL2'Access;

   begin

      -- Determine length of first list
      loop
         Length1 := Length1 + 1;
         exit when Node_Ptr1.Next = null;
         Node_Ptr1 := Node_Ptr1.Next;
      end loop;

      -- Determine length of second list
      loop
         Length2 := Length2 + 1;
         exit when Node_Ptr2.Next = null;
         Node_Ptr2 := Node_Ptr2.Next;
      end loop;

      Put_Line (" Length1 = " & Integer'Image (Length1));
      Put_Line (" Length2 = " & Integer'Image (Length2));

      -- Check for existance of intersection
      if Node_Ptr1 /= Node_Ptr2 then
         return null;
      end if;

      -- Reinitialize pointers to start of the list
      Node_Ptr1 := LL1'Access;
      Node_Ptr2 := LL2'Access;

      -- Fast forward pointer of longest list
      if Length1 > Length2 then

         for k in 1.. (Length1 - Length2) loop
            Node_Ptr1 := Node_Ptr1.Next;
         end loop;

      elsif Length2 > Length1 then

         for k in 1 .. (Length2 - Length1) loop
            Node_Ptr2 := Node_Ptr2.Next;
         end loop;

      end if;

      -- Start simultaneous comparing
      loop

         exit when Node_Ptr1 = Node_Ptr2;

         Node_Ptr1 := Node_Ptr1.Next;
         Node_Ptr2 := Node_Ptr2.Next;

      end loop;

      return Node_Ptr1;

   end Intersection2;


   ----------------
   -- Test cases --
   ----------------

   procedure Run_Test_Cases
     (Fcn_Ptr : access function
        (LL1, LL2 : aliased in out Node) return access Node) is

      -- Common
      Node8 : aliased Node := Linked_List_Node (8);
      Node7 : aliased Node := Linked_List_Node (7, Node8);
      Node6 : aliased Node := Linked_List_Node (6, Node7);
      Node5 : aliased Node := Linked_List_Node (5, Node6);

      -- Branch 1
      Node4_1 : aliased Node := Linked_List_Node (14, Node5);
      Node3_1 : aliased Node := Linked_List_Node (13, Node4_1);
      Node2_1 : aliased Node := Linked_List_Node (12, Node3_1);
      Node1_1 : aliased Node := Linked_List_Node (11, Node2_1);

      -- Branch 2
      Node2_2 : aliased Node := Linked_List_Node (22, Node5);
      Node1_2 : aliased Node := Linked_List_Node (21, Node2_2);

   begin

      Assert
        (Fcn_Ptr (Node8, Node8) = Node8'Unchecked_Access,
         "Test 1 failed");
      Assert
        (Fcn_Ptr (Node1_1, Node1_2) = Node5'Unchecked_Access,
         "Test 2 failed");
      Assert
        (Fcn_Ptr (Node1_2, Node1_1) = Node5'Unchecked_Access,
         "Test 3 failed");

   end Run_Test_Cases;

   procedure Test_Intersection1 (T : in out Test_Cases.Test_Case'Class) is
   begin
      Run_Test_Cases (Intersection1'Access);
   end Test_Intersection1;

   procedure Test_Intersection2 (T : in out Test_Cases.Test_Case'Class) is
   begin
      Run_Test_Cases (Intersection2'Access);
   end Test_Intersection2;

end Ex_2_7_Intersection;
