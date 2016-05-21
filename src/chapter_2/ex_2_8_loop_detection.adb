with AUnit.Assertions;      use AUnit.Assertions;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with Ada.Text_IO;           use Ada.Text_IO;
with CtCI.Linked_List_Node; use CtCI.Linked_List_Node;

package body Ex_2_8_Loop_Detection is

   ----------------------------------------------------------------------------
   -- Algorithm 1                                                            --
   --                                                                        --
   --    Comp. complexity : O(N)                                             --
   --    Space complexity : O(1)                                             --
   ----------------------------------------------------------------------------

   function Loop_Detection (LL : Node_Access) return Node_Access is

      C1 : Node_Access := LL;
      C2 : Node_Access := LL;

   begin

      loop
         -- Return if the list is finite
         if C1.Next = null or
           (C2.Next = null or else C2.Next.Next = null) then
            return null;
         end if;

         -- Move forward slow/fast
         C1 := C1.Next;
         C2 := C2.Next.Next;

         -- Detect collision
         exit when C1 = C2;

      end loop;

      -- Loop detected, reinitialize second cursor
      C2 := LL;

      -- Move forward again until collision
      while C1 /= C2 loop
         C1 := C1.Next;
         C2 := C2.Next;
      end loop;

      return C1;

   end Loop_Detection;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Loop_Detection (T : in out Test_Cases.Test_Case'Class) is

      List1  : Nodes := Linked_List (1);
      List2  : Nodes := Linked_List (2);
      List3  : Nodes := Linked_List (3);
      List11 : Nodes := Linked_List (11);

   begin

      -- Test linked-lists _without_ a loop
      Assert (Loop_Detection (List1  (1)) = null, "Test 1 failed");
      Assert (Loop_Detection (List2  (1)) = null, "Test 2 failed");
      Assert (Loop_Detection (List3  (1)) = null, "Test 3 failed");
      Assert (Loop_Detection (List11 (1)) = null, "Test 4 failed");

      -- Insert loops (deliberately corrupt lists)
      List1   (1).Next := List1  (1);
      List2   (2).Next := List2  (1);
      List3   (3).Next := List3  (2);
      List11 (11).Next := List11 (4);

      -- Test linked-lists _with_ a loop
      Assert (Loop_Detection (List1  (1)) = List1  (1), "Test 5 failed");
      Assert (Loop_Detection (List2  (1)) = List2  (1), "Test 6 failed");
      Assert (Loop_Detection (List3  (1)) = List3  (2), "Test 7 failed");
      Assert (Loop_Detection (List11 (1)) = List11 (4), "Test 8 failed");

      -- Dispose lists
      Dispose (List1 );
      Dispose (List2 );
      Dispose (List3 );
      Dispose (List11);

   end Test_Loop_Detection;

end Ex_2_8_Loop_Detection;
