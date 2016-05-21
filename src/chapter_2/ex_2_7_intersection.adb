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
     (LL1, LL2 : Node_Access) return Node_Access is

      C1 : Node_Access;
      C2 : Node_Access;

   begin

      -- Initialize the pointer to the first list
      C1 := LL1;
      loop

         -- Initialize the pointer to the second list
         C2 := LL2;
         loop

            -- Compare references, return current reference is equal
            if C1 = C2 then
               return C1;
            end if;

            -- Exit the loop when at the end of the list
            exit when C2.Next = null;
            C2 := C2.Next;

         end loop;

         -- Exit the loop when at the end of the list
         exit when C1.Next = null;
         C1 := C1.Next;

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
     (LL1, LL2 : Node_Access) return Node_Access is

      Length1 : Natural := 0;
      Length2 : Natural := 0;

      C1 : Node_Access := LL1;
      C2 : Node_Access := LL2;

   begin

      -- Determine length of first list
      loop
         Length1 := Length1 + 1;
         exit when C1.Next = null;
         C1 := C1.Next;
      end loop;

      -- Determine length of second list
      loop
         Length2 := Length2 + 1;
         exit when C2.Next = null;
         C2 := C2.Next;
      end loop;

      -- Check for existance of intersection
      if C1 /= C2 then
         return null;
      end if;

      -- Reinitialize pointers to start of the list
      C1 := LL1;
      C2 := LL2;

      -- Fast forward pointer of longest list
      if Length1 > Length2 then

         for k in 1.. (Length1 - Length2) loop
            C1 := C1.Next;
         end loop;

      elsif Length2 > Length1 then

         for k in 1 .. (Length2 - Length1) loop
            C2 := C2.Next;
         end loop;

      end if;

      -- Start simultaneous comparing
      loop

         exit when C1 = C2;

         C1 := C1.Next;
         C2 := C2.Next;

      end loop;

      return C1;

   end Intersection2;


   ----------------
   -- Test cases --
   ----------------

   procedure Run_Test_Cases
     (Fcn_Access : access function
        (LL1, LL2 : Node_Access) return Node_Access) is

      List1 : Nodes := Linked_List(2);  -- Head 1
      List2 : Nodes := Linked_List(4);  -- Head 2
      List3 : Nodes := Linked_List(4);  -- Tail (common)

   begin

      -- Create intersection (assume singly linked-list)
      List1 (List1'Last).Next := List3 (List3'First);
      List2 (List2'Last).Next := List3 (List3'First);

      -- Test function
      Assert
        (Condition => Fcn_Access
           (List3 (List3'Last),
            List3 (List3'Last)) = List3 (List3'Last),
         Message   => "Test 1 failed");

      Assert
        (Condition => Fcn_Access
           (List1 (List1'First),
            List2 (List2'First)) = List3 (List3'First),
         Message   => "Test 2 failed");

       Assert
        (Condition => Fcn_Access
           (List2 (List2'First),
            List1 (List1'First)) = List3 (List3'First),
         Message   => "Test 3 failed");

      -- Dispose lists
      Dispose (List1);
      Dispose (List2);
      Dispose (List3);

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
