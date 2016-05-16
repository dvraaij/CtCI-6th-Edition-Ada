with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Containers;   use Ada.Containers;

with Ada.Containers.Doubly_Linked_Lists;

package body Ex_2_4_Partition is

   ---------------
   -- Algorithm --
   ---------------

   procedure Partition (LL : in out List; Threshold : Natural) is

      C      : Cursor := LL.First;
      C_Next : Cursor := Next (C);

   begin

      -- Input checking
      if LL = Empty_List or LL.Length = 1 then
         return;
      end if;

      -- Exit when at end of list
      while C /= No_Element loop

         -- Store subsequent node as current cursor position is lost after
         -- splicing.
         C_Next := Next (C);

         -- Prepend node to the head of the list if the element value is
         -- smaller than the threshold.
         if LL (C) < Threshold then
            LL.Splice (LL.First, LL, C);
         end if;

         -- Proceed to the next node
         C := C_Next;

      end loop;

   end Partition;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Partition (T : in out Test_Cases.Test_Case'Class) is

      type Elements_Array is array (Positive range <>) of Natural;

      function To_List (Elements : Elements_Array) return List is
      begin

         return Result : List := Empty_List do
            for E : Natural of Elements loop
               Result.Append (E);
            end loop;
         end return;

      end To_List;

      function To_Array (LL : List) return Elements_Array is

         Count : Positive;

      begin

         return Elements : Elements_Array (1 .. Integer (LL.Length)) do
            Count := Elements'First;
            for E : Natural of LL loop
               Elements (Count) := E;
               Count            := Count + 1;
            end loop;
         end return;

      end To_Array;

      Array1    : Elements_Array := (3, 5, 1, 6, 2, 4, 5, 8, 5, 10, 2, 1);
      Threshold : Natural := 5;
      List1     : List;

   begin

      -- Partition list
      List1 := To_List (Array1);
      Partition (List1, Threshold);
      Array1 := To_Array (List1);

      -- Verify result

      -- NOTE: The result is verified using a quantified expression
      --       (predicate logic) which can informally be stated as:
      --
      --    There EXISTS an element k SUCH THAT
      --       ALL elements before k are smaller         than the threshold AND
      --       ALL elements after  k are larger or equal than the threshold
      --
      --    Note that the actual value of k is irrelevant for this execise; the
      --    partition will be regardless of k being smaller, equal or larger
      --    than the threshold.

      Assert
        ((for some k in Array1'Range =>
            (for all l in Array1'First .. k-1 =>
               Array1 (l) < Threshold) and
            (for all m in k+1 .. Array1'Last =>
               Array1 (m) >= Threshold)) =
         True,
         "Test 1 failed");

   end Test_Partition;

end Ex_2_4_Partition;
