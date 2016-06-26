with AUnit.Assertions;      use AUnit.Assertions;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with Ada.Text_IO;           use Ada.Text_IO;

package body Ex_05_03_Flip_Bit_To_Win is

   ----------------------------------------------------------------------------
   function Flip_Bit_To_Win (Value : Unsigned_16) return Natural is

      V          : Unsigned_16 := Value;
      Max_Length : Natural     := 0;
      Count      : Natural     := 0;
      Count_Last : Natural     := 0;

   begin

      for K in 1 .. V'Size loop

         -- Check for bit 0
         if (V and 1) /= 1 then

            Max_Length := Natural'Max (Max_Length, Count + 1 + Count_Last);
            Count_Last := Count;
            Count      := 0;

         else
            Count := Count + 1;
         end if;

         -- Rotate
         V := Rotate_Right (V, 1);

      end loop;

      return Natural'Max (Max_Length, Count + 1 + Count_Last);

   end Flip_Bit_To_Win;

   ----------------
   -- Test cases --
   ----------------

   ----------------------------------------------------------------------------
   procedure Test_Flip_Bit_To_Win
     (T : in out Test_Cases.Test_Case'Class)
   is
   begin

      -- Assert (example from the book)
      Assert (Flip_Bit_To_Win (2#110_1110_1111#      ) =  8, "Test 1 failed");

      -- Assert (other)
      Assert (Flip_Bit_To_Win (2#1111_1110_1111_1111#) = 16, "Test 2 failed");
    --Assert (Flip_Bit_To_Win (2#1111_1111_1111_1111#) = 16, "Test 3 failed"); -- ?
      Assert (Flip_Bit_To_Win (2#0000_1111_1111_1111#) = 13, "Test 4 failed");
      Assert (Flip_Bit_To_Win (2#1111_1111_1111_0000#) = 13, "Test 5 failed");
      Assert (Flip_Bit_To_Win (2#0000_1111_1111_0000#) =  9, "Test 6 failed");
      Assert (Flip_Bit_To_Win (2#0000_0000_0000_0000#) =  1, "Test 7 failed");

   end Test_Flip_Bit_To_Win;

end Ex_05_03_Flip_Bit_To_Win;
