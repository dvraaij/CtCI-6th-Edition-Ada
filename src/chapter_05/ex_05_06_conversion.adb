with AUnit.Assertions;      use AUnit.Assertions;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with Ada.Text_IO;           use Ada.Text_IO;

package body Ex_05_06_Conversion is

   ----------------------------------------------------------------------------
   function Conversion (Value_1, Value_2 : Unsigned_16) return Natural is

      Diff : Unsigned_16 := Value_1 xor Value_2;
      Ones : Natural     := 0;

   begin

      -- Count number of ones (i.e. bit differences)
      for K in 1 .. Diff'Size loop

         if (Diff and 1) = 1 then
            Ones := Ones + 1;
         end if;

         Diff := Shift_Right (Diff, 1);

      end loop;

      -- Return the number of bits to flip
      return Ones;

   end Conversion;

   ----------------
   -- Test cases --
   ----------------

   ----------------------------------------------------------------------------
   procedure Test_Conversion
     (T : in out Test_Cases.Test_Case'Class)
   is
   begin

      Assert ( Conversion (2#11101#, 2#01111#) = 2, "Test 1 failed");
      Assert ( Conversion (2#0#    , 2#0#    ) = 0, "Test 2 failed");
      Assert ( Conversion (2#11111#, 2#00000#) = 5, "Test 3 failed");

   end Test_Conversion;

end Ex_05_06_Conversion;
