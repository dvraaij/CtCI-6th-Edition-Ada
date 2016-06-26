with AUnit.Assertions;      use AUnit.Assertions;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with Ada.Text_IO;           use Ada.Text_IO;

with Interfaces;

package body Ex_05_01_Insertion is

   ----------------------------------------------------------------------------
   function Insertion
     (N, M : Unsigned_16; I, J : Natural) return Unsigned_16
   is
      Mask : Unsigned_16 := 0;

   begin

      for K in 0 .. (J-I) loop
         Mask := Shift_Left (Mask + 1, 1);
      end loop;

      -- Clear bits to be replaced, insert new ones and return result
      return (N and not Shift_Left (Mask, I)) or Shift_Left (M, I);

   end Insertion;

   ----------------
   -- Test cases --
   ----------------

   ----------------------------------------------------------------------------
   procedure Test_Insertion
     (T : in out Test_Cases.Test_Case'Class)
   is
   begin

      Assert
        (Insertion ( 2#100_0000_0000#, 2#10011#, 2, 6) = 2#100_0100_1100#,
        "Test 1 failed");

   end Test_Insertion;

end Ex_05_01_Insertion;
