with AUnit.Assertions;      use AUnit.Assertions;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with Ada.Text_IO;           use Ada.Text_IO;

package body Ex_05_07_Pairwise_Swap is

   ----------------------------------------------------------------------------
   function Pairwise_Swap (Value : Unsigned_16) return Unsigned_16 is

      Mask_Odd  : Unsigned_16 := 2#1010_1010_1010_1010#;
      Mask_Even : Unsigned_16 := 2#0101_0101_0101_0101#;

   begin

      return
        (Shift_Left  (Value, 1) and Mask_Odd ) or
        (Shift_Right (Value, 1) and Mask_Even);

   end Pairwise_Swap;

   ----------------
   -- Test cases --
   ----------------

   ----------------------------------------------------------------------------
   procedure Test_Pairwise_Swap
     (T : in out Test_Cases.Test_Case'Class)
   is
   begin

      -- Assert some trivials
      Assert
        (Pairwise_Swap (2#0101_0101_0101_0101#) = 2#1010_1010_1010_1010#,
         "Test 1 failed");
      Assert
        (Pairwise_Swap (2#1010_1010_1010_1010#) = 2#0101_0101_0101_0101#,
         "Test 2 failed");

      -- Assert some invariants
      Assert
        (Pairwise_Swap (2#1100_1100_1100_1100#) = 2#1100_1100_1100_1100#,
         "Test 4 failed");
      Assert
        (Pairwise_Swap (2#0011_0011_0011_0011#) = 2#0011_0011_0011_0011#,
         "Test 5 failed");
      Assert
        (Pairwise_Swap (2#0000_0000_0000_0000#) = 2#0000_0000_0000_0000#,
         "Test 6 failed");
       Assert
        (Pairwise_Swap (2#1111_1111_1111_1111#) = 2#1111_1111_1111_1111#,
         "Test 7 failed");

   end Test_Pairwise_Swap;

end Ex_05_07_Pairwise_Swap;
