with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with Interfaces;       use Interfaces;

package Ex_05_03_Flip_Bit_To_Win is

   function Flip_Bit_To_Win (Value : Unsigned_16) return Natural;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Flip_Bit_To_Win
     (T : in out Test_Cases.Test_Case'Class);

end Ex_05_03_Flip_Bit_To_Win;
