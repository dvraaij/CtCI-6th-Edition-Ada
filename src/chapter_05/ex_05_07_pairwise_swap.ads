with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with Interfaces;       use Interfaces;

package Ex_05_07_Pairwise_Swap is

   function Pairwise_Swap (Value : Unsigned_16) return Unsigned_16;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Pairwise_Swap
     (T : in out Test_Cases.Test_Case'Class);

end Ex_05_07_Pairwise_Swap;
