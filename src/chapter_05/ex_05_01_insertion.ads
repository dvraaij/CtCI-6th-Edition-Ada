with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with Interfaces;       use Interfaces;

package Ex_05_01_Insertion is

   function Insertion
     (N, M : Unsigned_16; I, J : Natural) return Unsigned_16;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Insertion
     (T : in out Test_Cases.Test_Case'Class);

end Ex_05_01_Insertion;
