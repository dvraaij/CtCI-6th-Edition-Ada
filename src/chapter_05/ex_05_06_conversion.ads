with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with Interfaces;       use Interfaces;

package Ex_05_06_Conversion is

   function Conversion (Value_1, Value_2 : Unsigned_16) return Natural;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Conversion
     (T : in out Test_Cases.Test_Case'Class);

end Ex_05_06_Conversion;
