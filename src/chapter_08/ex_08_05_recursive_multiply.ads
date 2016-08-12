with AUnit;                 use AUnit;
with AUnit.Test_Cases;      use AUnit.Test_Cases;

with System.Unsigned_Types; use System.Unsigned_Types;

package Ex_08_05_Recursive_Multiply is

   function Recursive_Multiply (A, B : Integer) return Integer;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Recursive_Multiply
     (T : in out Test_Cases.Test_Case'Class);

end Ex_08_05_Recursive_Multiply;
