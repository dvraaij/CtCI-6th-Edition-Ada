with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Ex_15_07_Fizz_Buzz is

   ----------------
   -- Algorithms --
   ----------------

   procedure Fizz_Buzz;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Fizz_Buzz
     (T : in out Test_Cases.Test_Case'Class);

end Ex_15_07_Fizz_Buzz;
