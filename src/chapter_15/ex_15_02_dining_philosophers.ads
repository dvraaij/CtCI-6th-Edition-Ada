with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Ex_15_02_Dining_Philosophers is

   ----------------
   -- Algorithms --
   ----------------

   procedure Dining_Philosophers;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Dining_Philosophers
     (T : in out Test_Cases.Test_Case'Class);

end Ex_15_02_Dining_Philosophers;
