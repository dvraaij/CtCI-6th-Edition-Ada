with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Ex_1_5_One_Away is

   ---------------
   -- Algorithm --
   ---------------

   function One_Away (S1, S2 : String) return Boolean;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_One_Away (T : in out Test_Cases.Test_Case'Class);

end Ex_1_5_One_Away;
