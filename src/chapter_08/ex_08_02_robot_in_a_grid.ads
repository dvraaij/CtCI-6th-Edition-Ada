with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Ex_08_02_Robot_In_A_Grid is

   function Robot_In_A_Grid return Boolean;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Robot_In_A_Grid
     (T : in out Test_Cases.Test_Case'Class);

end Ex_08_02_Robot_In_A_Grid;
