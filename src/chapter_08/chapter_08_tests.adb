-- AUnit
with AUnit.Test_Cases; use AUnit.Test_Cases;

-- Exercises
with Ex_08_01_Triple_Step;     use Ex_08_01_Triple_Step;
with Ex_08_02_Robot_In_A_Grid; use Ex_08_02_Robot_In_A_Grid;

package body Chapter_08_Tests is

   procedure Register_Tests (T : in out Test_Case) is

      use AUnit.Test_Cases.Registration;

   begin

      Register_Routine
        (Test    => T,
         Routine => Test_Triple_Step'Access,
         Name    => "8.1 : Triple_Step");
      Register_Routine
        (Test    => T,
         Routine => Test_Robot_In_A_Grid'Access,
         Name    => "8.2 : Robot_In_A_Grid");

   end Register_Tests;

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("Chapter 8");
   end Name;

end Chapter_08_Tests;
