-- AUnit
with AUnit.Test_Cases; use AUnit.Test_Cases;

-- Exercises
with Ex_08_01_Triple_Step;        use Ex_08_01_Triple_Step;
with Ex_08_02_Robot_In_A_Grid;    use Ex_08_02_Robot_In_A_Grid;
with Ex_08_03_Magic_Index;        use Ex_08_03_Magic_Index;
with Ex_08_05_Recursive_Multiply; use Ex_08_05_Recursive_Multiply;
with Ex_08_06_Towers_Of_Hanoi;    use Ex_08_06_Towers_Of_Hanoi;

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
      Register_Routine
        (Test    => T,
         Routine => Test_Magic_Index'Access,
         Name    => "8.3 : Magic_Index");
        Register_Routine
        (Test    => T,
         Routine => Test_Recursive_Multiply'Access,
         Name    => "8.5 : Recursive_Multiply");
      Register_Routine
        (Test    => T,
         Routine => Test_Towers_Of_Hanoi'Access,
         Name    => "8.6 : Tower_Of_Hanoi");

   end Register_Tests;

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("Chapter 8");
   end Name;

end Chapter_08_Tests;
