-- AUnit
with AUnit.Test_Cases; use AUnit.Test_Cases;

-- Exercises
with Ex_3_1_Three_In_One;    use Ex_3_1_Three_In_One;
with Ex_3_2_Stack_Min;       use Ex_3_2_Stack_Min;
with Ex_3_3_Stack_Of_Plates; use Ex_3_3_Stack_Of_Plates;
with Ex_3_4_Queue_Via_Stack; use Ex_3_4_Queue_Via_Stack;
with Ex_3_5_Sort_Stack;      use Ex_3_5_Sort_Stack;
with Ex_3_6_Animal_Shelter;  use Ex_3_6_Animal_Shelter;

package body Chapter_3_Tests is

   procedure Register_Tests (T : in out Test_Case) is

      use AUnit.Test_Cases.Registration;

   begin

      Register_Routine
        (Test    => T,
         Routine => Test_Three_In_One'Access,
         Name    => "3.1 : Three_In_One");
      Register_Routine
        (Test    => T,
         Routine => Test_Stack_Min'Access,
         Name    => "3.2 : Stack_Min");
      Register_Routine
        (Test    => T,
         Routine => Test_Stack_Of_Plates'Access,
         Name    => "3.3 : Stack_Of_Plates");
      Register_Routine
        (Test    => T,
         Routine => Test_Queue_Via_Stack'Access,
         Name    => "3.4 : Queue_Via_Stack");
      Register_Routine
        (Test    => T,
         Routine => Test_Sort_Stack'Access,
         Name    => "3.5 : Sort_Stack");
      Register_Routine
        (Test    => T,
         Routine => Test_Animal_Shelter'Access,
         Name    => "3.6 : Animal_Shelter");

   end Register_Tests;

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("Chapter 3");
   end Name;

end Chapter_3_Tests;
