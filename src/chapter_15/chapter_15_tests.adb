-- AUnit
with AUnit.Test_Cases; use AUnit.Test_Cases;

-- Exercises
with Ex_15_02_Dining_Philosophers; use Ex_15_02_Dining_Philosophers;
with Ex_15_05_Call_In_Order;       use Ex_15_05_Call_In_Order;
with Ex_15_07_Fizz_Buzz;           use Ex_15_07_Fizz_Buzz;

package body Chapter_15_Tests is

   procedure Register_Tests (T : in out Test_Case) is

      use AUnit.Test_Cases.Registration;

   begin

      Register_Routine
        (Test    => T,
         Routine => Test_Dining_Philosophers'Access,
         Name    => "15.2 : Dining_Philosophers");
      Register_Routine
        (Test    => T,
         Routine => Test_Call_In_Order_1'Access,
         Name    => "15.5 : Call_In_Order_1");
      Register_Routine
        (Test    => T,
         Routine => Test_Call_In_Order_2'Access,
         Name    => "15.5 : Call_In_Order_2");
      Register_Routine
        (Test    => T,
         Routine => Test_Fizz_Buzz'Access,
         Name    => "15.7 : Fizz_Buzz");

   end Register_Tests;

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("Chapter 15");
   end Name;

end Chapter_15_Tests;
