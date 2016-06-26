-- AUnit
with AUnit.Test_Cases; use AUnit.Test_Cases;

-- Exercises
with Ex_05_01_Insertion;       use Ex_05_01_Insertion;

with Ex_05_03_Flip_Bit_To_Win; use Ex_05_03_Flip_Bit_To_Win;

with Ex_05_05_Debugger;        use Ex_05_05_Debugger;
with Ex_05_06_Conversion;      use Ex_05_06_Conversion;
with Ex_05_07_Pairwise_Swap;   use Ex_05_07_Pairwise_Swap;

package body Chapter_05_Tests is

   procedure Register_Tests (T : in out Test_Case) is

      use AUnit.Test_Cases.Registration;

   begin

      Register_Routine
        (Test    => T,
         Routine => Test_Insertion'Access,
         Name    => "5.1 : Insertion");

      Register_Routine
        (Test    => T,
         Routine => Test_Flip_Bit_To_Win'Access,
         Name    => "5.3 : Flip_Bit_To_Win");

      Register_Routine
        (Test    => T,
         Routine => Test_Debugger'Access,
         Name    => "5.5 : Debugger");
      Register_Routine
        (Test    => T,
         Routine => Test_Conversion'Access,
         Name    => "5.6 : Conversion");
      Register_Routine
        (Test    => T,
         Routine => Test_Pairwise_Swap'Access,
         Name    => "5.7 : Pairwise_Swap");

   end Register_Tests;

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("Chapter 5");
   end Name;

end Chapter_05_Tests;
