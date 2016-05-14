-- AUnit
with AUnit.Test_Cases; use AUnit.Test_Cases;

-- Exercises
with Ex_2_1_Remove_Dubs; use Ex_2_1_Remove_Dubs;

package body Chapter_2_Tests is

   procedure Register_Tests (T : in out Test_Case) is

      use AUnit.Test_Cases.Registration;

   begin

      Register_Routine
        (Test    => T,
         Routine => Test_Remove_Dups1'Access,
         Name    => "2.1 : Remove_Dups1");
      Register_Routine
        (Test    => T,
         Routine => Test_Remove_Dups1'Access,
         Name    => "2.1 : Remove_Dups2");

   end Register_Tests;

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("Chapter 2");
   end Name;

end Chapter_2_Tests;
