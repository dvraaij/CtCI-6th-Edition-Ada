-- AUnit
with AUnit.Test_Cases; use AUnit.Test_Cases;

-- Exercises
with Ex_02_01_Remove_Dubs;        use Ex_02_01_Remove_Dubs;
with Ex_02_02_Return_Kth_To_Last; use Ex_02_02_Return_Kth_To_Last;
with Ex_02_03_Delete_Middle_Node; use Ex_02_03_Delete_Middle_Node;
with Ex_02_04_Partition;          use Ex_02_04_Partition;
with Ex_02_05_Sum_Lists;          use Ex_02_05_Sum_Lists;
with Ex_02_06_Palindrome;         use Ex_02_06_Palindrome;
with Ex_02_07_Intersection;       use Ex_02_07_Intersection;
with Ex_02_08_Loop_Detection;     use Ex_02_08_Loop_Detection;

package body Chapter_02_Tests is

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
      Register_Routine
        (Test    => T,
         Routine => Test_Return_Kth_To_Last'Access,
         Name    => "2.2 : Return_Kth_To_Last");
      Register_Routine
        (Test    => T,
         Routine => Test_Delete_Middle_Node'Access,
         Name    => "2.3 : Delete_Middle_Node");
      Register_Routine
        (Test    => T,
         Routine => Test_Partition'Access,
         Name    => "2.4 : Partition");
      Register_Routine
        (Test    => T,
         Routine => Test_Sum_Lists1'Access,
         Name    => "2.5 : Sum_Lists1");
      Register_Routine
        (Test    => T,
         Routine => Test_Sum_Lists2'Access,
         Name    => "2.5 : Sum_Lists2");
      Register_Routine
        (Test    => T,
         Routine => Test_Palindrome'Access,
         Name    => "2.6 : Palindrome");
      Register_Routine
        (Test    => T,
         Routine => Test_Intersection1'Access,
         Name    => "2.7 : Intersection1");
      Register_Routine
        (Test    => T,
         Routine => Test_Intersection2'Access,
         Name    => "2.7 : Intersection2");
      Register_Routine
        (Test    => T,
         Routine => Test_Loop_Detection'Access,
         Name    => "2.8 : Loop_Detection");

   end Register_Tests;

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("Chapter 2");
   end Name;

end Chapter_02_Tests;
