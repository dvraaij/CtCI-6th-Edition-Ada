-- AUnit
with AUnit.Test_Cases; use AUnit.Test_Cases;

-- Exercises
with Ex_01_01_Is_Unique;              use Ex_01_01_Is_Unique;
with Ex_01_02_Check_Permutation;      use Ex_01_02_Check_Permutation;
with Ex_01_03_Urlify;                 use Ex_01_03_Urlify;
with Ex_01_04_Palindrome_Permutation; use Ex_01_04_Palindrome_Permutation;
with Ex_01_05_One_Away;               use Ex_01_05_One_Away;
with Ex_01_06_String_Compression;     use Ex_01_06_String_Compression;
with Ex_01_07_Rotate_Matrix;          use Ex_01_07_Rotate_Matrix;
with Ex_01_08_Zero_Matrix;            use Ex_01_08_Zero_Matrix;
with Ex_01_09_String_Rotation;        use Ex_01_09_String_Rotation;

package body Chapter_01_Tests is

   procedure Register_Tests (T : in out Test_Case) is

      use AUnit.Test_Cases.Registration;

   begin

      Register_Routine
        (Test    => T,
         Routine => Test_Is_Unique1'Access,
         Name    => "1.1 : Is_Unique1");
      Register_Routine
        (Test    => T,
         Routine => Test_Is_Unique2'Access,
         Name    => "1.1 : Is_Unique2");
      Register_Routine
        (Test    => T,
         Routine => Test_Check_Permutation1'Access,
         Name    => "1.2 : Check_Permutation1");
      Register_Routine
        (Test    => T,
         Routine => Test_Check_Permutation2'Access,
         Name    => "1.2 : Check_Permutation2");
      Register_Routine
        (Test    => T,
         Routine => Test_URLify'Access,
         Name    => "1.3 : Urlify");
      Register_Routine
        (Test    => T,
         Routine => Test_Palindrome_Permutation'Access,
         Name    => "1.4 : Palindrome_Permutation");
      Register_Routine
        (Test    => T,
         Routine => Test_One_Away'Access,
         Name    => "1.5 : One_Away");
      Register_Routine
        (Test    => T,
         Routine => Test_String_Compression'Access,
         Name    => "1.6 : String_Compression");
      Register_Routine
        (Test    => T,
         Routine => Test_Rotate_Matrix'Access,
         Name    => "1.7 : Rotate_Matrix");
      Register_Routine
        (Test    => T,
         Routine => Test_Zero_Matrix1'Access,
         Name    => "1.8 : Zero_Matrix1");
      Register_Routine
        (Test    => T,
         Routine => Test_Zero_Matrix2'Access,
         Name    => "1.8 : Zero_Matrix2");
      Register_Routine
        (Test    => T,
         Routine => Test_String_Rotation'Access,
         Name    => "1.9 : String_Rotation");

   end Register_Tests;

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("Chapter 1");
   end Name;

end Chapter_01_Tests;
