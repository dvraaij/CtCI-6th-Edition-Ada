with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Ex_01_04_Palindrome_Permutation is

   ---------------
   -- Algorithm --
   ---------------

   function Palindrome_Permutation (S : String) return Boolean;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Palindrome_Permutation
     (T : in out Test_Cases.Test_Case'Class);

end Ex_01_04_Palindrome_Permutation;
