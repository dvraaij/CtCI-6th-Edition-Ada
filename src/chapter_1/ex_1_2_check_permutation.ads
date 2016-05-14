with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Ex_1_2_Check_Permutation is

   ----------------
   -- Algorithms --
   ----------------

   function Check_Permutation1 (S1, S2 : String) return Boolean;
   function Check_Permutation2 (S1, S2 : String) return Boolean;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Check_Permutation1 (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Check_Permutation2 (T : in out Test_Cases.Test_Case'Class);

end Ex_1_2_Check_Permutation;
