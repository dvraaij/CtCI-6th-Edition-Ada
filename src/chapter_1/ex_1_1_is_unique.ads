with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Ex_1_1_Is_Unique is

   ----------------
   -- Algorithms --
   ----------------

   function Is_Unique1 (S : String) return Boolean;
   function Is_Unique2 (S : String) return Boolean;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Is_Unique1 (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Is_Unique2 (T : in out Test_Cases.Test_Case'Class);

end Ex_1_1_Is_Unique;
