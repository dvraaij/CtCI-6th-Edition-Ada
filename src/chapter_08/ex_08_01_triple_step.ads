with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Ex_08_01_Triple_Step is

   function Triple_Step (N : Positive) return Natural;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Triple_Step
     (T : in out Test_Cases.Test_Case'Class);

end Ex_08_01_Triple_Step;
