with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Ex_1_3_Urlify is

   ---------------
   -- Algorithm --
   ---------------

   procedure URLify (S : in out String; Length : Positive);

   ----------------
   -- Test cases --
   ----------------

   procedure Test_URLify (T : in out Test_Cases.Test_Case'Class);

end Ex_1_3_Urlify;
