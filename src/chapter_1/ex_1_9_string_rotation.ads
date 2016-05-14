with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Ex_1_9_String_Rotation is

   ---------------
   -- Algorithm --
   ---------------

   -- NOTE: The function specification has (just for fun) been extended with
   --       a contract which may be used to prove the actual code using SPARK.

   function String_Rotation (S1, S2 : String) return Boolean with
      Pre  => S1'Length > 0 and S2'Length > 0,
      Post => String_Rotation'Result =
      (for some k in S1'Range =>
         (S1 (k .. S1'Last) & S1 (S1'First .. k) = S2));

   ----------------
   -- Test cases --
   ----------------

   procedure Test_String_Rotation (T : in out Test_Cases.Test_Case'Class);

end Ex_1_9_String_Rotation;
