with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package  Ex_1_6_String_Compression is

   ---------------
   -- Algorithm --
   ---------------

   function String_Compression (S : in String) return String;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_String_Compression
     (T : in out Test_Cases.Test_Case'Class);

end Ex_1_6_String_Compression;
