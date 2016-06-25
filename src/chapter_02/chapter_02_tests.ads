with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Chapter_02_Tests is

   type Test_Case is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T : in out Test_Case);

   function Name (T : Test_Case) return Message_String;

end Chapter_02_Tests;
