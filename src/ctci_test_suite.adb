with Chapter_1_Tests;
with Chapter_2_Tests;

package body CtCI_Test_Suite is

   use AUnit.Test_Suites;

   -- Statically allocate test suite:
   Result : aliased Test_Suite;

   --  Statically allocate test cases:
   Test_1 : aliased Chapter_1_Tests.Test_Case;

   function Suite return Access_Test_Suite is
   begin

      Add_Test (Result'Access, Test_1'Access);

      return Result'Access;

   end Suite;

end CtCI_Test_Suite;
