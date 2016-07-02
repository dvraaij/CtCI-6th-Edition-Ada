with Chapter_01_Tests;
with Chapter_02_Tests;
with Chapter_03_Tests;
with Chapter_04_Tests;
with Chapter_05_Tests;
with Chapter_15_Tests;

package body CtCI_Test_Suite is

   use AUnit.Test_Suites;

   -- Statically allocate test suite:
   Result : aliased Test_Suite;

   --  Statically allocate test cases:
   Test_01 : aliased Chapter_01_Tests.Test_Case;
   Test_02 : aliased Chapter_02_Tests.Test_Case;
   Test_03 : aliased Chapter_03_Tests.Test_Case;
   Test_04 : aliased Chapter_04_Tests.Test_Case;
   Test_05 : aliased Chapter_05_Tests.Test_Case;
   Test_15 : aliased Chapter_15_Tests.Test_Case;

   function Suite return Access_Test_Suite is
   begin

      Add_Test (Result'Access, Test_01'Access);
      Add_Test (Result'Access, Test_02'Access);
      Add_Test (Result'Access, Test_03'Access);
      Add_Test (Result'Access, Test_04'Access);
      Add_Test (Result'Access, Test_05'Access);
      Add_Test (Result'Access, Test_15'Access);

      return Result'Access;

   end Suite;

end CtCI_Test_Suite;
