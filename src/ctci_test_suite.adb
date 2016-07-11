with Chapter_01_Tests;
with Chapter_02_Tests;
with Chapter_03_Tests;
with Chapter_04_Tests;
with Chapter_05_Tests;
with Chapter_08_Tests;

package body CtCI_Test_Suite is

   use AUnit.Test_Suites;

   -- Statically allocate test suite:
   Result : aliased Test_Suite;

   --  Statically allocate test cases:
   Test_1 : aliased Chapter_01_Tests.Test_Case;
   Test_2 : aliased Chapter_02_Tests.Test_Case;
   Test_3 : aliased Chapter_03_Tests.Test_Case;
   Test_4 : aliased Chapter_04_Tests.Test_Case;
   Test_5 : aliased Chapter_05_Tests.Test_Case;
   Test_8 : aliased Chapter_08_Tests.Test_Case;

   function Suite return Access_Test_Suite is
   begin

      Add_Test (Result'Access, Test_1'Access);
      Add_Test (Result'Access, Test_2'Access);
      Add_Test (Result'Access, Test_3'Access);
      Add_Test (Result'Access, Test_4'Access);
      Add_Test (Result'Access, Test_5'Access);
      Add_Test (Result'Access, Test_8'Access);

      return Result'Access;

   end Suite;

end CtCI_Test_Suite;
