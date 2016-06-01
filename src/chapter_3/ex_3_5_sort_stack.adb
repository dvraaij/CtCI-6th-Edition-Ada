with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Text_IO;      use Ada.Text_IO;

with Ada.Numerics.Discrete_Random;
with CtCI.Stack;


package body Ex_3_5_Sort_Stack is

   ----------------------------------------------------------------------------
   procedure Test_Sort_Stack (T : in out Test_Cases.Test_Case'Class) is

      subtype Test_Range is Integer range -20 .. 20;

      package Random_Generator_Test_Range is
        new Ada.Numerics.Discrete_Random (Test_Range);
      use Random_Generator_Test_Range;

      package Stack_Test_Range is
        new CtCI.Stack (Test_Range);
      use Stack_Test_Range;

      -- Sort (ascending) procedure
      procedure Sort_Stack_Test_Range is
        new Sort;

      RG : Generator;
      S  : Stack;

   begin

      -- Add some random numbers
      for k in 1 .. 100 loop
         Push (S, Random (RG));
      end loop;

      -- Sort the stack (ascending)
      Sort_Stack_Test_Range (S);

      -- Assert proper ordering
      declare
         Previous : Test_Range := Pop (S);
      begin
         while not Is_Empty (S) loop
            Assert (Previous <= Peek (S), "Test 1 failed");
            Previous := Pop (S);
         end loop;
      end;

   end Test_Sort_Stack;

end Ex_3_5_Sort_Stack;
