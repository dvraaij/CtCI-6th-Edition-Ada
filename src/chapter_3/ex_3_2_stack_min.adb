with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Containers;   use Ada.Containers;

package body Ex_3_2_Stack_Min is

   ----------------------------------------------------------------------------
   procedure Push (S : in out Stack; Item : Integer) is
   begin

      -- Add to stack
      S.Data.Append (Item);

      -- Check if new minimum value
      if Is_Empty (S.Min) or else Item < Min (S) then
         S.Min.Append (Last (S.Data));
      end if;

   end Push;

   ----------------------------------------------------------------------------
   function Pop (S : in out Stack) return Integer is
      Item : Integer := Peek (S);
   begin

      -- Check if popped value is the currently minimum value
      if Last (S.Data) = Last_Element (S.Min) then
         S.Min.Delete_Last;
      end if;

      -- Remove from stack
      S.Data.Delete_Last;

      return Item;

   end Pop;

   ----------------------------------------------------------------------------
   function Peek (S : Stack) return Integer is
   begin
      return Last_Element (S.Data);
   end Peek;

   ----------------------------------------------------------------------------
   function Is_Empty (S : Stack) return Boolean is
   begin
      return Is_Empty (S.Data);
   end;

   ----------------------------------------------------------------------------
   function Min (S : Stack) return Integer is
   begin
      return Element (Last_Element (S.Min));
   end Min;

   ----------------------------------------------------------------------------
   procedure Test_Stack_Min (T : in out Test_Cases.Test_Case'Class) is

      S    : Stack;
      Item : Integer;

   begin

      -- Push some items onto the stack while asserting the minimum value
      Push (S, 8);
      Push (S, 5); Assert (Min (S) = 5, "Test 1 failed");
      Push (S, 5);
      Push (S, 2);
      Push (S, 9);
      Push (S, 4);
      Push (S, 1); Assert (Min (S) = 1, "Test 2 failed");

      -- Pop some items onto the stack while asserting the minimum value
      Item := Pop (S); Assert (Min (S) = 2, "Test 3 failed");
      Item := Pop (S);
      Item := Pop (S);
      Item := Pop (S); Assert (Min (S) = 5, "Test 4 failed");
      Item := Pop (S); Assert (Min (S) = 5, "Test 5 failed");
      Item := Pop (S); Assert (Min (S) = 8, "Test 6 failed");
      Item := Pop (S);

      -- Assert that the stack is empty again
      Assert (Is_Empty (S) = True, "Test 7 failed");

   end Test_Stack_Min;

end Ex_3_2_Stack_Min;
