with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Text_IO;      use Ada.Text_IO;

package body Ex_03_04_Queue_Via_Stack is

   ----------------------------------------------------------------------------
   procedure Transfer (Q : in out My_Queue) is
   begin

      while not Is_Empty (Q.Stacks (Add)) loop
         Push (Q.Stacks (Remove), Pop (Q.Stacks (Add)));
      end loop;

   end Transfer;

   ----------------------------------------------------------------------------
   procedure Add (Q : in out My_Queue; Item : Integer) is
   begin

      -- Push item
      Push (Q.Stacks (Add), Item);

   end Add;

   ----------------------------------------------------------------------------
   function Remove(Q : in out My_Queue) return Integer is
   begin

      -- Transfer new items if necessary
      if Is_Empty (Q.Stacks (Remove)) then
         Transfer (Q);
      end if;

      -- Pop item
      return Pop (Q.Stacks (Remove));

   end Remove;

   ----------------------------------------------------------------------------
   function Peek (Q : in out My_Queue) return Integer is
   begin

      -- Transfer new items if necessary
      if Is_Empty (Q.Stacks (Remove)) then
         Transfer (Q);
      end if;

      -- Peek item
      return Peek (Q.Stacks (Remove));

   end Peek;

   ----------------------------------------------------------------------------
   function Is_Empty (Q : My_Queue) return Boolean is
   begin
      return
        Is_Empty (Q.Stacks (Add   )) and
        Is_Empty (Q.Stacks (Remove));
   end Is_Empty;

   ----------------------------------------------------------------------------
   procedure Test_Queue_Via_Stack (T : in out Test_Cases.Test_Case'Class) is

      Q    : My_Queue;
      Item : Integer;

   begin

      -- Add some items
      for k in 1 .. 10 loop
         Add (Q, k);
      end loop;

      -- Assert that the next item is the first added
      Assert (Peek (Q) = 1, "Test 1 failed");

      -- Remove some items
      for k in 1 .. 5 loop
         Item := Remove (Q);
      end loop;

      -- Assert that the next item is 6
      Assert (Peek (Q) = 6, "Test 2 failed");

      -- Remove other items
      for k in 1 .. 5 loop
         Item := Remove (Q);
      end loop;

      -- Assert that the queue is empty again
      Assert (Is_Empty (Q) = True, "Test 3 failed");

   end Test_Queue_Via_Stack;

end Ex_03_04_Queue_Via_Stack;
