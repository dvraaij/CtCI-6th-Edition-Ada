with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Text_IO;      use Ada.Text_IO;

package body Ex_03_03_Stack_Of_Plates is

   ----------------------------------------------------------------------------
   procedure Push (S: in out Set_Of_Stacks; Item : Integer) is
   begin

      -- Check if current stack is full
      if Is_Full (S.Stacks (S.Last)) then

         if S.Last = Stack_Index'Last then
            raise Constraint_Error with "Stack is full";
         end if;

         S.Last := S.Last + 1;

      end if;

      -- Push item
      Push (S.Stacks (S.Last), Item);

   end Push;

   ----------------------------------------------------------------------------
   function Pop (S : in out Set_Of_Stacks) return Integer is

      Item : Integer;

   begin

      -- Pop item
      Item := Pop (S.Stacks (S.Last));

      -- Check if current (last) stack is empty
      if Is_Empty (S.Stacks (S.Last)) and not Is_Empty (S) then
         S.Last := S.Last - 1;
      end if;

      return Item;

   end Pop;

   ----------------------------------------------------------------------------
   function Pop_At (S     : in out Set_Of_Stacks;
                    Index :        Stack_Index  ) return Integer
   is
      Item : Integer;
   begin

      -- Pop item
      Item := Pop (S.Stacks (Index));

      -- Propagate
      declare
         Temp_Stack : Bounded_Stack (Max_Items);
      begin

         -- For each stack, from the Index'th till the next to last
         for k in Index .. (S.Last - 1) loop

            -- Get to the bottom of the next stack
            while Bounded_Stack_Integer.Count (S.Stacks (k+1)) > 1 loop
               Push (Temp_Stack, Pop (S.Stacks (k+1)));
            end loop;

            -- Transfer bottom item
            Push (S.Stacks (k), Pop (S.Stacks (k+1)));

            -- Put back the other items
            while not Is_Empty (Temp_Stack) loop
              Push (S.Stacks (k+1), Pop (Temp_Stack));
            end loop;

         end loop;

         -- Check if current (last) stack is empty
         if Is_Empty (S.Stacks (S.Last)) and not Is_Empty (S) then
            S.Last := S.Last - 1;
         end if;

      end;

      return Item;

   end Pop_At;

   ----------------------------------------------------------------------------
   function Peek (S : Set_Of_Stacks) return Integer
   is
   begin
      return Peek (S.Stacks (S.Last));
   end Peek;

   ----------------------------------------------------------------------------
   function Is_Empty (S : Set_Of_Stacks) return Boolean
   is
   begin
      return S.Last = Stack_Index'First and Is_Empty (S.Stacks (S.Last));
   end;

   ----------------------------------------------------------------------------
   function Is_Full (S : Set_Of_Stacks) return Boolean
   is
   begin
      return S.Last = Stack_Index'Last and Is_Full (S.Stacks (S.Last));
   end;

   ----------------------------------------------------------------------------
   procedure Test_Stack_Of_Plates (T : in out Test_Cases.Test_Case'Class) is

      S    : Set_Of_Stacks;
      Item : Integer;

   begin

      -- Push 41 items (9 left)
      for k in 1 .. 41 loop
         Push (S, k);
      end loop;

      -- Assert that this leaves us in the 5th stack
      Assert (S.Last = 5, "Test 1 failed");

      -- Pop an item somewhere in the middle
      Item := Pop_At (S, 2);

      -- Assert that this was the 20th item and 4 (full) stacks are left
      Assert (Item   = 20, "Test 2 failed");
      Assert (S.Last =  4, "Test 3 failed"); -- FIXME: Fails

      -- Push another 10 items (0 left)
      for k in 42 .. 51 loop
         Push (S, k);
      end loop;

      -- All stacks should be full now
      Assert (Is_Full (S), "Test 4 failed");

      -- Assert that an exception is raised when we try to push another item
      begin
         Push (S, 51);
         Assert (False, "Test 5 failed");
      exception
         when Constraint_Error => null;
      end;

      -- Pop all items and assert that item 20 was removed earlier.
      for k in 1 .. 50 loop
         Item := Pop (S);
         Assert (Item /= 20, "Test 6 failed");
      end loop;

      -- Assert that the stack is empty again
      Assert (Is_Empty (S) = True, "Test 7 failed");

   end Test_Stack_Of_Plates;

end Ex_03_03_Stack_Of_Plates;
