with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Containers;   use Ada.Containers;

private with Ada.Containers.Doubly_Linked_Lists;

package Ex_3_2_Stack_Min is

   -----------
   -- Stack --
   -----------

   type Stack is private;

   -----------------
   -- Subprograms --
   -----------------

   procedure Push (S : in out Stack; Item : Integer);
   function  Pop  (S : in out Stack) return Integer;
   function  Peek (S :        Stack) return Integer;

   function Is_Empty (S : Stack) return Boolean;
   function Min      (S : Stack) return Integer;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Stack_Min (T : in out Test_Cases.Test_Case'Class);

private

   -- NOTE: In this exercise we use the doubly linked-list from the
   --       Ada Standard Library to implement a stack The standard library
   --       does not have a stack container.

   -- Linked-list (used as stack) for the actual (integer) data.
   package LL_Integer is
     new Ada.Containers.Doubly_Linked_Lists (Integer);
   use LL_Integer;

   -- Linked-list (used as stack) for the cursors to the minimal values.
   package LL_Cursor is
     new Ada.Containers.Doubly_Linked_Lists (Cursor);
   use LL_Cursor;

   -- The full stack type
   type Stack is
      record
         Data : LL_Integer.List;
         Min  : LL_Cursor.List;
      end record;

end Ex_3_2_Stack_Min;
