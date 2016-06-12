with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

private with CtCI.Bounded_Stack;

package Ex_3_3_Stack_Of_Plates is

   -----------
   -- Stack --
   -----------

   type Set_Of_Stacks is private;

   type Stack_Index is private;

   -----------------
   -- Subprograms --
   -----------------

   procedure Push
     (S    : in out Set_Of_Stacks;
      Item : Integer             );

   function Pop
     (S : in out Set_Of_Stacks)
      return Integer;

   function Pop_At
     (S     : in out Set_Of_Stacks;
      Index :        Stack_Index  )
      return Integer;

   function Peek
     (S : Set_Of_Stacks)
      return Integer;

   function Is_Empty
     (S : Set_Of_Stacks)
      return Boolean;

   function Is_Full
     (S : Set_Of_Stacks)
      return Boolean;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Stack_Of_Plates
     (T : in out Test_Cases.Test_Case'Class);

private

   package Bounded_Stack_Integer is
     new CtCI.Bounded_Stack (Integer);
   use Bounded_Stack_Integer;

   -- NOTE: Using a fixed array of statically allocated bounded stacks. One
   --       may use e.g. a linked-list (instead of an array) for an unbounded
   --       number of stacks. One may use an access type to dynamically
   --       allocate a new bounded stack.

   Max_Items  : constant := 10;
   Max_Stacks : constant :=  5;

   type Stack_Index is
     new Positive range 1 .. Max_Stacks;

   type Stack_Array is
     array (Stack_Index) of Bounded_Stack (Max_Items);

   type Set_Of_Stacks is
      record
         Last   : Stack_Index := Stack_Index'First;
         Stacks : Stack_Array;
      end record;

end Ex_3_3_Stack_Of_Plates;
