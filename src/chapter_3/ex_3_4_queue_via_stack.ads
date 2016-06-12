with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

private with CtCI.Stack;

package Ex_3_4_Queue_Via_Stack is

   -----------
   -- Queue --
   -----------

   type My_Queue is private;

   -----------------
   -- Subprograms --
   -----------------

   procedure Add      (Q : in out My_Queue; Item : Integer);
   function  Remove   (Q : in out My_Queue) return Integer;
   function  Peek     (Q : in out My_Queue) return Integer;
   function  Is_Empty (Q :        My_Queue) return Boolean;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Queue_Via_Stack
     (T : in out Test_Cases.Test_Case'Class);

private

   package Stack_Integer is
     new CtCI.Stack (Integer);
   use Stack_Integer;

   type Stack_Index is (Add, Remove);

   type Stack_Array is array (Stack_Index) of Stack_Integer.Stack;

   type My_Queue is
      record
         Stacks : Stack_Array;
      end record;

end Ex_3_4_Queue_Via_Stack;
