------------------------------------------------------------------------------
--                                                                          --
--                           CtCI Code Library                              --
--                                                                          --
--                  C t C I . B o u n d e d _ S t a c k                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This package is used in exercise 3.3.                                    --
--                                                                          --
-- This package is meant for use in solutions of the execises in the book   --
-- "Cracking the Coding Interview" and should not be used in production     --
-- software. Use the appropriate containers from the Ada Standard Library   --
-- instead.                                                                 --
--                                                                          --
------------------------------------------------------------------------------

private with CtCI.Stack;

generic
   type Element_Type is private;
package CtCI.Bounded_Stack is

   type Bounded_Stack (Max_Items : Positive) is private;

   procedure Push (S : in out Bounded_Stack; Item : Element_Type);
   function  Pop  (S : in out Bounded_Stack) return Element_Type;
   function  Peek (S :        Bounded_Stack) return Element_Type;

   function Is_Empty (S : Bounded_Stack) return Boolean
     with Inline;

   function Is_Full  (S : Bounded_Stack) return Boolean
     with Inline;

   function Count    (S : Bounded_Stack) return Natural
     with Inline;

private

   package Unbounded_Stack is
     new CtCI.Stack (Element_Type);
   use Unbounded_Stack;

   type Bounded_Stack (Max_Items : Positive) is new Unbounded_Stack.Stack with
      record
         Count : Natural := 0;
      end record;

end CtCI.Bounded_Stack;
