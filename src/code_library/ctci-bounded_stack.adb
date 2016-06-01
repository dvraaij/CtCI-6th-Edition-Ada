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

package body CtCI.Bounded_Stack is

   ----------------------------------------------------------------------------
   overriding
   procedure Push (S : in out Bounded_Stack; Item : Element_Type) is
   begin

      -- Check if stack is full
      if Is_Full (S) then
         raise Constraint_Error with "Stack is full";
      end if;

      -- Push to stack
      Push (Unbounded_Stack.Stack (S), Item);

      -- Increment item counter
      S.Count := S.Count + 1;

   end Push;

   ----------------------------------------------------------------------------
   overriding
   function Pop  (S : in out Bounded_Stack) return Element_Type is

      Item : Element_Type;

   begin

      -- Pop from stack
      Item := Pop (Unbounded_Stack.Stack (S));

      -- Decrement item counter
      S.Count := S.Count - 1;

      -- Return item
      return Item;

   end Pop;

   ----------------------------------------------------------------------------
   overriding
   function Peek (S : Bounded_Stack) return Element_Type is
   begin
      return Peek (Unbounded_Stack.Stack (S));
   end Peek;

   ----------------------------------------------------------------------------
   overriding
   function Is_Empty (S : Bounded_Stack) return Boolean is
   begin
      return Is_Empty (Unbounded_Stack.Stack (S));
   end Is_Empty;

   ----------------------------------------------------------------------------
   not overriding
   function Is_Full (S : Bounded_Stack) return Boolean is
   begin
      return S.Count = S.Max_Items;
   end Is_Full;

   ----------------------------------------------------------------------------
   not overriding
   function Count (S : Bounded_Stack) return Natural is
   begin
      return S.Count;
   end Count;

end CtCI.Bounded_Stack;
