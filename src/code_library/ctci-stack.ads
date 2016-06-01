------------------------------------------------------------------------------
--                                                                          --
--                           CtCI Code Library                              --
--                                                                          --
--                          C t C I . S t a c k                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This package attempts to mimic the Java implementation listed in         --
-- chapter 3 as much as possible.                                           --
--                                                                          --
-- This package is meant for use in solutions of the execises in the book   --
-- "Cracking the Coding Interview" and should not be used in production     --
-- software. Use the appropriate containers from the Ada Standard Library   --
-- instead.                                                                 --
--                                                                          --
------------------------------------------------------------------------------

generic
   type Element_Type is private;
package CtCI.Stack is

   type Stack is tagged private;

   procedure Push (S : in out Stack; Item : Element_Type);
   function  Pop  (S : in out Stack) return Element_Type;
   function  Peek (S :        Stack) return Element_Type;

   function Is_Empty (S : Stack) return Boolean
     with Inline;

   ----------
   -- Sort --
   ----------

   generic
      with function "<=" (Left, Right : Element_Type) return Boolean is <>;
   procedure Sort (S : in out Stack);

private

   type Stack_Node;
   type Stack_Node_Access is access Stack_Node;

   type Stack_Node is
      record
         Data : Element_Type;
         Next : Stack_Node_Access;
      end record;

   type Stack is tagged
      record
         Top : Stack_Node_Access;
      end record;

end CtCI.Stack;
