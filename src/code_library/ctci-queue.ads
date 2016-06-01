------------------------------------------------------------------------------
--                                                                          --
--                           CtCI Code Library                              --
--                                                                          --
--                          C t C I . Q u e u e                             --
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
package CtCI.Queue is

   type Queue is tagged private;

   procedure Add    (Q : in out Queue; Item : Element_Type);
   function  Remove (Q : in out Queue) return Element_Type;
   function  Peek   (Q :        Queue) return Element_Type;

   function Is_Empty (Q : Queue) return Boolean
     with Inline;

private

   type Queue_Node;
   type Queue_Node_Access is access Queue_Node;

   type Queue_Node is
      record
         Data : Element_Type;
         Next : Queue_Node_Access;
      end record;

   type Queue is tagged
      record
         First : Queue_Node_Access;
         Last  : Queue_Node_Access;
      end record;

end CtCI.Queue;
