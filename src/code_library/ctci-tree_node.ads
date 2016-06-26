------------------------------------------------------------------------------
--                                                                          --
--                           CtCI Code Library                              --
--                                                                          --
--                       C t C I . T r e e _ N o d e                        --
--                                                                          --
--                                 S p e c                                  --
-- This package is used in chapter 4.                                       --
--                                                                          --                                                                         --
-- This package is meant for use in solutions of the execises in the book   --
-- "Cracking the Coding Interview" and should not be used in production     --
-- software. Use the appropriate containers from the Ada Standard Library   --
-- instead.                                                                 --
--                                                                          --
------------------------------------------------------------------------------

package CtCI.Tree_Node is

   type Tree_Node;
   type Tree_Node_Access is access Tree_Node;

   type Tree_Node is
      record
         Data   : Integer;
         Left   : Tree_Node_Access;
         Right  : Tree_Node_Access;
         Parent : Tree_Node_Access;
      end record;

   ---------------------------
   -- Creation and Disposal --
   ---------------------------

   type Integer_Array is
     array (Natural range <>) of Integer;

   function Create_Minimal_BST
     (A : Integer_Array) return Tree_Node_Access;

   procedure Dispose
     (This : in out Tree_Node_Access);

   -------------
   -- Methods --
   -------------

   procedure Insert_In_Order
     (This : Tree_Node_Access; D : Integer);

   function Is_BST
     (This : Tree_Node_Access) return Boolean;

   function Height
     (This : Tree_Node_Access) return Positive;

   function Find
     (This : Tree_Node_Access;
      D    : Integer         ) return Tree_Node_Access;

end CtCI.Tree_Node;
