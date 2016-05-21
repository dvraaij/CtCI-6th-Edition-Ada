------------------------------------------------------------------------------
--                                                                          --
--                           CtCI Code Library                              --
--                                                                          --
--                C t C I . L i n k e d _ L i s t _ N o d e                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This packge attempts to mimic the Java implementation as much as         --
-- possible. The data fields of the node type are public accessible as is   --
-- needed in some of the exercises.                                         --
--                                                                          --
-- This package is meant for use in solutions of the execises in the book   --
-- "Cracking the Coding Interview" and should not be used in production     --
-- software. Use the appropriate containers from the Ada Standard Library   --
-- instead.                                                                 --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package CtCI.Linked_List_Node is

   type Node;
   type Node_Access is access Node;

   type Node is
      record
         Next : Node_Access;
         Prev : Node_Access;
         Last : Node_Access;
         Data : Integer;
      end record;

   -- Additional type to easily handle linked-lists during testing.
   type Nodes is array (Positive range <>) of Node_Access;

   -----------------------
   -- Factory functions --
   -----------------------

   function Linked_List_Node return Node_Access;

   function Linked_List_Node
     (Data : Integer) return Node_Access;

   function Linked_List_Node
     (Data : Integer;
      Next : Node_Access) return Node_Access;

   function Linked_List_Node
     (Data : Integer;
      Next : Node_Access;
      Prev : Node_Access) return Node_Access;

   -- Additional function to easily generate linked-lists during testing.
   function Linked_List
     (N : Positive) return Nodes;

   -----------------------
   -- Dispose functions --
   -----------------------

   procedure Dispose is new Ada.Unchecked_Deallocation
     (Object => Node, Name => Node_Access);

    procedure Dispose
     (L : in out Nodes);

   -----------------------
   -- Utility functions --
   -----------------------

   procedure Set_Next
     (This : Node_Access;
      Next : Node_Access);

   procedure Set_Previous
     (This : Node_Access;
      Prev : Node_Access);

   function Print_Forward
     (This : Node_Access) return String;

   function Clone
     (This : Node_Access) return Node_Access;

end CtCI.Linked_List_Node;
