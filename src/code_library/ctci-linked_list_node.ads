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

package CtCI.Linked_List_Node is
   pragma Pure;

   type Node is
      record
         Next : access Node;
         Prev : access Node;
         Last : access Node;
         Data : Integer;
      end record;

   -----------------------
   -- Factory functions --
   -----------------------

   function Linked_List_Node return Node;

   function Linked_List_Node
     (Data : Integer) return Node;

    function Linked_List_Node
     (Data : Integer;
      Next : aliased in out Node) return Node;

   function Linked_List_Node
     (Data : Integer;
      Next : aliased in out Node;
      Prev : aliased in out Node) return Node;

   -----------------------
   -- Utility functions --
   -----------------------

   procedure Set_Next
     (This : aliased in out Node;
      Next : aliased in out Node);

   procedure Set_Previous
     (This : aliased in out Node;
      Prev : aliased in out Node);

   function Print_Forward
     (This : Node) return String;

end CtCI.Linked_List_Node;
