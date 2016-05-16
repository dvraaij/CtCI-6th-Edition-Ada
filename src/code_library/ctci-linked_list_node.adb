------------------------------------------------------------------------------
--                                                                          --
--                           CtCI Code Library                              --
--                                                                          --
--                C t C I . L i n k e d _ L i s t _ N o d e                 --
--                                                                          --
--                                 B o d y                                  --
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

package body CtCI.Linked_List_Node is

   ---------------------
   -- Factory methods --
   ---------------------

   function Linked_List_Node return Node is
   begin
      return Node'
        (Next => null,
         Prev => null,
         Last => null,
         Data => 0);
   end Linked_List_Node;


   function Linked_List_Node (Data : Integer) return Node is
   begin
      return Node'
        (Next => null,
         Prev => null,
         Last => null,
         Data => Data);
   end Linked_List_Node;


   function Linked_List_Node
     (Data : Integer;
      Next : aliased in out Node) return Node
   is
      N : aliased Node;
   begin
      N.Data := Data;
      Set_Next (N, Next);
      return N;
   end Linked_List_Node;


   function Linked_List_Node
     (Data : Integer;
      Next : aliased in out Node;
      Prev : aliased in out Node) return Node
   is
      N : aliased Node;
   begin
      N.Data := Data;
      Set_Next (N, Next);
      Set_Previous (N, Prev);
      return N;
   end Linked_List_Node;


   -------------
   -- Methods --
   -------------

   procedure Set_Next
     (This : aliased in out Node;
      Next : aliased in out Node)
   is
   begin
      This.Next := Next'Access;
      if This'Access = This.Last then
         This.Last := Next'Access;
      end if;
      if Next.Prev /= This'Access then
         Set_Previous (Next, This);
      end if;
   end Set_Next;


   procedure Set_Previous
     (This : aliased in out Node;
      Prev : aliased in out Node)
   is
   begin
      This.Prev := Prev'Access;
      if Prev.Next /= This'Access then
         Set_Next (Prev, This);
      end if;
   end Set_Previous;


   function Print_Forward
     (This : Node) return String
   is
   begin
      if (This.Next /= null) then
         return  Integer'Image (This.Data) & "->" &
           Print_Forward (This.Next.all);
      else
         return Integer'Image (This.Data);
      end if;
   end Print_Forward;

end CtCI.Linked_List_Node;
