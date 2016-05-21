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

   function Linked_List_Node return Node_Access is
   begin
      return N : Node_Access := new Node do
         N.Last := N;
         N.Data := 0;
      end return;
   end Linked_List_Node;


   function Linked_List_Node
     (Data : Integer) return Node_Access
   is
   begin
      return N : Node_Access := new Node do
         N.Last := N;
         N.Data := Data;
      end return;
   end Linked_List_Node;


   function Linked_List_Node
     (Data : Integer;
      Next : Node_Access) return Node_Access
   is
   begin
      return N : Node_Access := new Node do
         N.Last := N;
         N.Data := Data;
         Set_Next (N, Next);
      end return;
   end Linked_List_Node;


   function Linked_List_Node
     (Data : Integer;
      Next : Node_Access;
      Prev : Node_Access) return Node_Access
   is
   begin
      return N : Node_Access := new Node do
         N.Last := N;
         N.Data := Data;
         Set_Next (N, Next);
         Set_Previous (N, Prev);
      end return;
   end Linked_List_Node;


   function Linked_List (N : Positive) return Nodes is
   begin
      return Result : Nodes (1 ..  N) do
         Result (N) :=  Linked_List_Node (N);
         for k in reverse 1 .. (N-1) loop
            Result (k) := Linked_List_Node (k, Result (k+1));
         end loop;
      end return;
   end Linked_List;

   -------------
   -- Methods --
   -------------

   procedure Dispose (L : in out Nodes) is
   begin
      for k in L'Range loop
         Dispose (L (k));
      end loop;
   end Dispose;


   procedure Set_Next
     (This : Node_Access;
      Next : Node_Access)
   is
   begin
      This.Next := Next;
      if Next = null then
         This.Last := This;
      else
         This.Last := Next.Last;
         if Next.Prev /= This then
            Set_Previous (Next, This);
         end if;
      end if;
   end Set_Next;


   procedure Set_Previous
     (This : Node_Access;
      Prev : Node_Access)
   is
   begin
      This.Prev := Prev;
      if Prev /= null and then Prev.Next /= This then
         Set_Next (Prev, This);
      end if;
   end Set_Previous;


   function Print_Forward
     (This : Node_Access) return String
   is
   begin
      if (This.Next /= null) then
         return  Integer'Image (This.Data) & "->" &
           Print_Forward (This.Next);
      else
         return Integer'Image (This.Data);
      end if;
   end Print_Forward;


   function Clone
     (This : Node_Access) return Node_Access
   is
      Next2 : Node_Access;
   begin
      if This.Next /= null then
         Next2 := Clone (This.Next);
      end if;
      return N : Node_Access do
         N := Linked_List_Node (This.Data, Next2);
      end return;
   end Clone;

end CtCI.Linked_List_Node;
