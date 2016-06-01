------------------------------------------------------------------------------
--                                                                          --
--                           CtCI Code Library                              --
--                                                                          --
--                          C t C I . Q u e u e                             --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Unchecked_Deallocation;

package body CtCI.Queue is

   procedure Dispose is new Ada.Unchecked_Deallocation
     (Object => Queue_Node, Name => Queue_Node_Access);

   ----------------------------------------------------------------------------
   procedure Add (Q : in out Queue; Item : Element_Type) is

      Node : Queue_Node_Access := new Queue_Node'(Item, null);

   begin

      -- If the queue is not empty, then let the currently last (tail) node
      -- point to the new node being added (the 'new' last (tail) node)
      if Q.Last /= null then
         Q.Last.Next := Node;
      end if;

      -- Set the new node as the last node
      Q.Last := Node;

      -- If the queue was empty, the new node is the first and last node.
      if (Q.First = null) then
         Q.First := Q.Last;
      end if;

   end Add;

   ----------------------------------------------------------------------------
   function Remove (Q : in out Queue) return Element_Type is

      Node : Queue_Node_Access;
      Item : Element_Type;

   begin

      -- Raise an error is the queue is empty
      if (Q.First = null) then
         raise Constraint_Error with "No such element";
      end if;

      -- Obtain the node at the head of the queue
      Node := Q.First;

      -- Obtain the data to be returned and set the 'new' first (head) node
      -- to be the subsequent node.
      Item    := Node.Data;
      Q.First := Node.Next;

      -- Deallocate node
      Dispose (Node);

      -- If the queue is empty now, then there is no first or last node
      if Q.First = null then
         Q.Last := null;
      end if;

      -- Return data
      return Item;

   end Remove;

   ----------------------------------------------------------------------------
   function Peek (Q : Queue) return Element_Type is
   begin

      -- Raise an error is the queue is empty
      if (Q.First = null) then
         raise Constraint_Error with "No such element";
      end if;

      -- Return data of the first (head) node
      return Q.First.Data;

   end Peek;

   ----------------------------------------------------------------------------
   function Is_Empty (Q : Queue) return Boolean is
   begin
      return Q.First = null;
   end Is_Empty;

end CtCI.Queue;
