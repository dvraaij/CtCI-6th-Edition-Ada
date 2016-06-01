------------------------------------------------------------------------------
--                                                                          --
--                           CtCI Code Library                              --
--                                                                          --
--                          C t C I . S t a c k                             --
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

package body CtCI.Stack is

   procedure Dispose is new Ada.Unchecked_Deallocation
     (Object => Stack_Node, Name => Stack_Node_Access);

   ----------------------------------------------------------------------------
   procedure Push (S : in out Stack; Item : Element_Type) is
   begin

      -- New top node
      S.Top := new Stack_Node'(Data => Item,
                               Next => S.Top);
   end Push;

   ----------------------------------------------------------------------------
   function Pop  (S : in out Stack) return Element_Type is

      Top  : Stack_Node_Access;
      Item : Element_Type;

   begin

      -- Check if stack is not empty
      if S.Top = null then
         raise Constraint_Error with "Stack is empty";
      end if;

      -- Fetch top node
      Top := S.Top;

      -- Retrieve item and set next top node
      Item  := Top.Data;
      S.Top := Top.Next;

      -- Deallocate stack node
      Dispose (Top);

      -- Return item
      return Item;

   end Pop;

   ----------------------------------------------------------------------------
   function Peek (S : Stack) return Element_Type is
   begin

      -- Check if stack is not empty
      if S.Top = null then
         raise Constraint_Error with "Stack is empty";
      end if;

      -- Return top item
      return S.Top.Data;

   end Peek;

   ----------------------------------------------------------------------------
   function Is_Empty (S : Stack) return Boolean is
   begin
      return S.Top = null;
   end Is_Empty;

   ---------------------------------------------------------------------------
   procedure Sort (S : in out Stack) is

      type Predicate is
        access function (Left, Right : Element_Type) return Boolean;

      -- Predicate functions
      function Is_GE (Left, Right : Element_Type) return Boolean is
        (Right <= Left);
      function Is_SE (Left, Right : Element_Type) return Boolean is
        (Left <= Right);

      -- Transfer items from one stack to the other
      function Transfer (From, To : in out Stack;
                         Do_Hold  :        Predicate) return Boolean
      is
         Hold   : Element_Type := Pop (From);
         Sorted : Boolean      := True;
      begin

         while not Is_Empty (From) loop

            if Do_Hold (Peek (From), Hold) then
               Push (To, Hold);
               Hold := Pop (From);
            else
               Push (To, Pop (From));
               Sorted := False;
            end if;

         end loop;

         Push (To, Hold);

         return Sorted;

      end Transfer;

   begin

      -- Input checking
      if Is_Empty (S) then
         return;
      end if;

      -- Transfer to and from S2 until sorted
      declare
         S2     : Stack;
         Sorted : Boolean;
      begin
         loop
            Sorted := Transfer (S , S2, Is_GE'Access);
            Sorted := Transfer (S2, S , Is_SE'Access);
            exit when Sorted;
         end loop;
      end;

   end Sort;

end CtCI.Stack;
