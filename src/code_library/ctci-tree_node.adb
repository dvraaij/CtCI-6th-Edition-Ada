------------------------------------------------------------------------------
--                                                                          --
--                           CtCI Code Library                              --
--                                                                          --
--                       C t C I . T r e e _ N o d e                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
-- This package is used in chapter 4.                                       --
--                                                                          --
-- This package is meant for use in solutions of the execises in the book   --
-- "Cracking the Coding Interview" and should not be used in production     --
-- software. Use the appropriate containers from the Ada Standard Library   --
-- instead.                                                                 --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body CtCI.Tree_Node is

   ----------------------------------------------------------------------------
   procedure Set_Left_Child (This, Left : Tree_Node_Access) is
   begin
      This.Left := Left;
      if Left /= null then
         Left.Parent := This;
      end if;
   end Set_Left_Child;

   ----------------------------------------------------------------------------
   procedure Set_Right_Child (This, Right : Tree_Node_Access) is
   begin
      This.Right := Right;
      if Right /= null then
         Right.Parent := This;
      end if;
   end Set_Right_Child;

   ----------------------------------------------------------------------------
   procedure Insert_In_Order (This : Tree_Node_Access; D : Integer) is
   begin

      if D <= This.Data then

         if This.Left = null then
            Set_Left_Child (This, new Tree_Node'(D, others => null));
         else
            Insert_In_Order (This.Left, D);
         end if;

      else

         if This.Right = null then
            Set_Right_Child (This, new Tree_Node'(D, others => null));
         else
            Insert_In_Order (This.Right, D);
         end if;

      end if;

   end Insert_In_Order;

   ----------------------------------------------------------------------------
   function Is_BST (This : Tree_Node_Access) return Boolean is
   begin

      if (This.Left /= null) then
         if (This.Data < This.Left.Data) or not Is_BST (This.Left) then
            return False;
         end if;
      end if;

      if (This.Right /= null) then
         if (This.Data >= This.Right.Data) or not Is_BST (This.Right) then
            return False;
         end if;
      end if;

      return True;

   end Is_BST;

   ----------------------------------------------------------------------------
   function Height (This : Tree_Node_Access) return Positive is

      Height_Left  : Natural := 0;
      Height_Right : Natural := 0;

   begin

      Height_Left  := (if This.Left  /= null then Height (This.Left ) else 0);
      Height_Right := (if This.Right /= null then Height (This.Right) else 0);

      return 1 + Integer'Max (Height_Left, Height_Right);

   end Height;

   ----------------------------------------------------------------------------
   function Find
     (This : Tree_Node_Access;
      D    : Integer         ) return Tree_Node_Access
   is
   begin

      if (D = This.Data) then
         return This;
      elsif (D <= This.Data) then
         return (if This.Left  /= null then Find (This.Left , D) else null);
      elsif (D > This.Data) then
         return (if This.Right /= null then Find (This.Right, D) else null);
      end if;

      return null;

   end Find;

   ----------------------------------------------------------------------------
   function Create_Minimal_BST (A : Integer_Array) return Tree_Node_Access is

      Mid : Natural := (A'First + A'Last) / 2;

   begin

      return TN : Tree_Node_Access do
         if A'Length = 0 then
            TN := null;
         else
            TN := new Tree_Node'(A (Mid), others => null);
            Set_Left_Child  (TN, Create_Minimal_BST (A (A'First .. Mid-1)) );
            Set_Right_Child (TN, Create_Minimal_BST (A (Mid+1 .. A'Last))  );
         end if;
      end return;

   end Create_Minimal_BST;

   ----------------------------------------------------------------------------
   procedure Dispose (This : in out Tree_Node_Access) is

      procedure Dispose_Tree_Node is new Ada.Unchecked_Deallocation
        (Object => Tree_Node, Name => Tree_Node_Access);

   begin

      -- Dispose left subtree if exists
      if (This.Left /= null) then
         Dispose (This.Left);
      end if;

      -- Dispose left subtree if exists
      if (This.Right /= null) then
         Dispose (This.Right);
      end if;

      -- Dispose child references in parent node if existing
      if (This.Parent /= null) then
         if (This.Parent.Left = This) then
            This.Parent.Left := null;
         elsif (This.Parent.Right = This) then
            This.Parent.Right := null;
         end if;
      end if;

      Dispose_Tree_Node (This);

   end Dispose;

end CtCI.Tree_Node;
