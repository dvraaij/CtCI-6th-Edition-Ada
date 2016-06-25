with AUnit.Assertions;      use AUnit.Assertions;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with Ada.Text_IO;           use Ada.Text_IO;

package body Ex_03_01_Three_In_One is

   -- NOTE: The solution presented here deviates from the one in the book. The
   --       solution in the book involves a lot of copy actions when one
   --       of stacks needs to grow. The solution presented here uses a storage
   --       technique similar (i.e. not exactly) to which may be found in
   --       e.g. FAT16.

   type Stack_Array_Info;
   type Stack_Array;

   -- Storage ----------------------------------------------------------------

   Max_Segment  : constant := 8;
   Segment_Size : constant := 16;
   Storage_Size : constant := Max_Segment * Segment_Size;

   subtype Storage_Index is Natural range 0 .. (Storage_Size - 1);
   subtype Segment_Index is Natural range 0 .. (Max_Segment  - 1);
   subtype Offset_Index  is Natural range 0 .. (Segment_Size - 1);

   type Segment_Info is
      record
         Id       : Segment_Index;
         Owner    : access Stack_Array_Info;
         Previous : access Segment_Info;
      end record;

   type Storage_Array_Info is
     array (Segment_Index) of aliased Segment_Info;

   type Storage_Array is
     array (Storage_Index) of Integer;

   function Base (SI : access Segment_Info) return Storage_Index is
     (SI.Id * Segment_Size);

   Storage      : Storage_Array := (others => 0);
   Storage_Info : Storage_Array_Info;


   -- Stack ------------------------------------------------------------------

   type Stack_Array_Info is
      record
         Segment : access Segment_Info;
         Offset  : Offset_Index;
      end record;

   type Stack_Array is
     array (Stack_Index) of aliased Stack_Array_Info;

   Stack : Stack_Array := (others => (Segment => null,
                                      Offset  => Offset_Index'Last));

   ----------------------------------------------------------------------------
   procedure Push (S : Stack_Index; Item : Integer) is

      -- Segment allocation
      procedure Allocate is
      begin

         for Segment of Storage_Info loop
            if Segment.Owner = null then

               -- Mark segment as used
               Segment.Owner    := Stack (S)'Access;
               Segment.Previous := Stack (S).Segment;

               -- Update stack info
               Stack (S).Segment := Segment'Access;
               Stack (S).Offset  := Offset_Index'First;

               -- Message for testing / debugging
             --Put_Line ("Segment " & Natural'Image (Segment.Id) &
             --            "   allocated for stack " & Natural'Image (S) );

               return;

            end if;
         end loop;

         -- Raise an exception if no free segment is available
         raise Storage_Error with "No free segment available";

      end Allocate;

   begin

      -- Allocate new segment if the current segment is full.
      if Stack (S).Offset /= Offset_Index'Last then
         Stack (S).Offset := Stack (S).Offset + 1;
      else
         Allocate;
      end if;

      -- Store item
      Storage (Base (Stack (S).Segment) + Stack (S).Offset) := Item;

   end Push;

   ----------------------------------------------------------------------------
   function Pop (S : Stack_Index) return Integer is

      -- Segment deacllocation
      procedure Deallocate is
      begin

         -- Mark segment as free
         Stack (S).Segment.Owner := null;

         -- Message for testing / debugging
       --Put_Line ("Segment " & Natural'Image (Stack (S).Segment.Id) &
       --             " deallocated for stack " & Natural'Image (S) );

         -- Update stack info
         Stack (S).Segment := Stack (S).Segment.Previous;
         Stack (S).Offset  := Offset_Index'Last;

      end Deallocate;

      -- Retrieve item
      Item : Integer := Peek (S);

   begin

      -- Deallocate segment if this was the last item within the segment.
      if Stack (S).Offset /= Offset_Index'First then
         Stack (S).Offset := Stack (S).Offset - 1;
      else
         Deallocate;
      end if;

      -- Return item
      return Item;

   end Pop;

   ----------------------------------------------------------------------------
   function Peek (S : Stack_Index) return Integer is
   begin

      -- Raise exception if stack is empty
      if Is_Empty (S) = True then
         raise Constraint_Error
           with "Stack " & Natural'Image (S) & " is empty";
      end if;

      -- Return item
      return Storage (Base (Stack (S).Segment) + Stack (S).Offset);

   end Peek;

   ----------------------------------------------------------------------------
   function Is_Empty (S : Stack_Index) return Boolean is
   begin
      return Stack (S).Segment = null;
   end;

   ----------------------------------------------------------------------------
   procedure Test_Three_In_One (T : in out Test_Cases.Test_Case'Class) is

      type Operation is (Push, Pop);

      type Mutation is
         record
            Op     : Operation;
            Stack  : Stack_Index;
            Repeat : Natural;
         end record;

      type Mutation_Array is
        array (Positive range <>) of Mutation;

      I  : Integer;

      M  : Mutation;
      MA : Mutation_Array :=
             (1 => (Push, 0,  10),
              2 => (Push, 1,   9),
              3 => (Push, 0,  80),
              4 => (Push, 2,  10),
              5 => (Pop , 0,  80),
              6 => (Pop,  0,  10),
              7 => (Push, 1,  50),
              8 => (Push, 0,  17),
              9 => (Pop , 0,  17));

   begin

      for k in MA'Range loop
         M := MA (k);
         for l in 1 .. M.Repeat loop
            case M.Op is
            when Push => Push (M.Stack, k);
            when Pop  => I := Pop (M.Stack);
            end case;
         end loop;
      end loop;

      Assert (Is_Empty (0) = True, "Test 1 failed");

   end Test_Three_In_One;

begin   -- Ex_3_1_Three_In_One

   -- Initialize storage info
   for k in Storage_Info'Range loop

      Storage_Info (k) :=
        (Id       => k,
         Owner    => null,
         Previous => null);

   end loop;

end Ex_03_01_Three_In_One;
