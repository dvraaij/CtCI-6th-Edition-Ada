with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Containers;   use Ada.Containers;

with Ada.Strings.Hash;
with Ada.Containers.Indefinite_Hashed_Sets;

package body Ex_2_1_Remove_Dubs is

   ----------------------------------------------------------------------------
   -- Algorithm 1                                                            --
   --                                                                        --
   --    Comp. complexity : O(N)                                             --
   --    Space complexity : O(N)                                             --
   ----------------------------------------------------------------------------

   -- NOTE: In the worst-case scenario (i.e. no duplicates), the hashed set
   --       will contain all N entries of the (linked) list.

   procedure Remove_Dups1 (LL : in out List) is

      -- Hashed set
      package HS_String is
        new Ada.Containers.Indefinite_Hashed_Sets
          (String, Ada.Strings.Hash, "=");
      use HS_String;

      HS : Set;

      -- Cursors
      C      : LL_String.Cursor;
      C_Next : LL_String.Cursor;

   begin

      -- Input checking
      if LL = Empty_List then
         return;
      end if;

      -- Initialize cursor to first element
      C := LL.First;

      loop

         -- Exit when at end of list
         exit when C = LL_String.No_Element;

         -- Next node that will be checked
         C_Next := LL_String.Next (C);

         -- Check if the element of the node element is a duplicate
         if HS.Contains ( LL (C) ) = True then
            LL.Delete (C);
         else
            HS.Insert (LL (C));
         end if;

         -- Next node to be checked
         C := C_Next;

      end loop;

   end Remove_Dups1;

   ----------------------------------------------------------------------------
   -- Algorithm 2                                                            --
   --                                                                        --
   --    Comp. complexity : O(N^2)                                           --
   --    Space complexity : O(1)                                             --
   ----------------------------------------------------------------------------

   procedure Remove_Dups2 (LL : in out List) is

      -- Cursors
      C1      : LL_String.Cursor;
      C2      : LL_String.Cursor;

      C2_Next : LL_String.Cursor;

   begin

      -- Input checking
      if LL = Empty_List then
         return;
      end if;

      -- Initialize cursor to first element
      C1 := LL.First;

      loop

         -- Exit when at end of list
         exit when C1 = LL_String.No_Element;

         -- Start comparing with next node
         C2 := LL_String.Next (C1);

         loop

            -- Exit when at end of list
            exit when C2 = LL_String.No_Element;

            -- Next element that will be checked
            C2_Next := LL_String.Next (C2);

            -- NOTE: To compare the elements, we cannot write
            --
            --         LL (C1) = LL (C2)
            --
            -- as the "=" operator is ambiguous: should it compare the
            -- node itself, or should it compare the element (by means of
            -- implicit dereferencing)? Hence, one should indicate explicitly
            -- that the element (in this case the string) must be compared.

            -- Check if element of each node are the same
            if LL_String.Element (C1) = LL_String.Element (C2) then
               LL.Delete (C2);
            end if;

            -- Next node to be checked
            C2 := C2_Next;

         end loop;

         -- Next node to be checked
         C1 := LL_String.Next (C1);

      end loop;

   end Remove_Dups2;

   ----------------
   -- Test cases --
   ----------------

   procedure Run_Test_Cases
     (Proc_Access : access procedure (LL : in out List)) is

      List1   : List;
      List2   : List;

      Result1 : List;
      Result2 : List;

   begin

      -- Lists
      List1.Append ("Amsterdam");

      List2.Append ("Amsterdam");
      List2.Append ("London");
      List2.Append ("New York");
      List2.Append ("Paris");
      List2.Append ("Amsterdam");
      List2.Append ("Tokyo");
      List2.Append ("Amsterdam");

      -- Expected results
      Result1.Append ("Amsterdam");

      Result2.Append ("Amsterdam");
      Result2.Append ("London");
      Result2.Append ("New York");
      Result2.Append ("Paris");
      Result2.Append ("Tokyo");

      -- Remove the duplicates
      Proc_Access (List1);
      Proc_Access (List2);

      -- Verify reuslt
      Assert (List1 = Result1, "Test 1 failed");
      Assert (List2 = Result2, "Test 2 failed");

   end Run_Test_Cases;

   procedure Test_Remove_Dups1 (T : in out Test_Cases.Test_Case'Class) is
   begin
      Run_Test_Cases (Remove_Dups1'Access);
   end Test_Remove_Dups1;

   procedure Test_Remove_Dups2 (T : in out Test_Cases.Test_Case'Class) is
   begin
      Run_Test_Cases (Remove_Dups2'Access);
   end Test_Remove_Dups2;

end Ex_2_1_Remove_Dubs;
