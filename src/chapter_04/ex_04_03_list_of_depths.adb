with AUnit.Assertions;      use AUnit.Assertions;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with Ada.Text_IO;           use Ada.Text_IO;

with CtCI.Tree_Node.Export; use CtCI.Tree_Node.Export;

with Ada.Containers; use Ada.Containers;

package body Ex_04_03_List_Of_Depths is

   ----------------------------------------------------------------------------
   function List_Of_Depths
     (TN : Tree_Node_Access) return Node_List_List.List
   is

      package NL  renames Node_List;
      package NLL renames Node_List_List;

      Result : NLL.List := NLL.Empty_List;

      -------------------------------------------------------------------------
      procedure Visit (TN : Tree_Node_Access; C : NLL.Cursor) is

         CC : NLL.Cursor := C;

      begin

         -- Do nothing if node is empty (null)
         if TN = null then
            return;
         end if;

         -- Add new list (at the end) if needed
         if (CC = NLL.No_Element) then
            Result.Insert (NLL.No_Element, NL.Empty_List, CC);
         end if;

         -- Add node to list
         Result (CC).Append (TN);

         -- Visit children
         Visit (TN.Left , Next (CC));
         Visit (TN.Right, Next (CC));

      end Visit;

   begin

      -- Visit and add nodes to list
      Visit (TN, NLL.No_Element);

      -- Result List
      return Result;

   end List_Of_Depths;

   ----------------
   -- Test cases --
   ----------------

   ----------------------------------------------------------------------------
   procedure Test_List_Of_Depths
     (T : in out Test_Cases.Test_Case'Class)
   is

      package NL  renames Node_List;
      package NLL renames Node_List_List;

      type Integer_Array is
        array (Natural range <>) of Integer;

      -------------------------------------------------------------------------
      function Create_Node_List
        (T : Tree_Node_Access;
         A : Integer_Array   ) return NL.List is
      begin
         return L : NL.List do
            for V of A loop
               L.Append (Find (T, V));
            end loop;
         end return;
      end Create_Node_List;

      -------------------------------------------------------------------------
      function Is_Equal (L1, L2 : NL.List) return Boolean is

         C1 : NL.Cursor := First (L1);
         C2 : NL.Cursor := First (L2);

      begin

         -- Assert lengths
         if Length (L1) /= Length (L2) then
            Put_Line ("Lengths incorrect (NL)");
            return False;
         end if;

         -- Assert contents
         loop

            -- Verify that nodes are equal
            if Element (C1) /= Element (C2) then

               Put_Line ("Nodes not equal:" &
                           Integer'Image (Element (C1).all.Data) & " vs." &
                           Integer'Image (Element (C2).all.Data) );

               return False;
            end if;

            -- Exit when this was the last node
            exit when C1 = Last (L1) and C2 = Last (L2);

            -- Next node
            Next (C1);
            Next (C2);

         end loop;

         return True;

      end Is_Equal;

      -------------------------------------------------------------------------
      function Is_Equal (L1, L2 : NLL.List) return Boolean is

         C1 : NLL.Cursor := First (L1);
         C2 : NLL.Cursor := First (L2);

      begin

         -- Assert lengths
         if Length (L1) /= Length (L2) then
            Put_Line ("Lengths incorrect (NLL)");
            return False;
         end if;

         -- Assert contents
         loop

            -- Verify that node lists are equal
            if not Is_Equal (L1 (C1), L2 (C2)) then
               return False;
            end if;

            -- Exit when this was the last node list
            exit when C1 = Last (L1) and C2 = Last (L2);

            -- Next node list
            Next (C1);
            Next (C2);

         end loop;

         return True;

      end Is_Equal;

      -- Trees for testing
      T1 : Tree_Node_Access :=
             Create_Minimal_BST ((1 => 0));

      T2 : Tree_Node_Access :=
             Create_Minimal_BST ((0, 1, 2, 3, 4, 5, 6));

      T3 : Tree_Node_Access :=
             Create_Minimal_BST ((0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12));

      -- Expected result
      Expect_1 : NLL.List;
      Expect_2 : NLL.List;
      Expect_3 : NLL.List;

   begin

      -- Export the test tree for visual inspection when debugging
      To_TikZ (T1, "ex_04_03_list_of_depths_T1.tex");
      To_TikZ (T2, "ex_04_03_list_of_depths_T2.tex");
      To_TikZ (T3, "ex_04_03_list_of_depths_T3.tex");

      -- Expected results
      Expect_1.Append ( Create_Node_List (T1, ( 0 => 0 )));

      Expect_2.Append ( Create_Node_List (T2, ( 0 => 3 )));
      Expect_2.Append ( Create_Node_List (T2, ( 1, 5 )));
      Expect_2.Append ( Create_Node_List (T2, ( 0, 2, 4, 6 )));

      Expect_3.Append ( Create_Node_List (T3, ( 0 => 6 )));
      Expect_3.Append ( Create_Node_List (T3, ( 2, 9 )));
      Expect_3.Append ( Create_Node_List (T3, ( 0, 4, 7, 11 )));
      Expect_3.Append ( Create_Node_List (T3, ( 1, 3, 5, 8, 10, 12 )));

      -- Assertions
      Assert (Is_Equal (List_Of_Depths(T1), Expect_1), "Test 1 failed");
      Assert (Is_Equal (List_Of_Depths(T2), Expect_2), "Test 2 failed");
      Assert (Is_Equal (List_Of_Depths(T3), Expect_3), "Test 3 failed");

      -- Dispose
      Dispose (T1);
      Dispose (T2);
      Dispose (T3);

   end Test_List_Of_Depths;

end Ex_04_03_List_Of_Depths;
