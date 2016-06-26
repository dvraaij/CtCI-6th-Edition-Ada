with AUnit.Assertions;      use AUnit.Assertions;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with Ada.Text_IO;           use Ada.Text_IO;

with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Doubly_Linked_Lists;

package body Ex_04_07_Build_Order is

   ----------
   -- Node --
   ----------

   type Project_Info;
   type Project_Info_Access is access Project_Info;

   -----------
   -- Graph --
   -----------

   package HM_Project_Info is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => Project                    ,
        Element_Type    => Project_Info_Access        ,
        Hash            => Ada.Strings.Unbounded.Hash ,
        Equivalent_Keys => "="                        );
   use HM_Project_Info;

   -----------
   -- Edges --
   -----------

   package LL_Cursor is
     new Ada.Containers.Doubly_Linked_Lists
       (Element_Type => HM_Project_Info.Cursor);

   ----------
   -- Node --
   ----------

   type State_Type is
     (Pending, Processing, Accepted);

   type Project_Info is
      record
         State      : State_Type     := Pending;
         Depends_On : LL_Cursor.List := LL_Cursor.Empty_List;
      end record;

   ----------------------------------------------------------------------------
   function Process_Input
     (P : Projects; D : Dependencies) return Map
   is

      use HM_Project_Info;

      Project_List : Map;
      C1,C2        : Cursor;

   begin

      -- Add projects
      for k in P'Range loop
         Project_List.Insert
           (Key      => P (k),
            New_Item => new Project_Info);
      end loop;

      -- Add dependencies
      for k in D'Range loop

         -- Find ...
         C1 := Project_List.Find (D (k).Root      );
         C2 := Project_List.Find (D (k).Depends_On);

         -- Assert that dependency is valid
         if C1 = No_Element or C2 = No_Element then
            raise Constraint_Error with "Incorrect dependency";
         end if;

         -- Add dependency
         Project_List (C1).Depends_On.Append (C2);

      end loop;

      -- Return the result
      return Project_List;

   end Process_Input;

   ----------------------------------------------------------------------------
   procedure Add_Partial_Build_Order
     (Build_Order_Queue : in out LL_Cursor.List;
      Project_List      :        HM_Project_Info.Map;
      C0                :        HM_Project_Info.Cursor)
   is

      use LL_Cursor;

      Search_Queue              : List;
      Build_Order_Partial_Queue : List;

   begin

      -- NOTE: Using the BFS algorithm.

      -- Enqueue the starting node
      Search_Queue.Prepend (C0);

      -- Visit each node in the search queue
      while not Is_Empty (Search_Queue) loop
         declare
            C : HM_Project_Info.Cursor := Last_Element (Search_Queue);
         begin

            -- Remove node from queue
            Delete_Last (Search_Queue);

            -- If this node is
            --   Accepted  , then ignore it
            --   Processing, then we've found a circularity
            --   Pending   , then add it to the result queue
            case Project_List (C).State is

               when Accepted =>
                  null;

               when Processing =>
                  raise Program_Error with "Circularity found";

               when Pending =>

                  -- (Debug) Show node being visted
                  -- Put_Line ("Visiting node " & Node_Id'Image(Id));

                  -- Add to the result queue
                  Build_Order_Partial_Queue.Prepend (C);

                  -- Transfer adjacent nodes to the search queue
                  for CC in Iterate (Project_List (C).Depends_On) loop
                     Search_Queue.Prepend (Project_List (C).Depends_On (CC));
                  end loop;

                  -- Mark current node as being processed
                  Project_List (C).State := Processing;

            end case;

         end;
      end loop;

      -- Prepend path to build order and mark node as accepted
      for C in Iterate (Build_Order_Partial_Queue) loop
         Build_Order_Queue.Prepend (Build_Order_Partial_Queue (C));
         Project_List (Build_Order_Partial_Queue (C)).State := Accepted;
      end loop;

   end Add_Partial_Build_Order;

  -----------------------------------------------------------------------------
  function Determine_Build_Order
     (P : Projects; D : Dependencies) return Build_Order
   is

      Project_List      : Map := Process_Input (P, D);
      Build_Order_Queue : LL_Cursor.List;

   begin

      -- Add a partial build order for each project
      for C in Iterate (Project_List) loop
         Add_Partial_Build_Order (Build_Order_Queue, Project_List, C);
      end loop;

      -- Move result from build order queue to an array
      return BO : Build_Order (1 .. Positive (Build_Order_Queue.Length)) do
         for K in BO'Range loop
            BO (K) := Key (Build_Order_Queue.First_Element);
            Build_Order_Queue.Delete_First;
         end loop;
      end return;

   end Determine_Build_Order;

   ----------------
   -- Test Cases --
   ----------------

   ----------------------------------------------------------------------------
   procedure Test_Build_Order
     (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      null;
   end Test_Build_Order;

end Ex_04_07_Build_Order;
