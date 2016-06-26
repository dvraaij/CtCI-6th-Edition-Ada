with AUnit.Assertions;      use AUnit.Assertions;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with Ada.Text_IO;           use Ada.Text_IO;

with Ada.Containers.Doubly_Linked_Lists;

package body Ex_04_01_Route_Between_Nodes is

   --------------------
   -- Directed Graph --
   --------------------

   package body Directed_Graph is

      -------------------------------------------------------------------------
      -- NOTE: Both DFS and BFS methods are implemented. The DFS methods has
      --       been implemented iteratively instead of recursively to clearly
      --       show the symmetry between the methods. Both differ only in the
      --       use of a stack (DFS) versus a queue (BFS).
      -------------------------------------------------------------------------

      -------------------------------------------------------------------------
      function Route_Between_Nodes_DFS
        (G    : in out Graph;
         From :        Node_Id;
         To   :        Node_Id) return Boolean
      is

         package LL_Stack is
           new Ada.Containers.Doubly_Linked_Lists (Node_Id);
         use LL_Stack;

         Stack : LL_Stack.List;

      begin

         -- Ensure all nodes are marked as not visited
         for Id in G'Range loop
            G (Id).Marked := False;
         end loop;

         -- Push the starting node onto the stack
         Stack.Append (From);

         -- Visit each node on the stack
         while not Is_Empty (Stack) loop
            declare
               Id : Node_Id := Last_Element (Stack);
            begin

               -- Remove node from stack
               Delete_Last (Stack);

               -- If not yet been visited
               if G (Id).Marked = False then

                  -- (Debug) Show node being visted
                  -- Put_Line ("Visiting node " & Node_Id'Image(Id));

                  -- Check if this is the destination node
                  if Id = To then
                     return True;
                  end if;

                  -- Transfer adjacent nodes to the stack
                  for C in Iterate (G (Id).Adjacent) loop
                     Stack.Append (G (Id).Adjacent (C));
                  end loop;

                  -- Mark current node as being visited
                  G (Id).Marked := True;

               end if;

            end;
         end loop;

         return False;

      end Route_Between_Nodes_DFS;

      -------------------------------------------------------------------------
      function Route_Between_Nodes_BFS
        (G    : in out Graph;
         From :        Node_Id;
         To   :        Node_Id) return Boolean
      is

         package LL_Queue is
           new Ada.Containers.Doubly_Linked_Lists (Node_Id);
         use LL_Queue;

         Queue : LL_Queue.List;

      begin

         -- Ensure all nodes are marked as not visited
         for Id in G'Range loop
            G (Id).Marked := False;
         end loop;

         -- Enqueue the starting node
         Queue.Prepend (From);

         -- Visit each node in the queue
         while not Is_Empty (Queue) loop
            declare
               Id : Node_Id := Last_Element (Queue);
            begin

               -- Remove node from queue
               Delete_Last (Queue);

               -- If not yet been visited
               if G (Id).Marked = False then

                  -- (Debug) Show node being visted
                  -- Put_Line ("Visiting node " & Node_Id'Image(Id));

                  -- Check if this is the destination node
                  if Id = To then
                     return True;
                  end if;

                  -- Transfer adjacent nodes to the queue
                  for C in Iterate (G (Id).Adjacent) loop
                     Queue.Prepend (G (Id).Adjacent (C));
                  end loop;

                  -- Mark current node as being visited
                  G (Id).Marked := True;

               end if;

            end;
         end loop;

         return False;

      end Route_Between_Nodes_BFS;

   end Directed_Graph;

   ----------------
   -- Test Cases --
   ----------------

   -- Declare a graph with 10 nodes
   subtype Node_Id is Positive range 1 .. 10;

   package Graph_10 is
     new Directed_Graph (Node_Id);
   use Graph_10;

   ----------------------------------------------------------------------------
   procedure Run_Test_Cases
     (Fcn_Access : access function (G    : in out Graph;
                                    From :        Node_Id;
                                    To   :        Node_Id) return Boolean)
   is
      G : Graph;
   begin

      -- A directed graph (see also "ex_4_1_route_between_nodes.pdf")
      G ( 1).Adjacent.Append( 2);
      G ( 2).Adjacent.Append( 3);
      G ( 3).Adjacent.Append( 2);
      G ( 3).Adjacent.Append( 3);
      G ( 3).Adjacent.Append( 9);
      G ( 4).Adjacent.Append( 3);
      G ( 4).Adjacent.Append( 5);
      G ( 5).Adjacent.Append( 6);
      G ( 6).Adjacent.Append( 5);
      G ( 6).Adjacent.Append( 7);
      G ( 7).Adjacent.Append(10);
      G ( 8).Adjacent.Append( 1);
      G ( 8).Adjacent.Append( 8);
      G ( 8).Adjacent.Append( 9);
      G ( 9).Adjacent.Append( 7);
      G ( 9).Adjacent.Append(10);
      G (10).Adjacent.Append( 4);
      G (10).Adjacent.Append( 6);

      -- Assertions
      Assert ( Fcn_Access (G, 8, 6) = True , "Test 1 failed");
      Assert ( Fcn_Access (G, 2, 1) = False, "Test 2 failed");
      Assert ( Fcn_Access (G, 8, 8) = True , "Test 3 failed");
      Assert ( Fcn_Access (G, 3, 3) = True , "Test 4 failed");

   end Run_Test_Cases;

   ----------------------------------------------------------------------------
   procedure Test_Route_Between_Nodes_DFS
     (T : in out Test_Cases.Test_Case'Class) is
   begin
      Run_Test_Cases (Route_Between_Nodes_DFS 'Access);
   end Test_Route_Between_Nodes_DFS ;

   ----------------------------------------------------------------------------
   procedure Test_Route_Between_Nodes_BFS
     (T : in out Test_Cases.Test_Case'Class) is
   begin
      Run_Test_Cases (Route_Between_Nodes_BFS'Access);
   end Test_Route_Between_Nodes_BFS;

end Ex_04_01_Route_Between_Nodes;
