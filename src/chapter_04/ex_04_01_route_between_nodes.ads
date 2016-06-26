with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with Ada.Containers.Doubly_Linked_Lists;

package Ex_04_01_Route_Between_Nodes is

   --------------------
   -- Directed Graph --
   --------------------

   generic
      type Node_Id is range <>;
   package Directed_Graph is

      package LL_Adjacent is
        new Ada.Containers.Doubly_Linked_Lists (Node_Id);
      use LL_Adjacent;

      type Node is
         record
            Marked   : Boolean          := False;
            Adjacent : LL_Adjacent.List := Empty_List;
         end record;

      type Graph is
        array (Node_Id) of Node;

      -- Find route via Depth-First Search (DFS)
      function Route_Between_Nodes_DFS
        (G    : in out Graph;
         From :        Node_Id;
         To   :        Node_Id) return Boolean;

      -- Find route via Breadth-First Search (BFS)
      function Route_Between_Nodes_BFS
        (G    : in out Graph;
         From :        Node_Id;
         To   :        Node_Id) return Boolean;

   end Directed_Graph;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Route_Between_Nodes_DFS
     (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Route_Between_Nodes_BFS
     (T : in out Test_Cases.Test_Case'Class);

end Ex_04_01_Route_Between_Nodes;
