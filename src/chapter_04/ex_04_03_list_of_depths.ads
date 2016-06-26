with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with CtCI.Tree_Node;   use CtCI.Tree_Node;

with Ada.Containers.Doubly_Linked_Lists;

package Ex_04_03_List_Of_Depths is

   -- List of nodes
   package Node_List is
     new Ada.Containers.Doubly_Linked_Lists (Tree_Node_Access);
   use Node_List;

   -- List of (List of nodes)
   package Node_List_List is
     new Ada.Containers.Doubly_Linked_Lists (Node_List.List);
   use Node_List_List;

   function List_Of_Depths
     (TN : Tree_Node_Access) return Node_List_List.List;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_List_Of_Depths
     (T : in out Test_Cases.Test_Case'Class);

end Ex_04_03_List_Of_Depths;
