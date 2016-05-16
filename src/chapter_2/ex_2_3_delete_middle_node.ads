with AUnit;                 use AUnit;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with CtCI.Linked_List_Node; use CtCI.Linked_List_Node;

package Ex_2_3_Delete_Middle_Node is

   ---------------
   -- Algorithm --
   ---------------

   procedure Delete_Middle_Node (N: aliased in out Node);

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Delete_Middle_Node (T : in out Test_Cases.Test_Case'Class);

end Ex_2_3_Delete_Middle_Node;
