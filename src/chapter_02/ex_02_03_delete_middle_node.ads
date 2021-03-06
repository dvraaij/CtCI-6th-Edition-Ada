with AUnit;                 use AUnit;
with AUnit.Test_Cases;      use AUnit.Test_Cases;

with CtCI.Linked_List_Node; use CtCI.Linked_List_Node;

package Ex_02_03_Delete_Middle_Node is

   ---------------
   -- Algorithm --
   ---------------

   procedure Delete_Middle_Node (N : Node_Access);

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Delete_Middle_Node (T : in out Test_Cases.Test_Case'Class);

end Ex_02_03_Delete_Middle_Node;
