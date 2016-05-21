with AUnit;                 use AUnit;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with CtCI.Linked_List_Node; use CtCI.Linked_List_Node;

package Ex_2_8_Loop_Detection is

   ---------------
   -- Algorithm --
   ---------------

   function Loop_Detection (LL : Node_Access) return Node_Access;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Loop_Detection (T : in out Test_Cases.Test_Case'Class);

end Ex_2_8_Loop_Detection;
