with AUnit;                 use AUnit;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with CtCI.Linked_List_Node; use CtCI.Linked_List_Node;

package Ex_02_07_Intersection is

   ---------------
   -- Algorithm --
   ---------------

   function Intersection1 (LL1, LL2 : Node_Access) return Node_Access;
   function Intersection2 (LL1, LL2 : Node_Access) return Node_Access;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Intersection1 (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Intersection2 (T : in out Test_Cases.Test_Case'Class);

end Ex_02_07_Intersection;
