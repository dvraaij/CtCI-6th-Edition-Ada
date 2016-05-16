with AUnit;                 use AUnit;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with CtCI.Linked_List_Node; use CtCI.Linked_List_Node;

package Ex_2_7_Intersection is

   ---------------
   -- Algorithm --
   ---------------

   function Intersection1 (LL1, LL2 : aliased in out Node) return access Node;
   function Intersection2 (LL1, LL2 : aliased in out Node) return access Node;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Intersection1 (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Intersection2 (T : in out Test_Cases.Test_Case'Class);

end Ex_2_7_Intersection;