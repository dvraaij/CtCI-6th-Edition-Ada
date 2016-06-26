with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with CtCI.Tree_Node;   use CtCI.Tree_Node;

package Ex_04_06_Successor is

   function Successor (TN : Tree_Node_Access) return Tree_Node_Access;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Successor (T : in out Test_Cases.Test_Case'Class);

end Ex_04_06_Successor;
