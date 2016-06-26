with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with CtCI.Tree_Node;   use CtCI.Tree_Node;

package Ex_04_08_First_Common_Ancestor is

   function First_Common_Ancestor
     (Root, TN1, TN2 : Tree_Node_Access) return Tree_Node_Access;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_First_Common_Ancestor
     (T : in out Test_Cases.Test_Case'Class);

end Ex_04_08_First_Common_Ancestor;
