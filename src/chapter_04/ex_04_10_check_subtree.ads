with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with CtCI.Tree_Node;   use CtCI.Tree_Node;

package Ex_04_10_Check_Subtree is

   function Check_Subtree
     (TN1, TN2 : Tree_Node_Access) return Boolean;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Check_Subtree
     (T : in out Test_Cases.Test_Case'Class);

end Ex_04_10_Check_Subtree;
