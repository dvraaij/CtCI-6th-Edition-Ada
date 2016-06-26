with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with CtCI.Tree_Node;   use CtCI.Tree_Node;

package Ex_04_09_BST_Sequences is

   procedure BST_Sequences (TN : Tree_Node_Access);

   ----------------
   -- Test cases --
   ----------------

   procedure Test_BST_Sequences
     (T : in out Test_Cases.Test_Case'Class);

end Ex_04_09_BST_Sequences;
