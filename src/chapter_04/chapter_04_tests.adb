-- AUnit
with AUnit.Test_Cases; use AUnit.Test_Cases;

-- Exercises
with Ex_04_01_Route_Between_Nodes;   use Ex_04_01_Route_Between_Nodes;
with Ex_04_02_Minimal_Tree;          use Ex_04_02_Minimal_Tree;
with Ex_04_03_List_Of_Depths;        use Ex_04_03_List_Of_Depths;
with Ex_04_04_Check_Balanced;        use Ex_04_04_Check_Balanced;
with Ex_04_05_Validate_BST;          use Ex_04_05_Validate_BST;
with Ex_04_06_Successor;             use Ex_04_06_Successor;
with Ex_04_07_Build_Order;           use Ex_04_07_Build_Order;
with Ex_04_08_First_Common_Ancestor; use Ex_04_08_First_Common_Ancestor;

with Ex_04_10_Check_Subtree;         use Ex_04_10_Check_Subtree;

package body Chapter_04_Tests is

   procedure Register_Tests (T : in out Test_Case) is

      use AUnit.Test_Cases.Registration;

   begin

      Register_Routine
        (Test    => T,
         Routine => Test_Route_Between_Nodes_DFS'Access,
         Name    => "4.1 : Route_Between_Nodes_DFS");
      Register_Routine
        (Test    => T,
         Routine => Test_Route_Between_Nodes_BFS'Access,
         Name    => "4.1 : Route_Between_Nodes_BFS");
      Register_Routine
       (Test    => T,
         Routine => Test_Minimal_Tree'Access,
         Name    => "4.2 : Minimal_Tree");
      Register_Routine
        (Test    => T,
         Routine => Test_List_Of_Depths'Access,
         Name    => "4.3 : List_Of_Depths");
      Register_Routine
        (Test    => T,
         Routine => Test_Check_Balanced'Access,
         Name    => "4.4 : Check_Balanced");
      Register_Routine
        (Test    => T,
         Routine => Test_Validate_BST'Access,
         Name    => "4.5 : Validate_BST");
      Register_Routine
        (Test    => T,
         Routine => Test_Successor'Access,
         Name    => "4.6 : Successor");

      Register_Routine
        (Test    => T,
         Routine => Test_First_Common_Ancestor'Access,
         Name    => "4.8 : First_Common_Ancestor");

      Register_Routine
        (Test    => T,
         Routine => Test_Check_Subtree'Access,
         Name    => "4.10 : Check_Subtree");

   end Register_Tests;

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("Chapter 4");
   end Name;

end Chapter_04_Tests;
