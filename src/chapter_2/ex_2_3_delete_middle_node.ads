with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with Ada.Unchecked_Deallocation;

package Ex_2_3_Delete_Middle_Node is

   --------------------------------------
   -- Types for buidling a linked-list --
   --------------------------------------

   -- NOTE: Using a custom defined linked-list instead of the linked-list
   --       container from the Ada standard library. Solution cannot be shown
   --       with the linked-list container type as pointer manipulation
   --       (i.e. manipulation of the "Next" field) is (for good reasons)
   --       hidden from the end-user in the container packages.

   type Node;
   type Node_Ptr is access Node;

   type Node is
      record
         Next    : Node_Ptr;
         Element : Character;
      end record;

   procedure Dispose is new Ada.Unchecked_Deallocation
     (Object => Node,
      Name   => Node_Ptr);

   ---------------
   -- Algorithm --
   ---------------

   procedure Delete_Middle_Node (N: in out Node) ;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Delete_Middle_Node (T : in out Test_Cases.Test_Case'Class);

end Ex_2_3_Delete_Middle_Node;
