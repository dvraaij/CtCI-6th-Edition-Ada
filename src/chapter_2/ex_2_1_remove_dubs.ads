with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Ex_2_1_Remove_Dubs is

   -------------------
   -- A linked-list --
   -------------------

   package LL_String is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);
   use LL_String;

   ---------------
   -- Algorithm --
   ---------------

   procedure Remove_Dups1 (LL : in out List);
   procedure Remove_Dups2 (LL : in out List);

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Remove_Dups1 (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Remove_Dups2 (T : in out Test_Cases.Test_Case'Class);

end Ex_2_1_Remove_Dubs;
