with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Containers;   use Ada.Containers;

with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Ex_2_2_Return_Kth_To_Last is

   -------------------
   -- A linked-list --
   -------------------

   -- NOTE: The Ada stadard library has no singly-linked list. Just using
   --       the doubly-linked list here and play fair by not using features
   --       that are not available in a singly-linekd list.

   package LL_String is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);
   use LL_String;

   ---------------
   -- Algorithm --
   ---------------

   function Return_Kth_To_Last
     (LL             : List;
      Offset_To_Last : Count_Type) return String;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Return_Kth_To_Last (T : in out Test_Cases.Test_Case'Class);

end Ex_2_2_Return_Kth_To_Last;
