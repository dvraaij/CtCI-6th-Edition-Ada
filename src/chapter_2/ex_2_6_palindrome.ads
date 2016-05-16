with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with Ada.Containers.Doubly_Linked_Lists;

package Ex_2_6_Palindrome is

   -------------------
   -- A linked-list --
   -------------------

   package LL_Natural is
     new Ada.Containers.Doubly_Linked_Lists (Natural);
   use LL_Natural;

   ---------------
   -- Algorithm --
   ---------------

   function Palindrome (LL : List) return Boolean;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Palindrome (T : in out Test_Cases.Test_Case'Class);

end Ex_2_6_Palindrome;
