with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with Ada.Containers.Doubly_Linked_Lists;

package Ex_02_05_Sum_Lists is

   -------------------
   -- A linked-list --
   -------------------

   package LL_Natural is
     new Ada.Containers.Doubly_Linked_Lists (Natural);
   use LL_Natural;

   ---------------
   -- Algorithm --
   ---------------

   type Config is private;

   function Sum_Lists (LL1, LL2 : List; Fcn : Config) return List;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Sum_Lists1 (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Sum_Lists2 (T : in out Test_Cases.Test_Case'Class);

private

   -- Configuration: functions to use
   type Config is
      record
         List_To_Value : access function (LL : List) return Natural;
         Value_To_List : access function (V : Natural) return List;
      end record;

end Ex_02_05_Sum_Lists;
