with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with Ada.Containers.Vectors;

package Ex_08_04_Power_Set is

   ---------------
   -- Set Types --
   ---------------

   package Set is
     new Ada.Containers.Vectors (Natural, Natural);

   package Set_Of_Sets is
     new Ada.Containers.Vectors (Natural, Set.Vector, Set."=");

   ---------------
   -- Algorithm --
   ---------------

   function Power_Set (S : Set.Vector) return Set_Of_Sets.Vector;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Power_Set (T : in out Test_Cases.Test_Case'Class);

end Ex_08_04_Power_Set;
