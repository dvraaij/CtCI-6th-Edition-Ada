with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with Ada.Containers.Bounded_Vectors;

package Ex_08_06_Towers_Of_Hanoi
is

   -----------
   -- Disks --
   -----------

   MAX_DISKS : constant := 6;

   subtype Disk is
     Integer range 1 .. MAX_DISKS;

   -----------
   -- Tower --
   -----------

   subtype Stack_Index is
     Integer range 1 .. MAX_DISKS;

   package Stack is
     new Ada.Containers.Bounded_Vectors (Stack_Index, Disk);
   use Stack;

   ---------------
   -- Algorithm --
   ---------------

   procedure Towers_Of_Hanoi (T1, T2, T3 : in out Vector);

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Towers_Of_Hanoi (T : in out Test_Cases.Test_Case'Class);

end Ex_08_06_Towers_Of_Hanoi;
