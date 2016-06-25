with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with Ada.Unchecked_Deallocation;

package Ex_01_07_Rotate_Matrix is

   -----------------
   -- Matrix types --
   -----------------

   type Matrix is
     array (Positive range <>, Positive range <>) of aliased Integer;

   type Matrix_Access is access Matrix;

   procedure Dispose is new Ada.Unchecked_Deallocation
     (Object => Matrix,
      Name   => Matrix_Access);

   ----------------
   -- Algorithms --
   ----------------

   procedure Rotate_Matrix (M_Access : not null Matrix_Access);

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Rotate_Matrix (T : in out Test_Cases.Test_Case'Class);

end Ex_01_07_Rotate_Matrix;
