with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with Ada.Unchecked_Deallocation;

package Ex_1_7_Rotate_Matrix is

   -----------------
   -- Matrix types --
   -----------------

   type Matrix is
     array (Positive range <>, Positive range <>) of aliased Integer;

   type Matrix_Ptr is access Matrix;

   procedure Dispose is new Ada.Unchecked_Deallocation
     (Object => Matrix,
      Name   => Matrix_Ptr);

   ----------------
   -- Algorithms --
   ----------------

   procedure Rotate_Matrix (M_Ptr : not null Matrix_Ptr);

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Rotate_Matrix (T : in out Test_Cases.Test_Case'Class);

end Ex_1_7_Rotate_Matrix;
