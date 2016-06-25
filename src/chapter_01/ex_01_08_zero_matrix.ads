with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with Ada.Unchecked_Deallocation;

package Ex_01_08_Zero_Matrix is

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

   procedure Zero_Matrix1 (M_Access : not null Matrix_Access);
   procedure Zero_Matrix2 (M_Access : not null Matrix_Access);

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Zero_Matrix1 (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Zero_Matrix2 (T : in out Test_Cases.Test_Case'Class);

end Ex_01_08_Zero_Matrix;
