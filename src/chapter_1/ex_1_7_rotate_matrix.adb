with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Text_IO;      use Ada.Text_IO;

package body Ex_1_7_Rotate_Matrix is

   ------------------
   -- Print matrix --
   ------------------

   procedure Put_Matrix (M_Ptr : not null Matrix_Ptr) is
   begin

      for p in M_Ptr'Range(1) loop
         for q in M_Ptr'Range(2) loop
            Put ( Integer'Image (M_Ptr (p,q)) );
         end loop;
         New_Line;
      end loop;
      New_Line;

   end Put_Matrix;

   ----------------------------------------------------------------------------
   -- Algorithm 1                                                            --
   --                                                                        --
   --    Comp. complexity : O(N^2)                                           --
   --    Space complexity : O(1)                                             --
   ----------------------------------------------------------------------------

   procedure Rotate_Matrix (M_Ptr : not null Matrix_Ptr) is

      -- Mappings
      function Top (Layer, Index : Natural) return access Integer is
        (M_Ptr ( M_Ptr'First(1) + Layer ,
                 M_Ptr'First(2) + Index )'Access)
        with Inline;

      function Right (Layer, Index : Natural) return access Integer is
        (M_Ptr ( M_Ptr'First(1) + Index ,
                 M_Ptr'Last (2) - Layer )'Access)
        with Inline;

      function Bottom (Layer, Index : Natural) return access Integer is
        (M_Ptr ( M_Ptr'Last (1) - Layer ,
                 M_Ptr'Last (2) - Index )'Access)
        with Inline;

      function Left (Layer, Index : Natural) return access Integer is
        (M_Ptr ( M_Ptr'Last (1) - Index ,
                 M_Ptr'First(2) + Layer )'Access)
        with Inline;

      -- Matrix dimensions
      N : constant Positive := M_Ptr'Length(1);
      M : constant Positive := M_Ptr'Length(2);

      -- Temporary variable used during swapping
      Temp : Integer;

   begin

      -- Input checking
      if N /= M then
         raise Constraint_Error with "matrix must be square";
      elsif N = 1 and M = 1 then
         return;
      end if;

      -- Rotate CCW
      for Layer in 0 .. (N/2)-1 loop
         for Index in Layer .. (N-Layer-2) loop

            Temp                      := Top    (Layer, Index).all;
            Top    (Layer, Index).all := Right  (Layer, Index).all;
            Right  (Layer, Index).all := Bottom (Layer, Index).all;
            Bottom (Layer, Index).all := Left   (Layer, Index).all;
            Left   (Layer, Index).all := Temp;

         end loop;
      end loop;

   end Rotate_Matrix;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Rotate_Matrix (T : in out Test_Cases.Test_Case'Class) is

      -- Allocate and initialize new matrices on the heap
      M1_Ptr : Matrix_Ptr := new Matrix'(1..2 => ( 1, 2 ));
      M2_Ptr : Matrix_Ptr := new Matrix'(1..4 => ( 1, 2, 3, 4 ));
      M3_Ptr : Matrix_Ptr := new Matrix'(1..5 => ( 1, 2, 3, 4, 5 ));

      -- Expected results
      Result1 : constant Matrix :=
        ( 1 => (1..2 => 2),
          2 => (1..2 => 1));

      Result2 : constant Matrix :=
        ( 1 => (1..4 => 4),
          2 => (1..4 => 3),
          3 => (1..4 => 2),
          4 => (1..4 => 1));

      Result3 : constant Matrix :=
        ( 1 => (1..5 => 5),
          2 => (1..5 => 4),
          3 => (1..5 => 3),
          4 => (1..5 => 2),
          5 => (1..5 => 1));

   begin

      -- Perform rotation
      Rotate_Matrix ( M1_Ptr );
      Rotate_Matrix ( M2_Ptr );
      Rotate_Matrix ( M3_Ptr );

      -- Verify result
      Assert ( M1_Ptr.all = Result1, "Test 1 failed" );
      Assert ( M2_Ptr.all = Result2, "Test 2 failed" );
      Assert ( M3_Ptr.all = Result3, "Test 3 failed" );

      -- Deallocate matrices
      Dispose ( M1_Ptr );
      Dispose ( M2_Ptr );
      Dispose ( M3_Ptr );

   end Test_Rotate_Matrix;

end Ex_1_7_Rotate_Matrix;
