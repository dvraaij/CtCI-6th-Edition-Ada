with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Text_IO;      use Ada.Text_IO;

package body Ex_01_08_Zero_Matrix is

   ------------------
   -- Print matrix --
   ------------------

   procedure Put_Matrix (M_Access : not null Matrix_Access) is
   begin

      for p in M_Access'Range(1) loop
         for q in M_Access'Range(2) loop
            Put ( Integer'Image (M_Access (p,q)) );
         end loop;
         New_Line;
      end loop;
      New_Line;

   end Put_Matrix;

   ----------------------------------------------------------------------------
   -- Algorithm 1                                                            --
   --                                                                        --
   --    Comp. complexity : O(N*M)                                           --
   --    Space complexity : O(N+M)                                           --
   ----------------------------------------------------------------------------

   procedure Zero_Matrix1 (M_Access : not null Matrix_Access) is

      -- Index ranges of matrix
      subtype RI is Positive range M_Access'Range(1);
      subtype CI is Positive range M_Access'Range(2);

      -- Markers to indicate if a column/row contains one or more zeros
      Zero_Row    : array (RI) of Boolean := (others => False);
      Zero_Column : array (CI) of Boolean := (others => False);

   begin

      -- Input checking
      --   NONE

      -- Check matrix for zeros
      for p in RI'Range loop
         for q in CI'Range loop
            if M_Access (p,q) = 0 then

               Zero_Row(p)    := True;
               Zero_Column(q) := True;

            end if;
         end loop;
      end loop;

      -- Zero rows
      for p in RI loop
         if Zero_Row (p) then
            for q in CI loop
               M_Access (p,q) := 0;
            end loop;
         end if;
      end loop;

      -- Zero columns
      for q in CI loop
         if Zero_Column (q) then
            for p in RI loop
               M_Access (p,q) := 0;
            end loop;
         end if;
      end loop;

   end Zero_Matrix1;

   ----------------------------------------------------------------------------
   -- Algorithm 2                                                            --
   --                                                                        --
   --    Comp. complexity : O(N*M)                                           --
   --    Space complexity : O(1)                                             --
   ----------------------------------------------------------------------------

   procedure Zero_Matrix2 (M_Access : not null Matrix_Access) is

      -- Index ranges of matrix
      subtype RI is Positive range M_Access'Range(1);
      subtype CI is Positive range M_Access'Range(2);

      -- Markers to indicate if the first column/row should be zeroed
      Zero_First_Column : Boolean := False;
      Zero_First_Row    : Boolean := False;

   begin

      -- Input checking
      --   NONE

      -- Check first row and column for zeros
      Zero_First_Row :=
        (for some q in CI'Range => M_Access (RI'First, q) = 0);
      Zero_First_Column :=
        (for some p in RI'Range => M_Access (p, CI'First) = 0);

      -- Check the rest of the matrix
      for p in RI'First + 1 .. RI'Last loop
         for q in CI'First + 1 .. CI'Last loop
            if M_Access (p,q) = 0 then

               M_Access (p, CI'First) := 0;
               M_Access (RI'First, q) := 0;

            end if;
         end loop;
      end loop;

      -- Zero rows (except first)
      for p in RI'First + 1 .. RI'Last loop
         if M_Access (p, CI'First) = 0 then
            for q in CI loop
               M_Access (p,q) := 0;
            end loop;
         end if;
      end loop;

      -- Zero columns (except first)
      for q in CI'First + 1 .. CI'Last loop
         if M_Access (RI'First, q) = 0 then
            for p in RI loop
               M_Access (p,q) := 0;
            end loop;
         end if;
      end loop;

      -- Zero first row (if needed)
      if Zero_First_Row = True then
         for q in CI loop
            M_Access (RI'First, q) := 0;
         end loop;
      end if;

      -- Zero first column (if needed)
      if Zero_First_Column = True then
         for p in RI loop
            M_Access (p, CI'First) := 0;
         end loop;
      end if;

   end Zero_Matrix2;

   ----------------
   -- Test cases --
   ----------------

   procedure Run_Cases
     ( Proc_Access : access procedure (M_Access : not null Matrix_Access) ) is

      -- Allocate and initialize new matrices on the heap
      M_Access1 : Matrix_Access :=
        new Matrix'(1 => (1 => 1));

      M_Access2 : Matrix_Access :=
        new Matrix'(1 => ( 1, 2, 3, 4, 5 ),
                    2 => ( 1, 2, 3, 0, 5 ),
                    3 => ( 0, 2, 3, 4, 5 ),
                    4 => ( 1, 2, 3, 4, 5 ),
                    5 => ( 1, 2, 3, 4, 5 ));

      M_Access3 : Matrix_Access :=
        new Matrix'(1 => ( 1, 2, 3, 4, 5 ),
                    2 => ( 1, 2, 3, 0, 5 ),
                    3 => ( 1, 2, 3, 4, 5 ),
                    4 => ( 1, 0, 3, 4, 5 ),
                    5 => ( 1, 2, 3, 4, 5 ));

      -- Expected results
      Result1 : constant Matrix :=
        (1 => (1 => 1));

      Result2 : constant Matrix :=
        ( 1 => ( 0, 2, 3, 0, 5 ),
          2 => ( 0, 0, 0, 0, 0 ),
          3 => ( 0, 0, 0, 0, 0 ),
          4 => ( 0, 2, 3, 0, 5 ),
          5 => ( 0, 2, 3, 0, 5 ));

      Result3 : constant Matrix :=
        ( 1 => ( 1, 0, 3, 0, 5 ),
          2 => ( 0, 0, 0, 0, 0 ),
          3 => ( 1, 0, 3, 0, 5 ),
          4 => ( 0, 0, 0, 0, 0 ),
          5 => ( 1, 0, 3, 0, 5 ));

   begin

      -- Perform zeroing
      Proc_Access ( M_Access1 );
      Proc_Access ( M_Access2 );
      Proc_Access ( M_Access3 );

      -- Verify result
      Assert ( M_Access1.all = Result1, "Test 1 failed" );
      Assert ( M_Access2.all = Result2, "Test 2 failed" );
      Assert ( M_Access3.all = Result3, "Test 3 failed" );

      -- Deallocate matrix
      Dispose ( M_Access1 );
      Dispose ( M_Access2 );
      Dispose ( M_Access3 );

   end Run_Cases;

   procedure Test_Zero_Matrix1 (T : in out Test_Cases.Test_Case'Class) is
   begin
      Run_Cases ( Zero_Matrix1'Access );
   end Test_Zero_Matrix1;

   procedure Test_Zero_Matrix2 (T : in out Test_Cases.Test_Case'Class) is
   begin
      Run_Cases ( Zero_Matrix2'Access );
   end Test_Zero_Matrix2;

end Ex_01_08_Zero_Matrix;
