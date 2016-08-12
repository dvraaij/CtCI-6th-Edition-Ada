with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Text_IO;      use Ada.Text_IO;

with Ada.Unchecked_Conversion;

package body Ex_08_05_Recursive_Multiply is

   ----------------------------------------------------------------------------
   function Recursive_Multiply (A, B : Integer) return Integer is

      -------------------------------------------------------------------------
      function Recurse (A, B : Unsigned) return Unsigned is

         -- Test if number is odd
         function Is_Odd (X : Unsigned) return Boolean is
           ((X and 2#1#) = 2#1#);

      begin

         -- (Debug) Show recursion
         -- Put_Line (Unsigned'Image (A) & " x " & Unsigned'Image (B));

         -- Determine result
         if B = 0 then
            return 0;
         elsif B = 2 then
            return Shift_Left (A, 1);
         elsif Is_Odd (B) then
            return Recurse (A, B-1) + A;
         else -- Is Even, factor out 2
            return Shift_Left (Recurse (A, Shift_Right (B, 1)), 1);
         end if;

      end Recurse;

      -- Conversion functions
      function Convert_To_Unsigned is
        new Ada.Unchecked_Conversion (Integer, Unsigned);
      function Convert_To_Integer is
        new Ada.Unchecked_Conversion (Unsigned, Integer);

      -- Input to recursion function (largest and smallest value)
      LV : Integer;
      SV : Integer;

   begin

      -- NOTE: Calling the Recurse function directly with parameters A and B
      --       without checking for B's magnitude and/or sign of B may increase
      --       the number of recursions significantly (although the result will
      --       be correct as a consequence of 2's complement arithmetic).

      -- (Preconditioning 1) Recurse using value closest to zero
      if abs (A) < abs (B) then
         LV := B;
         SV := A;
      else
         LV := A;
         SV := B;
      end if;

      -- (Preconditioning 2) Recurse down to zero
      if (SV < 0) then
         return Convert_To_Integer
           (Recurse
              (A => Convert_To_Unsigned (-LV) ,
               B => Convert_To_Unsigned (-SV) ));
      else
         return Convert_To_Integer
           (Recurse
              (A => Convert_To_Unsigned ( LV) ,
               B => Convert_To_Unsigned ( SV) ));
      end if;

   end Recursive_Multiply;


   ----------------
   -- Test cases --
   ----------------

   ----------------------------------------------------------------------------
   procedure Test_Recursive_Multiply
     (T : in out Test_Cases.Test_Case'Class)
   is
   begin

      Assert (Recursive_Multiply ( 0,   1) =   0, "Test 1 failed");
      Assert (Recursive_Multiply ( 1,   0) =   0, "Test 2 failed");

      Assert (Recursive_Multiply ( 5,   1) =   5, "Test 3 failed");
      Assert (Recursive_Multiply ( 5,   2) =  10, "Test 4 failed");
      Assert (Recursive_Multiply ( 5,  11) =  55, "Test 5 failed");
      Assert (Recursive_Multiply ( 5,  12) =  60, "Test 6 failed");

      Assert (Recursive_Multiply ( 5,  -1) =  -5, "Test 7 failed");
      Assert (Recursive_Multiply ( 5,  -2) = -10, "Test 8 failed");
      Assert (Recursive_Multiply ( 5, -11) = -55, "Test 9 failed");
      Assert (Recursive_Multiply ( 5, -12) = -60, "Test 10 failed");

      Assert (Recursive_Multiply (-5,   1) =  -5, "Test 11 failed");
      Assert (Recursive_Multiply (-5,   2) = -10, "Test 12 failed");
      Assert (Recursive_Multiply (-5,  11) = -55, "Test 13 failed");
      Assert (Recursive_Multiply (-5,  12) = -60, "Test 14 failed");

      Assert (Recursive_Multiply (-5,  -1) =   5, "Test 15 failed");
      Assert (Recursive_Multiply (-5,  -2) =  10, "Test 16 failed");
      Assert (Recursive_Multiply (-5, -11) =  55, "Test 17 failed");
      Assert (Recursive_Multiply (-5, -12) =  60, "Test 18 failed");

   end Test_Recursive_Multiply;

end Ex_08_05_Recursive_Multiply;
