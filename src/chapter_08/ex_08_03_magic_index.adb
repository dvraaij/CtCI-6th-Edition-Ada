with AUnit.Assertions;      use AUnit.Assertions;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with Ada.Text_IO;           use Ada.Text_IO;

package body Ex_08_03_Magic_Index is

   ----------------------------------------------------------------------------
   function Magic_Index (A : Input_Array; MI : out Positive) return Boolean is

      -------------------------------------------------------------------------
      function Has_Magic_Index
        (L, U :     Positive;
         MI   : out Positive) return Boolean
      is
         M : Positive := (L + U) / 2;
      begin

         if A (M) = M then    -- Found a magic index?
            MI := M;
            return True;
         elsif L = U then     -- If not and range is single element
            return False;
         elsif A(M) > M then
            return Has_Magic_Index (L    , M - 1, MI);
         else
            return Has_Magic_Index (M + 1, U    , MI);
         end if;

      end;

   begin

      -- Range of array index and contents of (sorted) array must at least
      -- overlap to have a chance that a magic index to exist.
      --
      --   Index range                       +---------+
      --   Check 1 on content range      +-------+
      --   Check 2 on content range                +----------+
      if
        (A (A'First) <= A'First and A (A'Last) >= A'First) or
        (A (A'First) <= A'Last  and A (A'Last) >= A'Last )
      then
         return Has_Magic_Index (A'First, A'Last, MI);
      else
         return False;
      end if;

   end Magic_Index;

   ----------------
   -- Test cases --
   ----------------

   ----------------------------------------------------------------------------
   procedure Test_Magic_Index (T : in out Test_Cases.Test_Case'Class) is

      -- Unique magic indices
      Input_1 : Input_Array (1 .. 1) := (1 => 1);
      Input_2 : Input_Array (1 .. 1) := (1 => 0);
      Input_3 : Input_Array (1 .. 2) := (-2, -3);
      Input_4 : Input_Array (1 .. 2) := (-3,  2);
      Input_5 : Input_Array (1 .. 4) := (-4, -1, 2, 4);
      Input_6 : Input_Array (1 .. 5) := (-1,  1, 3, 2, 6);

      -- Multiple magic indices
      Input_7 : Input_Array (1 .. 4) := (1, 2, 3, 4);
      Input_8 : Input_Array (1 .. 5) := (1, 2, 3, 4, 5);

      -- Ouput variable with magic index (if found)
      MI : Positive;

   begin

      Assert (Magic_Index (Input_1, MI) = True  and then MI = 1, "Test 1 failed");
      Assert (Magic_Index (Input_2, MI) = False                , "Test 2 failed");
      Assert (Magic_Index (Input_3, MI) = False                , "Test 3 failed");
      Assert (Magic_Index (Input_4, MI) = True  and then MI = 2, "Test 4 failed");
      Assert (Magic_Index (Input_5, MI) = True  and then MI = 4, "Test 5 failed");
      Assert (Magic_Index (Input_6, MI) = True  and then MI = 3, "Test 6 failed");

      Assert (Magic_Index (Input_7, MI) = True  and then MI = 2, "Test 7 failed");
      Assert (Magic_Index (Input_8, MI) = True  and then MI = 3, "Test 8 failed");

   end Test_Magic_Index;

end Ex_08_03_Magic_Index;
