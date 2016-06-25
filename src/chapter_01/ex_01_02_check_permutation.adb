with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Text_IO;      use Ada.Text_IO;

with Ada.Containers.Generic_Array_Sort;

package body Ex_01_02_Check_Permutation is

   ----------------------------------------------------------------------------
   -- Algorithm 1                                                            --
   --                                                                        --
   --    Comp. complexity : O(N log N)   (depends on the sort algorithm)     --
   --    Space complexity : O(1)                                             --
   ----------------------------------------------------------------------------

   function Check_Permutation1 (S1, S2 : String) return Boolean is

      -- Instantiate new generic procedure
      procedure String_Sort is
        new Ada.Containers.Generic_Array_Sort (Positive, Character, String);

      S1_Work : String := S1;
      S2_Work : String := S2;

   begin

      -- Input checking
      if S1'Length /= S2'Length then
         return False;
      elsif S1'Length = 0 or S2'Length = 0 then
         return False;
      end if;

      -- Sort strings
      String_Sort (S1_Work);
      String_Sort (S2_Work);

      -- Test for equality
      return S1_Work = S2_Work;

   end Check_Permutation1;


   ----------------------------------------------------------------------------
   -- Algorithm 2                                                            --
   --                                                                        --
   --    Comp. complexity : O(N)                                             --
   --    Space complexity : O(N)                                             --
   ----------------------------------------------------------------------------

   function Check_Permutation2 (S1, S2 : String) return Boolean is

      -- In Ada, the String type is by definition an array of Character which
      -- is by definition a character from the Latin-1 set.
      --
      --   see also: http://www.adaic.org/resources/add_content/standards/12rm/html/RM-A-1.html
      --
      Count : array (Character) of Integer := (others => 0);

   begin

      -- Input checking
      if S1'Length /= S2'Length then
         return False;
      elsif S1'Length = 0 or S2'Length = 0 then
         return False;
      end if;

      -- Try to cancel characters between strings
      for k in S1'Range loop
         Count (S1 (k)) := Count (S1 (k)) + 1;
         Count (S2 (k)) := Count (S2 (k)) - 1;
      end loop;

      -- Test if all characters cancelled
      return (for all k in Count'Range => Count (k) = 0);

   end Check_Permutation2;


   ----------------
   -- Test cases --
   ----------------

   procedure Run_Test_Cases
     (Fcn_Access : access function (S1, S2 : String) return Boolean)
   is
   begin

      Assert (Fcn_Access (""       , "a"      ) = False, "Test 1 Failed");
      Assert (Fcn_Access ("a"      , "ab"     ) = False, "Test 2 Failed");
      Assert (Fcn_Access ("aaaaaab", "baaaaaa") = True , "Test 3 Failed");
      Assert (Fcn_Access ("abcdefg", "cgfebad") = True , "Test 4 Failed");
      Assert (Fcn_Access ("abcdefg", "abcdffg") = False, "Test 5 Failed");

   end Run_Test_Cases;

   procedure Test_Check_Permutation1 (T : in out Test_Cases.Test_Case'Class) is
   begin
      Run_Test_Cases (Check_Permutation1'Access);
   end Test_Check_Permutation1;

   procedure Test_Check_Permutation2 (T : in out Test_Cases.Test_Case'Class) is
   begin
      Run_Test_Cases (Check_Permutation2'Access);
   end Test_Check_Permutation2;

end Ex_01_02_Check_Permutation;
