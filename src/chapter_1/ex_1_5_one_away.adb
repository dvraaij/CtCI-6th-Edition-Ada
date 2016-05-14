with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Text_IO;      use Ada.Text_IO;

package body Ex_1_5_One_Away is

   ----------------------------------------------------------------------------
   -- Algorithm 1                                                            --
   --                                                                        --
   --    Comp. complexity : O(N)  (with N the length of the shortest string) --
   --    Space complexity : O(1)                                             --
   ----------------------------------------------------------------------------

   function One_Away (S1, S2 : String) return Boolean is

      C1         : Positive := S1'First;
      C2         : Positive := S2'First;
      Found_Diff : Boolean  := False;

   begin

      -- Input checking
      if S1 = S2 then
         return True;
      elsif abs (S1'Length - S2'Length) > 1 then
         return False;
      end if;

      -- Algorithm
      while (C1 <= S1'Last and C2 <= S2'Last) loop

         -- If equal, just update cursors and continue
         if S1 (C1) = S2 (C2) then
            C1 := C1 + 1;
            C2 := C2 + 1;

         -- If we've already found a difference, then return
         elsif Found_Diff = True then
            return False;

         -- Allow the difference (insert/replace/delete), update cursors
         -- accordingly.
         else
            Found_Diff := True;

            if S1'Length = S2'Length then
               C1 := C1 + 1;
               C2 := C2 + 1;
            elsif S2'Length < S1'Length then
               C1 := C1 + 1;
            elsif S1'Length < S2'Length then
               C2 := C2 + 1;
            end if;

         end if;

      end loop;

      return True;

   end One_Away;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_One_Away (T : in out Test_Cases.Test_Case'Class) is
   begin

      Assert (One_Away ("pale" , "ple" ) = True , "Test 1 failed");
      Assert (One_Away ("pales", "pale") = True , "Test 2 failed");
      Assert (One_Away ("pale" , "bale") = True , "Test 3 failed");
      Assert (One_Away ("pale" , "bake") = False, "Test 4 failed");

   end Test_One_Away;

end Ex_1_5_One_Away;
