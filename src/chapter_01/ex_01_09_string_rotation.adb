with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Text_IO;      use Ada.Text_IO;

package body Ex_01_09_String_Rotation is

   ----------------------------------------------------------------------------
   -- Algorithm 1                                                            --
   --                                                                        --
   --    Comp. complexity : O(N)                                             --
   --    Space complexity : O(1)                                             --
   ----------------------------------------------------------------------------

   function String_Rotation (S1, S2 : String) return Boolean is

      C2 : Positive range S2'Range := S2'First;

   begin

      -- Input checking
      if S1'Length = 0 or S2'Length = 0 then
         return False;
      elsif S1'Length /= S2'Length then
         return False;
      end if;

      -- Algorithm
      for R in 1 .. 2 loop
         for C1 in S1'Range loop

            if S1 (C1) = S2 (C2) then
               if C2 = S2'Last then
                  return True;
               else
                  C2 := C2 + 1;
               end if;
            else
               C2 := S2'First;
            end if;

         end loop;
      end loop;

      return False;

   end String_Rotation;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_String_Rotation (T : in out Test_Cases.Test_Case'Class) is
   begin

      Assert
        (Condition => String_Rotation ("a", "a") = True,
         Message   => "Test 1 failed");
      Assert
        (Condition => String_Rotation ("a", "b") = False,
         Message   => "Test 2 failed");
      Assert
        (Condition => String_Rotation ("waterbottle", "erbottlewat") = True,
         Message   => "Test 3 failed");
      Assert
        (Condition => String_Rotation ("waterbottle", "waterbottle") = True,
         Message   => "Test 4 failed");
     Assert
        (Condition => String_Rotation ("waterbottle", "waterbottl_") = False,
         Message   => "Test 5 failed");

   end Test_String_Rotation;

end Ex_01_09_String_Rotation;
