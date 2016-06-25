with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Text_IO;      use Ada.Text_IO;

with Ada.Strings;

package body Ex_01_03_Urlify is

   ----------------------------------------------------------------------------
   -- Algorithm 1                                                            --
   --                                                                        --
   --    Comp. complexity : O(N)                                             --
   --    Space complexity : O(1)                                             --
   ----------------------------------------------------------------------------

   procedure URLify (S : in out String; Length : Positive) is

      C1 : Positive := Length;
      C2 : Positive := S'Last;

      Number_Of_Spaces : Natural := 0;

      Is_Space : Boolean;

   begin

      -- Input checking
      if S'Length = 0 then   -- Not specified in exercise
         return;
      end if;

      -- Count number of spaces
      for k in S'First .. (S'First + Length - 1) loop
         if S (k) = Ada.Strings.Space then
            Number_Of_Spaces := Number_Of_Spaces + 1;
         end if;
      end loop;

      -- Check if new string fits
      if (Length + 2 * Number_Of_Spaces) /= S'Length then
         raise Constraint_Error with "string length incorrect";
      end if;

      -- Looping basckwards over string
      loop

         -- Check for space
         Is_Space := (S (C1) = Ada.Strings.Space);

         -- If so insert "%20", else just copy
         if Is_Space then
            S (C2-2 .. C2) := "%20";
         else
            S (C2) := S (C1);
         end if;

         -- Exit loop if all characters have been processed
         exit when C1 = S'First;

         -- Update cursors
         if Is_Space then
            C1 := C1 - 1;
            C2 := C2 - 3;
         else
            C1 := C1 - 1;
            C2 := C2 - 1;
         end if;

      end loop;

   end URLify;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_URLify (T : in out Test_Cases.Test_Case'Class) is

      Input1 : String := "Mr John Smith    ";
      Input2 : String := "abcd   ";
      Input3 : String := " abcd  ";

      Length1 : Positive := 13;
      Length2 : Positive := 5;
      Length3 : Positive := 5;

      Result1 : constant String := "Mr%20John%20Smith";
      Result2 : constant String := "abcd%20";
      Result3 : constant String := "%20abcd";

   begin

      URLify (Input1, Length1);
      URLify (Input2, Length2);
      URLify (Input3, Length3);

      Assert (Input1 = Result1, "Test 1 failed");
      Assert (Input2 = Result2, "Test 2 failed");
      Assert (Input3 = Result3, "Test 3 failed");

   end Test_URLify;

end Ex_01_03_Urlify;
