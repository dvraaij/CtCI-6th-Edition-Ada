with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Containers;   use Ada.Containers;

package body Ex_2_6_Palindrome is


   ---------------
   -- Algorithm --
   ---------------

   function Palindrome (LL : List) return Boolean is

      C1 : Cursor := LL.First;
      C2 : Cursor := LL.Last;

   begin

      -- Input checking
      if LL = Empty_List then
         return False;
      end if;

      loop

         -- Compare elements
         if Element (C1) /= Element (C2) then
            return False;
         end if;

         -- Exit the loop if all elements have been compared
         exit when C1 = C2 or Next(C1) = C2;

         -- Next nodes
         C1 := Next (C1);
         C2 := Previous (C2);

      end loop;

      return True;

   end Palindrome;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Palindrome (T : in out Test_Cases.Test_Case'Class) is

      type Elements_Array is array (Positive range <>) of Natural;

      function To_List (Elements : Elements_Array) return List is
      begin

         return Result : List := Empty_List do
            for E : Natural of Elements loop
               Result.Append (E);
            end loop;
         end return;

      end To_List;

      List1 : List := To_List ((1 => 1));
      List2 : List := To_List ((1, 2));
      List3 : List := To_List ((1, 1));
      List4 : List := To_List ((1, 2, 3));
      List5 : List := To_List ((1, 2, 1));
      List6 : List := To_List ((1, 2, 3, 1));
      List7 : List := To_List ((1, 2, 2, 1));

   begin

      Assert (Palindrome (List1) = True , "Test 1 failed");
      Assert (Palindrome (List2) = False, "Test 2 failed");
      Assert (Palindrome (List3) = True , "Test 3 failed");
      Assert (Palindrome (List4) = False, "Test 4 failed");
      Assert (Palindrome (List5) = True , "Test 5 failed");
      Assert (Palindrome (List6) = False, "Test 6 failed");
      Assert (Palindrome (List7) = True , "Test 7 failed");

   end Test_Palindrome;

end Ex_2_6_Palindrome;
