with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Containers;   use Ada.Containers;

package body Ex_2_2_Return_Kth_To_Last is

   ----------------------------------------------------------------------------
   -- Algorithm 1                                                            --
   --                                                                        --
   --    Comp. complexity : O(N)                                             --
   --    Space complexity : O(1)                                             --
   ----------------------------------------------------------------------------

   function Return_Kth_To_Last
     (LL             : List;
      Offset_To_Last : Count_Type) return String
   is

      C1  : Cursor;
      C2  : Cursor;
      Lag : Count_Type;

   begin

      -- Input checking
      if LL = Empty_List then
         raise Constraint_Error;
      elsif not (Offset_To_Last < LL.Length) then
         raise Constraint_Error;
      end if;

      -- Initialize cursor and lag counter
      C1  := LL.First;
      C2  := LL.First;
      Lag := 0;

      loop

         -- Exit when at end of list
         exit when C1 = LL.Last;

         -- If the lag is equal to the requested offset
         if Lag = Offset_to_Last then

            -- Increment both
            Next (C1);
            Next (C2);

         else

            -- Increment only C1 and count lag
            Next (C1);
            Lag := Lag + 1;

         end if;

      end loop;

      -- Return the element of the kth to last node
      return LL (C2);

   end Return_Kth_To_Last;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Return_Kth_To_Last (T : in out Test_Cases.Test_Case'Class) is

      List1 : List;

   begin

      -- Create a list
      for k in 1 .. 10 loop
         List1.Append ("Node" & Integer'Image (k));
      end loop;

      -- Verify some valid offsets
      Assert (Return_Kth_To_Last (List1, 9) = "Node 1" , "Test 1 failed");
      Assert (Return_Kth_To_Last (List1, 5) = "Node 5" , "Test 2 failed");
      Assert (Return_Kth_To_Last (List1, 0) = "Node 10", "Test 3 failed");

      -- Verify exception
      declare
         Result : access String;
      begin
         Result := new String'(Return_Kth_To_Last (List1, 10));
         Assert (False, "Test 4 failed");
      exception
         when Constraint_Error => null;
      end;

   end Test_Return_Kth_To_Last;

end Ex_2_2_Return_Kth_To_Last;
