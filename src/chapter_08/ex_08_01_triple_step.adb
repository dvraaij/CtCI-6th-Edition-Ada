with AUnit.Assertions;      use AUnit.Assertions;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;

package body Ex_08_01_Triple_Step is

   -- Example combinations
   --
   -- 1 ((1))
   -- 2 ((1 1), (2))
   -- 3 ((1 1 1), (2 1), (1 2), (3))
   -- 4 ((1 1 1 1), (1 1 2), (1 2 1), (2 1 1) (1 3), (3 1), (2 2))
   -- 5 ((1 1 1 1 1), (1 1 1 2), (1 1 2 1),(1 2 1 1), (2 1 1 1),
   --    (1 2 2), (2 1 2), (2 2 1), (1 1 3), (1 3 1), (3 1 1), (2 3), (3 2))

   ----------------------------------------------------------------------------
   function Triple_Step (N : Positive) return Natural is

      -- NOTE: The actual recursion function "Step_Up" is a seperate function
      --       to ensure that the main function "Tripe_Step") only accepts
      --       (physically feasible) positive values whereas the recursion
      --       function is allowed to accept negative values.

      -------------------------------------------------------------------------
      function Step_Up (Stairs_Left : Integer) return Natural is
      begin

         if Stairs_Left < 0 then

            -- If less than 0 stairs are left, then the solution is invalid
            return 0;

         elsif Stairs_Left = 0 then

            -- If exactly 0 stairs are left, then the solution is valid
            return 1;

         else

            -- Try to move up by 1, 2 or 3 steps
            return
              Step_Up (Stairs_Left-1) +
              Step_Up (Stairs_Left-2) +
              Step_Up (Stairs_Left-3);

         end if;

      end Step_Up;

   begin
      return
        Step_Up (N-1) +
        Step_Up (N-2) +
        Step_Up (N-3);
   end Triple_Step;

   ----------------
   -- Test cases --
   ----------------

   ----------------------------------------------------------------------------
   procedure Test_Triple_Step (T : in out Test_Cases.Test_Case'Class) is
   begin

      for K in 1 .. 10 loop
         Put (Triple_Step (K));
         New_Line;
      end loop;
      New_Line;

   end Test_Triple_Step;

end Ex_08_01_Triple_Step;
