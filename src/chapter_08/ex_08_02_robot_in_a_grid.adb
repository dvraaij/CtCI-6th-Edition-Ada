with AUnit.Assertions;      use AUnit.Assertions;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with Ada.Text_IO;           use Ada.Text_IO;

with Ada.Numerics.Discrete_Random;

package body Ex_08_02_Robot_In_A_Grid is

   subtype Coord_X is Integer range 0 .. 10;
   subtype Coord_Y is Integer range 0 .. 10;

   type Grid_Node is (Empty, Block, Path);
   type Grid      is array (Coord_X, Coord_Y) of Grid_Node;

   G : Grid := (others => (others => Empty));

   ----------------------------------------------------------------------------
   function Robot_In_A_Grid return Boolean is

      function Try_Move (X : Coord_X; Y : Coord_Y) return Boolean is

         In_Last_Column : Boolean := X = Coord_X'Last;
         In_Last_Row    : Boolean := Y = Coord_Y'Last;
         Success        : Boolean := False;

      begin

         if G (X,Y) = Block then

            -- If no-go position
            Success := False;

         elsif In_Last_Column and In_Last_Row then

            -- If at destination
            Success := True;

         elsif In_Last_Column then

            -- If in last colomn, then right movement not possible
            Success := Try_Move (X, Y + 1);

         elsif In_Last_Row then

            -- If in last row, downward movement not possible
            Success := Try_Move (X + 1, Y);

         else

            -- Try right movement first or else downward movement (if right
            -- movement failed).
            Success := Try_Move (X + 1, Y) or else Try_Move (X, Y + 1);

         end if;

         -- Mark path on grid
         if Success then
            G (X,Y) := Path;
         end if;

         return Success;

      end Try_Move;

      -- Initial move. Note that the order (right movement first, then
      -- downward movement) implies a preference for moving in the right
      -- direction.
      Success : Boolean := Try_Move (1, 0) or else Try_Move (0, 1);

   begin
      return Success;
   end Robot_In_A_Grid;

   ----------------------------------------------------------------------------
   procedure Print_Grid is
   begin

      for Y in Coord_Y'Range loop
         for X in Coord_X'Range loop
            case G (X,Y) is
            when Empty => Put (". ");
            when Block => Put ("X ");
            when Path  => Put ("+ ");
            end case;
         end loop;
         New_Line;
      end loop;
      New_Line;

   end Print_Grid;

   ----------------
   -- Test cases --
   ----------------

   ----------------------------------------------------------------------------
   procedure Test_Robot_In_A_Grid (T : in out Test_Cases.Test_Case'Class) is

      package Random_X is
        new Ada.Numerics.Discrete_Random (Coord_X);
      use Random_X;

      package Random_Y is
        new Ada.Numerics.Discrete_Random (Coord_Y);
      use Random_Y;

   begin

      -- Insert some no-go nodes
      declare
         Gen_X : Random_X.Generator;
         Gen_Y : Random_Y.Generator;
      begin
         Random_X.Reset (Gen_X, 123456);
         Random_Y.Reset (Gen_Y, 654321);
         for K in 0 .. 20 loop
            G (Random_X.Random (Gen_X), Random_Y.Random (Gen_Y)) := Block;
         end loop;
      end;

      -- Try to find a path
      if Robot_In_A_Grid then
         Print_Grid;
      else
         Put_Line ("No feasible path found");
      end if;

   end Test_Robot_In_A_Grid;

end Ex_08_02_Robot_In_A_Grid;
