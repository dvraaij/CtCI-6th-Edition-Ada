with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Ex_05_08_Draw_Line is

   -- Screen dimensions
   Screen_Width  : constant := 80;
   Screen_Height : constant := 25;

   -- Coordinate subtypes
   subtype Coord_X is Integer range 1 .. Screen_Width;
   subtype Coord_Y is Integer range 1 .. Screen_Height;

   -- Procedure to draw a line on the screenbuffer
   procedure Draw_Line
     (X0 : Coord_X; Y0 : Coord_Y;
      X1 : Coord_X; Y1 : Coord_Y);

   -- Procedure to draw the screenbuffer
   procedure Draw_Screen;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Draw_Line
     (T : in out Test_Cases.Test_Case'Class);

end Ex_05_08_Draw_Line;
