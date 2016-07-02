with AUnit.Assertions;      use AUnit.Assertions;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with Ada.Text_IO;           use Ada.Text_IO;

with Interfaces;    use Interfaces;
with Ada.Numerics;  use Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

package body Ex_05_08_Draw_Line is

   Buffer_Size : constant := (Screen_Width / 8 * Screen_Height);

   type Screen is
     array (Integer range 0 .. (Buffer_Size - 1)) of Unsigned_8;

   S : Screen;

   ----------------------------------------------------------------------------
   function Index_Major (X : Coord_X; Y : Coord_Y) return Integer is
     (((Y-1) * Screen_Width + (X-1)) / 8)
   with Inline;

   ----------------------------------------------------------------------------
   function Index_Minor (X : Coord_X; Y : Coord_Y) return Integer is
     ((X-1) mod 8)
   with Inline;

   ----------------------------------------------------------------------------
   procedure Set_Pixel (X : Coord_X; Y : Coord_Y) is
   begin
      S (Index_Major (X, Y)) :=
           S (Index_Major (X, Y)) or Shift_Left (1, Index_Minor (X, Y));
   end Set_Pixel;

   ----------------------------------------------------------------------------
   procedure Reset_Pixel (X : Coord_X; Y : Coord_Y) is
   begin
      S (Index_Major (X, Y)) :=
        S (Index_Major (X, Y)) and not Shift_Left (1, Index_Minor (X, Y));
   end Reset_Pixel;

   ----------------------------------------------------------------------------
   function Get_Pixel (X : Coord_X; Y : Coord_Y) return Boolean is
   begin
      return
        (S (Index_Major (X, Y)) and Shift_Left (1, Index_Minor (X, Y))) /= 0;
   end Get_Pixel;

   ----------------------------------------------------------------------------
   procedure Draw_Line
     (X0 : Coord_X; Y0 : Coord_Y;
      X1 : Coord_X; Y1 : Coord_Y)
   is

      -- NOTE: This line drawing procedure is based on Bessenham's line
      --       algorithm. For more information see
      --
      --          https://en.wikipedia.org/wiki/Bresenham's_line_algorithm

      ------------
      -- Octant --
      ------------

      -- Determine to which octant the requested line belongs
      --
      --    \2|1/
      --    3\|/0
      --   ---+---
      --    4/|\7
      --    /5|6\

      DY : Integer := Y1 - Y0;
      DX : Integer := X1 - X0;

      Octant : constant Integer range 0 .. 7 :=
                 (if DY >= 0 then
                    (if DX >= 0 then
                       (if abs (DX) >= abs (DY) then 0 else 1)
                     else
                       (if abs (DX) >= abs (DY) then 3 else 2))
                  else
                    (if DX >= 0 then
                       (if abs (DX) >= abs (DY) then 7 else 6)
                     else
                       (if abs (DX) >= abs (DY) then 4 else 5)));

      --------------------------------
      -- Coordinate transformations --
      --------------------------------

      -- Forward and inverse transformations to map the original coordinate
      -- to and from octant 0.

      subtype Coord_U is Integer;
      subtype Coord_V is Integer;

      -------------------------------------------------------------------------
      procedure Transform_To_Octant0
        (X : in  Coord_X; Y : in  Coord_Y;
         U : out Coord_U; V : out Coord_V)
      is
      begin

         case Octant is
         when 0 => U :=  X; V :=  Y;
         when 1 => U :=  Y; V :=  X;
         when 2 => U :=  Y; V := -X;
         when 3 => U := -X; V :=  Y;
         when 4 => U := -X; V := -Y;
         when 5 => U := -Y; V := -X;
         when 6 => U := -Y; V :=  X;
         when 7 => U :=  X; V := -Y;
         end case;

         return;

      end Transform_To_Octant0;

      -------------------------------------------------------------------------
      procedure Transform_From_Octant0
        (U : in  Coord_U; V : in  Coord_V;
         X : out Coord_X; Y : out Coord_Y)
      is
      begin

         case Octant is
         when 0 => X :=  U; Y :=  V;
         when 1 => X :=  V; Y :=  U;
         when 2 => X := -V; Y :=  U;
         when 3 => X := -U; Y :=  V;
         when 4 => X := -U; Y := -V;
         when 5 => X := -V; Y := -U;
         when 6 => X :=  V; Y := -U;
         when 7 => X :=  U; Y := -V;
         end case;

         return;

      end Transform_From_Octant0;

      -- Transformed coordinates
      U0 : Coord_U; V0 : Coord_V;
      U1 : Coord_U; V1 : Coord_U;

   begin

      -- Transform to octant 0
      Transform_To_Octant0 (X0, Y0, U0, V0);
      Transform_To_Octant0 (X1, Y1, U1, V1);

      -- Draw line using Bresenham's line algorithm
      declare
         DV : Integer := V1 - V0;
         DU : Integer := U1 - U0;
         D  : Integer := DV - DU;
         V  : Coord_V := V0;
      begin
         for U in U0 .. (U1-1) loop

            -- Transform from octant 0 to original octant and draw pixel
            declare
               X : Coord_X;
               Y : Coord_Y;
            begin
               Transform_From_Octant0 (U, V, X, Y);
               Set_Pixel (X, Y);
            end;

            if (D >= 0) then
               V := V + 1;
               D := D - DU;
            end if;
            D := D + DV;

         end loop;
      end;

   end Draw_Line;

   ----------------------------------------------------------------------------
   procedure Draw_Screen is
   begin

      for Y in Coord_Y loop
         for X in Coord_X loop
            Put (if Get_Pixel (X, Y) = True then 'X' else '.');
         end loop;
         New_Line;
      end loop;

   end Draw_Screen;

   ----------------
   -- Test cases --
   ----------------

   ----------------------------------------------------------------------------
   procedure Test_Draw_Line
     (T : in out Test_Cases.Test_Case'Class) is

      package Math is
        new Ada.Numerics.Generic_Elementary_Functions (Float);
      use Math;

   begin

      -- Draw a frame
      Draw_Line ( Coord_X'First, Coord_Y'First, Coord_X'Last , Coord_Y'First);
      Draw_Line ( Coord_X'First, Coord_Y'Last , Coord_X'Last , Coord_Y'Last );
      Draw_Line ( Coord_X'First, Coord_Y'First, Coord_X'First, Coord_Y'Last );
      Draw_Line ( Coord_X'Last , Coord_Y'First, Coord_X'Last , Coord_Y'Last );

      -- Some lines
      for K in 0 .. 11 loop
         Draw_Line
           ( 40, 12,
             40 + Integer ( 30.0 * Cos ( 2.0 * Pi * Float (K) / 12.0 )),
             12 + Integer ( 10.0 * Sin ( 2.0 * Pi * Float (K) / 12.0 )));
      end loop;

      -- Show the resulting screen
      Draw_Screen;

   end Test_Draw_Line;

end Ex_05_08_Draw_Line;
