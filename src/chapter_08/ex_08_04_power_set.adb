with AUnit.Assertions;      use AUnit.Assertions;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Containers;        use Ada.Containers;

with System.Unsigned_Types; use System.Unsigned_Types;

package body Ex_08_04_Power_Set is

   ----------------------------------------------------------------------------
   function Power_Set (S : Set.Vector) return Set_Of_Sets.Vector is

      -------------------------------------------------------------------------
      function Recurse (N : Unsigned) return Set_Of_Sets.Vector is

         T : Set.Vector := Set.Empty_Vector;

      begin

         -- Return the empty set if zero
         if N = 0 then
            return Set_Of_Sets.Empty_Vector;
         end if;

         -- Select elements from the original set
         for K in 0 .. Natural (Set.Length (S)) - 1 loop

            if (Shift_Right (N, K) and 2#1#) = 2#1# then
               Set.Append (T, Set.Element (S, K));
            end if;

         end loop;

         -- Return the union of the computed set and the next
         return Set_Of_Sets."&" (T, Recurse (N-1));

      end Recurse;

      -- Result
      Result : Set_Of_Sets.Vector := Set_Of_Sets.Empty_Vector;

   begin

      -- Input checking
      if Natural (Set.Length (S)) > Unsigned'Size then
         raise Constraint_Error with "Too many elements.";
      end if;

      -- Compute the power set
      Result := Recurse (Unsigned (2 ** Natural (Set.Length(S))));

      -- Return the sorted result (sorting here is optional)
      declare

         function Is_Shorter (X, Y : Set.Vector) return Boolean is
           ( Set.Length (X) <  Set.Length (Y));

         package Sort_On_Length is
           new Set_Of_Sets.Generic_Sorting (Is_Shorter);

      begin
         Sort_On_Length.Sort (Result);
         return Result;
      end;

   end Power_Set;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Power_Set (T : in out Test_Cases.Test_Case'Class) is

      Input  : Set.Vector;
      Result : Set_Of_Sets.Vector;

   begin

      -- Initialize input vector
      declare
         use Set;
      begin
         Input := 1 & 2 & 3 & 4;
      end;

      -- Compute the power set
      Result := Power_Set (Input);

      -- Show the power set
      for S of Result loop

         if Set.Is_Empty (S) then
            Put_Line ("Empty Set");
         else
            for E of S loop
               Put (E, 2);
            end loop;
            New_Line;
         end if;

      end loop;

   end Test_Power_Set;

end Ex_08_04_Power_Set;
