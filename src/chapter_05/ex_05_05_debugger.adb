with AUnit.Assertions;      use AUnit.Assertions;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with Ada.Text_IO;           use Ada.Text_IO;

package body Ex_05_05_Debugger is

   function Debugger (Value : Unsigned_16) return Boolean is
     ((Value and (Value - 1)) = 0);

   ----------------
   -- Test cases --
   ----------------

   ----------------------------------------------------------------------------
   procedure Test_Debugger
     (T : in out Test_Cases.Test_Case'Class)
   is
      Result : Boolean;
   begin

      -- Some test values to see the pattern
      for K in 0 .. 127 loop

         -- Expected result
         case K is
            when 0      => Result := True;
            when 1      => Result := True;
            when 2      => Result := True;
            when 4      => Result := True;
            when 8      => Result := True;
            when 16     => Result := True;
            when 32     => Result := True;
            when 64     => Result := True;
            when others => Result := False;
         end case;

         -- Assert
         Assert (Debugger ( Unsigned_16 (K)) = Result,
                 "Test" & Integer'Image (K) & " failed");

      end loop;

   end Test_Debugger;

end Ex_05_05_Debugger;
