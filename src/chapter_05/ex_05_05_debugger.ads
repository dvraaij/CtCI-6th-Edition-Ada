with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with Interfaces;       use Interfaces;

package Ex_05_05_Debugger is

   function Debugger (Value : Unsigned_16) return Boolean;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Debugger
     (T : in out Test_Cases.Test_Case'Class);

end Ex_05_05_Debugger;
