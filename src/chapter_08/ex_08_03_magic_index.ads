with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Ex_08_03_Magic_Index is

   type Input_Array is
     array (Positive range <>) of Integer;

   function Magic_Index (A : Input_Array; MI : out Positive) return Boolean;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Magic_Index
     (T : in out Test_Cases.Test_Case'Class);

end Ex_08_03_Magic_Index;
