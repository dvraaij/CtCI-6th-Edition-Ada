with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Ex_03_01_Three_In_One is

   ----------------------
   -- Number of stacks --
   ----------------------

   Max_Stack : constant := 3;

   subtype Stack_Index is Natural range 0 .. (Max_Stack - 1);

   -----------------
   -- Subprograms --
   -----------------

   procedure Push (S : Stack_Index; Item : Integer);
   function  Pop  (S : Stack_Index) return Integer;
   function  Peek (S : Stack_Index) return Integer;

   function Is_Empty (S : Stack_Index) return Boolean;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Three_In_One (T : in out Test_Cases.Test_Case'Class);

end Ex_03_01_Three_In_One;
