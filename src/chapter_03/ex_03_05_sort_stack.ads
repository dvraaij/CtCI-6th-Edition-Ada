with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with CtCI.Stack;

package Ex_03_05_Sort_Stack is

   -- NOTE: The sort function is implemented in the CtCI.Stack package as
   --       a generic child package. Please have a look at
   --
   --          src/Code_Library/ctci-stack.ads
   --          src/Code_Library/ctci-stack.adb

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Sort_Stack
     (T : in out Test_Cases.Test_Case'Class);

end Ex_03_05_Sort_Stack;
