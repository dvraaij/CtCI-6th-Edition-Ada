with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Ex_15_05_Call_In_Order is

   procedure Call_In_Order_1;
   procedure Call_In_Order_2;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Call_In_Order_1
     (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Call_In_Order_2
     (T : in out Test_Cases.Test_Case'Class);

end Ex_15_05_Call_In_Order;
