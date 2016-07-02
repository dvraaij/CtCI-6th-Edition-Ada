with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Text_IO;      use Ada.Text_IO;

package body Ex_15_05_Call_In_Order is

   ----------------------------------------------------------------------------
   procedure Call_In_Order_1 is

      -- Solution using a passive protected type (which acts like a monitor)

      protected Foo is
         entry First;
         entry Second;
         entry Third;
      private
         First_Done  : Boolean := False;
         Second_Done : Boolean := False;
         Third_Done  : Boolean := False;
      end Foo;

      protected body Foo is

         ----------------------------------------------------------------------
         entry First
           when True is
         begin
            Put_Line ("First");
            First_Done := True;
         end First;

         ----------------------------------------------------------------------
         entry Second
           when First_Done is
         begin
            Put_Line ("Second");
            Second_Done := True;
         end Second;

         ----------------------------------------------------------------------
         entry Third
           when Second_Done is
         begin
            Put_Line ("Third");
            Third_Done := True;
         end Third;

      end Foo;

      -------------------
      -- Calling tasks --
      -------------------

      task Task_A;
      task Task_B;
      task Task_C;

      -------------------------------------------------------------------------
      task body Task_A is
      begin
         delay (0.50);
         Foo.First;
      end Task_A;

      -------------------------------------------------------------------------
      task body Task_B is
      begin
         delay (0.25);
         Foo.Second;
      end Task_B;

      -------------------------------------------------------------------------
      task body Task_C is
      begin
         Foo.Third;
      end Task_C;

   begin
      null;
   end Call_In_Order_1;

   ----------------------------------------------------------------------------
   procedure Call_In_Order_2 is

      -- Solution using an active task type

      task Foo is
         entry First;
         entry Second;
         entry Third;
      end Foo;

      -------------------------------------------------------------------------
      task body Foo is
      begin

         select
            accept First do
               Put_Line ("First");
            end First;
         or
            terminate;
         end select;

         select
            accept Second do
               Put_Line ("Second");
            end Second;
         or
            terminate;
         end select;

         select
            accept Third do
               Put_Line ("Third");
            end Third;
         or
            terminate;
         end select;

      end Foo;

      -------------------
      -- Calling tasks --
      -------------------

      task Task_A;
      task Task_B;
      task Task_C;

      -------------------------------------------------------------------------
      task body Task_A is
      begin
         delay (0.50);
         Foo.First;
      end Task_A;

      -------------------------------------------------------------------------
      task body Task_B is
      begin
         delay (0.25);
         Foo.Second;
      end Task_B;

      -------------------------------------------------------------------------
      task body Task_C is
      begin
         Foo.Third;
      end Task_C;

   begin
      null;
   end Call_In_Order_2;

   ----------------
   -- Test cases --
   ----------------

   ----------------------------------------------------------------------------
   procedure Test_Call_In_Order_1 (T : in out Test_Cases.Test_Case'Class) is
   begin
      Call_In_Order_1;
      Assert (True, "Test 1 failed");
   end Test_Call_In_Order_1;

   ----------------------------------------------------------------------------
   procedure Test_Call_In_Order_2 (T : in out Test_Cases.Test_Case'Class) is
   begin
      Call_In_Order_2;
      Assert (True, "Test 1 failed");
   end Test_Call_In_Order_2;

end Ex_15_05_Call_In_Order;
