with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Text_IO;      use Ada.Text_IO;

package body Ex_15_07_Fizz_Buzz is

   ----------------------------------------------------------------------------
   procedure Fizz_Buzz is

      ------------
      -- Number --
      ------------

      subtype Number_Type is
        Integer range 0 .. 100;

      type Test_Access is
        access procedure (N : in out Number_Type);

      protected Number is
         entry Process (Proc : Test_Access; Done : out Boolean);
      private
         N : Number_Type := 0;
      end Number;

      -------------------------------------------------------------------------
      protected body Number is
         entry Process (Proc : Test_Access; Done : out Boolean)
           when True is
         begin
            if N < Number_Type'Last then
               Proc (N);
               Done := False;
            else
               Done := True;
            end if;
         end Process;
      end Number;

      -----------
      -- Tests --
      -----------

      task Fizz;
      task Buzz;
      task Fizz_Buzz;
      task Default;

      -------------------------------------------------------------------------
      procedure Test_Fizz (N : in out Number_Type) is
      begin
         if (N mod 3 = 0) and (N mod 5 /= 0) then
            Put_Line ("Fizz");
            N := N + 1;
         end if;
      end Test_Fizz;

      -------------------------------------------------------------------------
      procedure Test_Buzz (N : in out Number_Type) is
      begin
         if (N mod 3 /= 0) and (N mod 5 = 0) then
            Put_Line ("Buzz");
            N := N + 1;
         end if;
      end Test_Buzz;

      -------------------------------------------------------------------------
      procedure Test_Fizz_Buzz (N : in out Number_Type) is
      begin
         if (N mod 3 = 0) and (N mod 5 = 0) then
            Put_Line ("FizzBuzz");
            N := N + 1;
         end if;
      end Test_Fizz_Buzz;

      -------------------------------------------------------------------------
      procedure Test_Default (N : in out Number_Type) is
      begin
         if (N mod 3 /= 0) and (N mod 5 /= 0) then
            Put_Line (Number_Type'Image (N));
            N := N + 1;
         end if;
      end Test_Default;

      -------------------------------------------------------------------------
      task body Fizz is
         Done : Boolean := False;
      begin
         while not Done loop
            Number.Process (Test_Fizz'Access, Done);
         end loop;
      end Fizz;

      -------------------------------------------------------------------------
      task body Buzz is
         Done : Boolean := False;
      begin
         while not Done loop
            Number.Process (Test_Buzz'Access, Done);
         end loop;
      end Buzz;

      -------------------------------------------------------------------------
      task body Fizz_Buzz is
         Done : Boolean := False;
      begin
         while not Done loop
            Number.Process (Test_Fizz_Buzz'Access, Done);
         end loop;
      end Fizz_Buzz;

      -------------------------------------------------------------------------
      task body Default is
         Done : Boolean := False;
      begin
         while not Done loop
            Number.Process (Test_Default'Access, Done);
         end loop;
      end Default;

   begin
      null;
   end Fizz_Buzz;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Fizz_Buzz
     (T : in out Test_Cases.Test_Case'Class) is
   begin
      Fizz_Buzz;
      Assert (True, "Test 1 failed");
   end Test_Fizz_Buzz;

end Ex_15_07_Fizz_Buzz;
