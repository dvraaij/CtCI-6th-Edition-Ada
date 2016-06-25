with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Containers;   use Ada.Containers;

package body Ex_02_05_Sum_Lists is


   --  +------------------+--------------+-----------+
   --  | Function         | Order digits | Method    |
   --  +------------------+--------------+-----------+
   --  | List_To_Value_B1 | Backward     | Iteration |
   --  | List_To_Value_B2 | Backward     | Recursion |
   --  | List_To_Value_F1 | Forward      | Iteration |
   --  | List_To_Value_F2 | Forward      | Recursion |
   --  +------------------+--------------+-----------+

   -- Backward iteration
   function List_To_Value_B1 (LL : List) return Natural is
   begin
      return Result : Natural := 0 do
         for Digit : Natural of reverse LL loop
            Result := 10 * Result + Digit;
         end loop;
      end return;
   end List_To_Value_B1;

   -- Forward recursion
   function List_To_Value_B2 (LL : List) return Natural is

      -- Recursion function
      function Recurse (C : Cursor) return Natural is
      begin
         if C = No_Element then
            return 0;
         else
            return LL (C) + 10 * Recurse (Next(C));
         end if;
      end Recurse;

   begin
      return Recurse (LL.First);
   end List_To_Value_B2;

   -- Forward iteration
   function List_To_Value_F1 (LL : List) return Natural is
   begin
      return Result : Natural := 0 do
         for Digit : Natural of LL loop
            Result := 10 * Result + Digit;
         end loop;
      end return;
   end List_To_Value_F1;

   -- Backward recursion
   function List_To_Value_F2 (LL : List) return Natural is

      -- Recursion function
      function Recurse (C : Cursor) return Natural is
      begin
         if C = No_Element then
            return 0;
         else
            return LL (C) + 10 * Recurse (Previous(C));
         end if;
      end Recurse;

   begin
      return Recurse (LL.Last);
   end List_To_Value_F2;

   --  +-----------------+--------------+-----------+
   --  | Function        | Order digits | Method    |
   --  +-----------------+--------------+-----------+
   --  | Value_To_List_B | Backward     | Iteration |
   --  | Value_To_List_F | Forward      | Iteration |
   --  +-----------------+--------------+-----------+

   function Value_To_List_B (V : Natural) return List is

      Result : List := Empty_List;
      Temp   : Natural := V;

   begin

      while Temp /= 0 loop
         Result.Append (Temp rem 10);
         Temp := Temp / 10;
      end loop;
      return Result;

   end Value_To_List_B;

   function Value_To_List_F (V : Natural) return List is

      Result : List := Empty_List;
      Temp   : Natural := V;

   begin

      while Temp /= 0 loop
         Result.Prepend (Temp rem 10);
         Temp := Temp / 10;
      end loop;
      return Result;

   end Value_To_List_F;

   ----------------------------------------------------------------------------
   -- Algorithm                                                              --
   --                                                                        --
   --    Comp. complexity : ?                                                --
   --    Space complexity : ?                                                --
   ----------------------------------------------------------------------------

   function Sum_Lists (LL1, LL2 : List; Fcn : Config) return List is

      Value1 : Natural;
      Value2 : Natural;

   begin

      -- Input checking
      if LL1 = Empty_List or LL2 = Empty_List then
         return Empty_List;
      end if;

      -- Extract values
      Value1 := Fcn.List_To_Value (LL1);
      Value2 := Fcn.List_To_Value (LL2);

      -- Compute sum and transform to list
      return Fcn.Value_To_List (Value1 + Value2);

   end Sum_Lists;

   ----------------
   -- Test cases --
   ----------------

   type Elements_Array is array (Positive range <>) of Natural;

   function Create_List (Elements : Elements_Array) return List is
   begin

      return Result : List := Empty_List do
         for E : Natural of Elements loop
            Result.Append (E);
         end loop;
      end return;

   end Create_List;

   -- Backward ordering
   procedure Test_Sum_Lists1 (T : in out Test_Cases.Test_Case'Class) is

      List1  : List := Create_List ((7, 1, 6));
      List2  : List := Create_List ((5, 9, 2));
      Result : List := Create_List ((2, 1, 9));

      Config1 : Config := (List_To_Value_B1'Access, Value_To_List_B'Access);
      Config2 : Config := (List_To_Value_B2'Access, Value_To_List_B'Access);

   begin

      Assert ( Sum_Lists (List1, List2, Config1) = Result, "Test 1 failed");
      Assert ( Sum_Lists (List1, List2, Config2) = Result, "Test 2 failed");

   end Test_Sum_Lists1;

   -- Forward ordering
   procedure Test_Sum_Lists2 (T : in out Test_Cases.Test_Case'Class) is

      List1  : List := Create_List ((6, 1, 7));
      List2  : List := Create_List ((2, 9, 5));
      Result : List := Create_List ((9, 1, 2));

      Config1 : Config := (List_To_Value_F1'Access, Value_To_List_F'Access);
      Config2 : Config := (List_To_Value_F2'Access, Value_To_List_F'Access);

   begin

      Assert ( Sum_Lists (List1, List2, Config1) = Result, "Test 1 failed");
      Assert ( Sum_Lists (List1, List2, Config2) = Result, "Test 2 failed");

   end Test_Sum_Lists2;

end Ex_02_05_Sum_Lists;
