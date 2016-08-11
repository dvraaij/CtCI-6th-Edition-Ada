with AUnit.Assertions;      use AUnit.Assertions;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with Ada.Text_IO;           use Ada.Text_IO;

package body Ex_08_06_Towers_Of_Hanoi is

   -- NOTE: The algorithm can be found be using the base case and build method
   --       (as explained in the book). The key is to observe that the for the
   --       case of N disks, one should recusively use the _transferpattern_
   --       of the case N-1 (highlighted below using boxes).
   --
   --          N = 1     1 --> 3          src --> dst
   --
   --
   --                  +---------+      +-------------+
   --                  | 1 --> 2 |      | src --> buf |
   --                  +---------+      +-------------+
   --          N = 2     1 --> 3          src --> dst
   --                  +---------+      +-------------+
   --                  | 2 --> 3 |      | buf --> dst |
   --                  +---------+      +-------------+
   --
   --
   --                  +---------+      +-------------+
   --                  | 1 --> 3 |      | src --> dst |
   --                  | 1 --> 2 |      | src --> buf |
   --                  | 3 --> 2 |      | dst --> buf |
   --                  +---------+      +-------------+
   --          N = 3     1 --> 3          src --> dst
   --                  +---------+      +-------------+
   --                  | 2 --> 1 |      | buf --> src |
   --                  | 2 --> 3 |      | buf --> dst |
   --                  | 1 --> 3 |      | src --> dst |
   --                  +---------+      +-------------+

   use Stack;

   ---------------------
   -- Stack functions --
   ---------------------

   procedure Push (T : in out Vector; D : Disk) is
   begin
      Append (T, D);
   end Push;

   function Peek (T : Vector) return Disk renames Last_Element;

   function Pop (T : in out Vector) return Disk is
      D : Disk :=  Peek (T);
   begin
      Delete_Last (T);
      return D;
   end;

   ---------------
   -- Algorithm --
   ---------------

   ----------------------------------------------------------------------------
   procedure Towers_Of_Hanoi (T1, T2, T3 : in out Vector) is

      -------------------------------------------------------------------------
      procedure Move_Disk (Src, Dst : in out Vector) is
      begin

         -- Assert feasibility of tower-to-tower disk transfer
         Assert (not Is_Empty (Src) and
                   (Is_Empty (Dst) or else Peek (Dst) > Peek (Src)),
                 "Test 2 failed");

         -- Transfer the disk
         Push (Dst, Pop (Src));

      end Move_Disk;

      -------------------------------------------------------------------------
      procedure Move_Top_Disks (N : Natural; Src, Dst, Buf : in out Vector) is
      begin

         if N = 0 then
            return;
         end if;

         Move_Top_Disks (N-1, Src, Buf, Dst);
         Move_Disk (          Src, Dst);
         Move_Top_Disks (N-1, Buf, Dst, Src);

      end Move_Top_Disks;

   begin

      -- Transfer disks
      Move_Top_Disks (Disk'Last, T1, T3, T2);

   end Towers_Of_Hanoi;

   ----------------------------------------------------------------------------
   procedure Test_Towers_Of_Hanoi (T : in out Test_Cases.Test_Case'Class) is

      T1 : Vector (MAX_DISKS);
      T2 : Vector (MAX_DISKS);
      T3 : Vector (MAX_DISKS);

   begin
      -- Initialize tower 1
      for K in reverse 1 .. Disk'Last loop
         Push (T1, K);
      end loop;

      -- Assert initial state
      Assert ((for all K in First_Index (T1) .. Last_Index (T1) - 1 =>
                Element (T1, K) > Element (T1, K+1)),
              "Test 1a failed");
      Assert (Is_Empty (T2), "Test 1b failed");
      Assert (Is_Empty (T3), "Test 1c failed");

      -- Simulate the game
      Towers_Of_Hanoi (T1, T2, T3);

      -- Assert final state
      Assert (Is_Empty (T1), "Test 3a failed");
      Assert (Is_Empty (T2), "Test 3b failed");
      Assert ((for all K in First_Index (T3) .. Last_Index (T3) - 1 =>
                Element (T3, K) > Element (T3, K+1)),
              "Test 3c failed");

   end Test_Towers_Of_Hanoi;

end Ex_08_06_Towers_Of_Hanoi;
