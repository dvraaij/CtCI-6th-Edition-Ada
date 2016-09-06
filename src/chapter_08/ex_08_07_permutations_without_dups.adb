with AUnit.Assertions;      use AUnit.Assertions;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;

package body Ex_08_07_Permutations_Without_Dups is

   -- NOTE: Generate permutations using Heap's algorithm.

   ----------------------------------------------------------------------------
   procedure Permutations_Without_Dups (S : String) is

      subtype String_Index is Natural range S'Range;

      -- Permutation counter
      Count : Positive := 1;

      -------------------------------------------------------------------------
      procedure Generate (N : String_Index; P : aliased in out String) is

         ----------------------------------------------------------------------
         procedure Put_Perm (S : String) is
         begin
            Put (Count, 3);
            Put_Line (" : " & P);
            Count := Count + 1;
         end Put_Perm;

         ----------------------------------------------------------------------
         procedure Swap (SA : access String; Idx_1, Idx_2 : String_Index) is
            C : Character;
         begin
            C          := SA (Idx_1);
            SA (Idx_1) := SA (Idx_2);
            SA (Idx_2) := C;
         end Swap;

         ---------------------------------------------------------------------
         function Is_Even (X : String_Index) return Boolean is
            (X mod 2 = 0);

      begin

         -- Heap's Algorithm
         if N = 1 then
            Put_Perm (P);
         else
            for K in 1 .. N loop
               Generate (N-1, P);
               if Is_Even (N) then
                  Swap (P'Access, K      , N);
               else
                  Swap (P'Access, P'First, N);
               end if;
            end loop;

         end if;

      end Generate;

      -- Copy string
      P : aliased String := S;

   begin

      -- Genrate the permutations
      Generate (P'Last, P);

   end Permutations_Without_Dups;


   ----------------
   -- Test cases --
   ----------------

   procedure Test_Permutations_Without_Dups
     (T : in out Test_Cases.Test_Case'Class)
   is
   begin

      New_Line; Permutations_Without_Dups ("a");
      New_Line; Permutations_Without_Dups ("ab");
      New_Line; Permutations_Without_Dups ("abc");
      New_Line; Permutations_Without_Dups ("abcd");
      New_Line;

   end Test_Permutations_Without_Dups;

end Ex_08_07_Permutations_Without_Dups;
