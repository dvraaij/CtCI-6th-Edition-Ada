with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Text_IO;      use Ada.Text_IO;

package body Ex_1_1_Is_Unique is

   ----------------------------------------------------------------------------
   -- Algorithm 1                                                            --
   --                                                                        --
   --    Comp. complexity : O(N)                                             --
   --    Space complexity : O(N)                                             --
   ----------------------------------------------------------------------------

   function Is_Unique1 (S : String) return Boolean is

      -- Using explicit type definition to request compiler for array packing
      type Packed_Array is array (Character) of Boolean with
        Pack;

      Used : Packed_Array := (others => False);

   begin

      -- Input checking
         if S'Length =   0 then return False;   -- Not specified in exercise
      elsif S'Length =   1 then return True;
      elsif S'Length > 256 then return False;
      end if;

      -- Algorithm
      for k in S'Range loop
         if Used (S(k)) = True then
            return False;
         end if;
         Used (S(k)) := True;
      end loop;

      return True;

   end Is_Unique1;

   ----------------------------------------------------------------------------
   -- Algorithm 2                                                            --
   --                                                                        --
   --    Comp. complexity : O(N^2)                                           --
   --    Space complexity : O(1)                                             --
   ----------------------------------------------------------------------------

   -- NOTE: one may obtain the computational complexity by analyzing the
   --       algorithm and obtain the sum using e.g. Maxima CAS:
   --
   --         bigO: sum(N-k, k, 1, N-1), simpsum=true$
   --         ratsimp(bigO);

   function Is_Unique2 (S : String) return Boolean is
   begin

      -- Input checking
         if S'Length =   0 then return False;   -- Not specified in exercise
      elsif S'Length =   1 then return True;
      elsif S'Length > 256 then return False;
      end if;

      -- Actual algorithm
      for k1 in S'Range loop
         for k2 in (k1+1) .. S'Last loop
            if S (k1) = S (k2) then
               return False;
            end if;
         end loop;
      end loop;

      return True;

   end Is_Unique2;

   ----------------
   -- Test cases --
   ----------------

   procedure Run_Test_Cases
     (Fcn_Ptr : access function (S : String) return Boolean)
   is
   begin
      Assert (Fcn_Ptr (""       ) = False, "Test 1 Failed");
      Assert (Fcn_Ptr ("a"      ) = True , "Test 2 Failed");
      Assert (Fcn_Ptr ("abcdefg") = True , "Test 3 Failed");
      Assert (Fcn_Ptr ("abcdffg") = False, "Test 4 Failed");
      Assert (Fcn_Ptr ("abcdebg") = False, "Test 5 Failed");
      Assert (Fcn_Ptr ("aacdefg") = False, "Test 6 Failed");
      Assert (Fcn_Ptr ("abcdegg") = False, "Test 7 Failed");
   end Run_Test_Cases;

   procedure Test_Is_Unique1 (T : in out Test_Cases.Test_Case'Class) is
   begin
      Run_Test_Cases (Is_Unique1'Access);
   end Test_Is_Unique1;

   procedure Test_Is_Unique2 (T : in out Test_Cases.Test_Case'Class) is
   begin
      Run_Test_Cases (Is_Unique2'Access);
   end Test_Is_Unique2;

end Ex_1_1_Is_Unique;
