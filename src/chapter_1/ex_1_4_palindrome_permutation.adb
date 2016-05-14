with AUnit.Assertions;        use AUnit.Assertions;
with AUnit.Test_Cases;        use AUnit.Test_Cases;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Ex_1_4_Palindrome_Permutation is

   ----------------------------------------------------------------------------
   -- Algorithm 1                                                            --
   --                                                                        --
   --    Comp. complexity : O(N)                                             --
   --    Space complexity : O(1)                                             --
   ----------------------------------------------------------------------------

   function Palindrome_Permutation (S : String) return Boolean is

      -- Subtype representing lower-case range only
      subtype Lower_Case is Character range 'a' .. 'z';

      -- Using explicit type definition to request compiler for array packing
      type Packed_Array is array (Lower_Case) of Boolean with
        Pack;

      -- The function states
      Count_Odd : Boolean      := False;
      Freq_Odd  : Packed_Array := (others => False);

      -- Convert to lower case
      S_Lower_Case : String := To_Lower (S);

   begin

      -- Input checking
      if S'Length = 0 then   -- Not specified in exercise
         return False;
      elsif S'Length = 1 then
         return True;
      end if;

      -- Algorithm
      for k in S_Lower_Case'Range loop
         if S_Lower_Case (k) in Lower_Case'Range then
            Freq_Odd (S_Lower_Case (k)) :=
              not Freq_Odd (S_Lower_Case (k));
            Count_Odd := not Count_Odd;
         end if;
      end loop;

      -- NOTE: Verification of odd/even frequency of characters implemented
      --       using Ada 2012 qualified expressions (just for fun). Are normally
      --       used for proving code semantics. Could also have been implemented
      --       using a for-loop and simple counter, which may be more efficient
      --       depending on how the compiler simplifies/optimizes the predicate.

      if Count_Odd = True then

         --   The following predicate returns true if and only if
         --     (1) There exists an array element k which is true AND
         --     (2) All elements before k are false AND
         --     (3) All elements after  k are false

         return
           (for some k in Freq_Odd'Range =>
              Freq_Odd (k) = True and
              (for all p in Freq_Odd'First .. Lower_Case'Pred (k) =>
                 Freq_Odd (p) = False) and
              (for all q in Lower_Case'Succ (k) .. Freq_Odd'Last =>
                 Freq_Odd (q) = False));
      else

         --   The following predicate returns true if and only if
         --     (1) All array elements are false

         return
           (for all k in Freq_Odd'Range => Freq_Odd (k) = False);

      end if;

   end Palindrome_Permutation;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Palindrome_Permutation
     (T : in out Test_Cases.Test_Case'Class) is
   begin

      Assert (Palindrome_Permutation ("Tact Coa") = True , "Test 1 Failed");
      Assert (Palindrome_Permutation ("Aa a")     = True , "Test 2 Failed");
      Assert (Palindrome_Permutation ("aaA A")    = True , "Test 3 Failed");
      Assert (Palindrome_Permutation ("a Aab")    = False, "Test 4 Failed");

   end Test_Palindrome_Permutation;

end Ex_1_4_Palindrome_Permutation;
