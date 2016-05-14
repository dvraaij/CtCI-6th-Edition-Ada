with AUnit.Assertions;    use AUnit.Assertions;
with AUnit.Test_Cases;    use AUnit.Test_Cases;
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Strings.Bounded; use Ada.Strings.Bounded;

package body Ex_1_6_String_Compression is

   ----------------------------------------------------------------------------
   -- Algorithm 1                                                            --
   --                                                                        --
   --    Comp. complexity : O(N)                                             --
   --    Space complexity : O(1)                                             --
   ----------------------------------------------------------------------------

   function String_Compression (S : in String) return String is

      ----------------
      -- RLE packet --
      ----------------

      type Packet is record
         Char  : Character;
         Count : Positive;
      end record;

      -- NOTE: The image of Count is always positive, hence, according to
      --       Ada 2012 RM 3.5-24.7/2, the first character will always be a
      --       space. This space is replaced here with the character under
      --       consideration.

      function Image (P : Packet) return String is
         S : String := Positive'Image (P.Count);
      begin
         S (S'First) := P.Char;
         return S;
      end Image;

      ---------------------------------
      -- Determine compressed length --
      ---------------------------------

      function Compressed_Length return Natural is

         P : Packet;
         N : Natural := 0;

      begin

         -- Trivials
         if S'Length = 0 then
            return 0;
         elsif S'Length = 1 then
            return 2;
         end if;

         -- Initialization
         P := (S (S'First), 1);

         -- Count the occurances of a character. Add length of packet to total
         -- compressed length N if next character in string is different
         -- compared to current.
         for k in (S'First + 1) .. S'Last loop
            if S (k) = P.Char then
               P.Count := P.Count + 1;
            else
               N := N + Image (P)'Length;
               P := (S (k), 1);
            end if;
         end loop;

         -- Flush state
         N := N + Image (P)'Length;

         return N;

      end Compressed_Length;

      -------------------------
      -- Compress the string --
      -------------------------

      function Compress (N : Natural) return String is

         package SC_Bounded_Length is new Ada.Strings.Bounded
           .Generic_Bounded_Length
           (N);
         use SC_Bounded_Length;

         P  : Packet;
         SC : Bounded_String := Null_Bounded_String;

      begin

         -- Initialization
         P := (S (S'First), 1);

         -- Count the occurances of a character. Append packet to compressed
         -- string if next character in string is different compared to
         -- current.
         for k in (S'First + 1) .. S'Last loop
            if S (k) = P.Char then
               P.Count := P.Count + 1;
            else
               SC := Append (SC, Image (P));
               P  := (S (k), 1);
            end if;
         end loop;

         -- Flush state
         SC := Append (SC, Image (P));

         return To_String (SC);

      end Compress;

      -- Compressed length
      N : Natural := Compressed_Length;

   begin

      -- Return compressed string only if shorter then original
      if N < S'Length then
         return Compress (N);
      else
         return S;
      end if;

   end String_Compression;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_String_Compression
     (T : in out Test_Cases.Test_Case'Class) is
   begin

      Assert
        (Condition => String_Compression ("a") = "a",
         Message   => "Test 1 failed");
      Assert
        (Condition => String_Compression ("abcdef") = "abcdef",
         Message   => "Test 2 failed");
      Assert
        (Condition => String_Compression ("aabcccccaaa") = "a2b1c5a3",
         Message   => "Test 3 failed");
      Assert
        (Condition => String_Compression ("aaaabbbbc") = "a4b4c1",
         Message   => "Test 4 failed");
      Assert
        (Condition => String_Compression ("aaaaaaaaaaaabbb") = "a12b3",
         Message   => "Test 5 failed");

   end Test_String_Compression;

end Ex_1_6_String_Compression;
