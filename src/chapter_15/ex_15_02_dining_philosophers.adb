with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Text_IO;      use Ada.Text_IO;

package body Ex_15_02_Dining_Philosophers is

   -- NOTE : This implementation of the dining philosophers is based on the
   --        example which can be found at
   --
   --        http://www.cs.mtu.edu/~shene/FORUM/Taiwan-Forum/...
   --           ComputerScience/004-Concurrency/WWW/SLIDES/14-Ada-Tasking.pdf

   procedure Dining_Philosophers is

      -- Number of philosophers and chopsticks
      subtype Id is Positive range 1 .. 5;

      task type Philosopher is
         entry Set_Id (K : Id);
      end Philosopher;

      task type Chopstick is
         entry Pick_Up;
         entry Put_Down;
      end Chopstick;

      -- Actual chopsticks and philosophers
      Chop : array (Id) of Chopstick;
      Phil : array (Id) of Philosopher;

      -- Behavior of the chopstick is a mutex. The infinite loop within
      -- the body requires subsequent calls to "Pick_Up" and "Put_Down" (only
      -- in that order) or allows to be terminated.
      task body Chopstick is
      begin
         loop
            select
               accept Pick_Up;
               accept Put_Down;
            or
               terminate;
            end select;
         end loop;
      end Chopstick;

      -- Behavior of the philosopher
      task body Philosopher is

         I     : Id;  -- Id of philospoher
         Left  : Id;  -- Id chopstick left-side
         Right : Id;  -- Id chopstick right-side

         Dinners     : Natural  := 0;
         Dinners_Max : constant := 100;

      begin

         -- Wait for an identity to be assigned
         accept Set_Id (K : Id) do
            I := K;
         end Set_Id;

         -- Chopsticks Id's on left and right
         Left  := I;
         Right := I mod 5 + 1;

         -- Eat and think loop
         while Dinners < Dinners_Max loop

            -- Resource hierarchy solution (as proposed by Dijkstra)
            --
            -- The program can easily be forced into a deadlock by inserting
            -- a small delay between the chopstick pickups, i.e.
            --
            --   Chop (Left ).Pick_Up; delay (10.0e-6);
            --   Chop (Right).Pick_Up;
            --
            -- The deadlock can be prevented by requiring the philosophers to
            -- always first pickup the chopstick (left or right) with the
            -- lowest number, and only then pickup the one with the highest
            -- number. The solution, however, does not balance the amount
            -- of dinners eaten between the philosophers. See also:
            --
            --   https://en.wikipedia.org/wiki/Dining_philosophers_problem

            -- Think
            Chop (Positive'Min (Left, Right) ).Pick_Up; delay (10.0e-6);
            Chop (Positive'Max (Left, Right) ).Pick_Up;
            -- Eat
            Chop (Right).Put_Down;
            Chop (Left ).Put_Down;

            Dinners := Dinners + 1;

            Put_Line ("Philosopher" & Positive'Image (I) &
                        " had dinner" & Positive'Image (Dinners) & " times.");
         end loop;

         Put_Line ("Philosopher" & Positive'Image (I) & " finished.");

      end Philosopher;

   begin   -- Main

      -- Assign Id to each philosopher
      for K in Id loop
         Phil (K).Set_Id (K);
      end loop;

   end Dining_Philosophers;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Dining_Philosophers
     (T : in out Test_Cases.Test_Case'Class) is
   begin
      Dining_Philosophers;
      Assert (True, "Test 1 failed");
   end Test_Dining_Philosophers;

end Ex_15_02_Dining_Philosophers;
