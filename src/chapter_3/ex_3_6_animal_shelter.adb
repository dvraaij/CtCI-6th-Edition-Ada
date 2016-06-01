with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Text_IO;      use Ada.Text_IO;

package body Ex_3_6_Animal_Shelter is

   ----------------------------------------------------------------------------
   procedure Enqueue (S : in out Shelter; A : Animal) is
   begin
      S.Append (A);
   end Enqueue;

   ----------------------------------------------------------------------------
   function Dequeue_Any (S : in out Shelter) return Animal is

      A : Animal;

   begin

      -- Check of the shelter is empty
      if Is_Empty (S) then
         raise Constraint_Error with "Shelter is empty";
      end if;

      -- Obtain animal
      A := First_Element (S);

      -- Delete from list
      Delete_First (S);

      -- Return the animal
      return A;

   end Dequeue_Any;

   ----------------------------------------------------------------------------
   function Dequeue_Dog (S : in out Shelter) return Animal is

      C : Cursor;
      A : Animal;

   begin

      -- Check of the shelter is empty
      if Is_Empty (S) then
         raise Constraint_Error with "Shelter is empty";
      end if;

      -- Find a dog
      C := S.First;
      while Element (C).Kind /= Dog loop

         -- Step back
         C := Next (C);

         -- Check we've iterated over all available animals
         if C = No_Element then
            raise Constraint_Error with "No dogs in shelter";
         end if;

      end loop;

      -- Obtain animal
      A := Element (C);

      -- Delete from the list
      Delete (S, C, 1);

      -- Return the animal
      return A;

   end Dequeue_Dog;

   ----------------------------------------------------------------------------
   function Dequeue_Cat (S : in out Shelter) return Animal is

      C : Cursor;
      A : Animal;

   begin

      -- Check of the shelter is empty
      if Is_Empty (S) then
         raise Constraint_Error with "Shelter is empty";
      end if;

      -- Find a cat
      C := S.First;
      while Element (C).Kind /= Cat loop

         -- Step back
         C := Next (C);

         -- Check we've iterated over all available animals
         if C = No_Element then
            raise Constraint_Error with "No cats in shelter";
         end if;

      end loop;

      -- Obtain animal
      A := Element (C);

      -- Delete from list
      Delete (S, C, 1);

      -- Return the animal
      return A;

   end Dequeue_Cat;

   ----------------------------------------------------------------------------
   procedure Test_Animal_Shelter (T : in out Test_Cases.Test_Case'Class) is

      S : Shelter;

   begin

      -- Put some animals into the shelter
      Enqueue (S, Animal'(Cat, To_Bounded_String ("Balthazar" )));
      Enqueue (S, Animal'(Dog, To_Bounded_String ("Dommel"    )));
      Enqueue (S, Animal'(Dog, To_Bounded_String ("Bobby"     )));
      Enqueue (S, Animal'(Cat, To_Bounded_String ("Tom"       )));
      Enqueue (S, Animal'(Dog, To_Bounded_String ("Droopy"    )));
      Enqueue (S, Animal'(Dog, To_Bounded_String ("Scooby Doo")));
      Enqueue (S, Animal'(Cat, To_Bounded_String ("Garfield"  )));
      Enqueue (S, Animal'(Dog, To_Bounded_String ("Odie"      )));
      Enqueue (S, Animal'(Dog, To_Bounded_String ("Snoopy"    )));

      -- Check animal being picked up from the shelter
      Assert (To_String (Dequeue_Cat (S).Name) = "Balthazar", "Test 1 failed");
      Assert (To_String (Dequeue_Cat (S).Name) = "Tom"      , "Test 2 failed");
      Assert (To_String (Dequeue_Dog (S).Name) = "Dommel"   , "Test 3 failed");
      Assert (To_String (Dequeue_Any (S).Name) = "Bobby"    , "Test 4 failed");
      Assert (To_String (Dequeue_Cat (S).Name) = "Garfield" , "Test 5 failed");
      Assert (To_String (Dequeue_Any (S).Name) = "Droopy"   , "Test 6 failed");

      declare
         A : Animal;
      begin
         A := Dequeue_Cat (S);
         Assert (False, "Test 7 failed");
      exception
         when Constraint_Error => null;
      end;

   end Test_Animal_Shelter;

end Ex_3_6_Animal_Shelter;
