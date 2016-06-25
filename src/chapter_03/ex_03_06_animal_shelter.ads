with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

        with Ada.Strings.Bounded;
private with Ada.Containers.Doubly_Linked_Lists;

package Ex_03_06_Animal_Shelter is

   -- Bounded string for pet name
   package String_20 is
     new Ada.Strings.Bounded.Generic_Bounded_Length (20);
   use String_20;

   --------------
   -- Schelter --
   --------------

   type Animal_Kind is (Dog, Cat);

   type Animal is
      record
         Kind : Animal_Kind;
         Name : String_20.Bounded_String;
      end record;

   type Shelter is private;

   procedure Enqueue     (S : in out Shelter; A : Animal);
   function  Dequeue_Any (S : in out Shelter) return Animal;
   function  Dequeue_Dog (S : in out Shelter) return Animal;
   function  Dequeue_Cat (S : in out Shelter) return Animal;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Animal_Shelter
     (T : in out Test_Cases.Test_Case'Class);

private

   package LL_Shelter is
     new Ada.Containers.Doubly_Linked_Lists (Animal);
   use LL_Shelter;

   type Shelter is
     new LL_Shelter.List with null record;

end Ex_03_06_Animal_Shelter;
