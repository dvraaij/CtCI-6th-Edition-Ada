with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Ex_04_07_Build_Order is

   -----------
   -- Types --
   -----------

   subtype Project is Unbounded_String;

   type Projects is
     array (Positive range <>) of Project;

   type Dependency is
      record
         Root       : Project;
         Depends_On : Project;
      end record;

   type Dependencies is
     array (Natural range <>) of Dependency;

   type Build_Order is
     array (Positive range <>) of Project;

   ----------------
   -- Subprogram --
   ----------------

   function Determine_Build_Order
     (P : Projects; D : Dependencies) return Build_Order;

   ----------------
   -- Test cases --
   ----------------

   procedure Test_Build_Order
     (T : in out Test_Cases.Test_Case'Class);

end Ex_04_07_Build_Order;
