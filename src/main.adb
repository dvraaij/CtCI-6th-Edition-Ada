with AUnit.Run;
with AUnit.Reporter.Text;

with CtCI_Test_Suite;

procedure Main is

   procedure Run is new AUnit.Run.Test_Runner (CtCI_Test_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;

begin
   Run (Reporter);
end Main;
