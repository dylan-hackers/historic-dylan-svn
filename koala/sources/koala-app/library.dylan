Module:   dylan-user
Synopsis: Koala HTTP Server Application
Author:   Carl Gay

define library koala-app
  use dylan;
  use koala;
end;


define module koala-app
  use dylan;
  use koala, import: { start-server };
end;

