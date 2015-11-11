-- Ce module orchestre les autres modules. C'est le point d'entrée de
-- l'application.

with Ada.Text_IO; use Ada.Text_IO;
with IO;          use IO;
with Repr;        use Repr;


procedure boites is
  param: Parametres;
  b: Boite;
  f: File_type;
begin
  param := recupereParametres;
  -- b := creeBoite(param.cmd);

  -- create(f, name => to_string(param.fname));
  -- put(f, boiteVersSVG(b));
  -- close(f);
end boites;
