-- Ce module orchestre les autres modules. C'est le point d'entrée de
-- l'application.

with Ada.Text_IO; use Ada.Text_IO;
with IO;          use IO;
with Boite;       use Boite;


procedure boites is
  param: Parametres;
  b : Boite;
  svg : String;
  f: File_type;
begin
  param := litParametres;
  b := creeBoite(param.cmd);
  svg := boiteVersSVG(b);

  create(f, name => param.fname);
  put(f, svg);
  close(f);
end boites;
