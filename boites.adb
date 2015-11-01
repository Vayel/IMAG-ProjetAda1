-- Ce module orchestre les autres modules. C'est le point d'entrée de
-- l'application.

with Ada.Text_IO; use Ada.Text_IO;
with IO;          use IO;
with Boite;       use Boite;


procedure boites is
  cmd : Commande;
  b : Boite;
  svg : String;
begin
  cmd := creeCommande;
  b := creeBoite(cmd);
  svg := boiteVersSVG(b);
  boiteVersFichier(svg);
end boites;
