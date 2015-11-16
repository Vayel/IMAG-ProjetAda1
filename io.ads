-- Ce module gère l'interaction avec le monde extérieur. IO signifie 
-- Input/Output.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Repr;                  use Repr;


package IO is
  ERREUR_COMMANDE: exception;
  ERREUR_DESSIN: exception;

  type Parametres is record
    cmd: Commande;
    fname: Unbounded_String := to_unbounded_string(""); 
  end record;

  -- Lit l'entrée standard et crée un objet Commande à partir des paramètres
  -- fournis.
  function recupereParametres return Parametres;

  -- Affichage d'une boite

  NB_FACETTES: constant Integer := 15;
  NB_FAC_PAR_PIECE: constant Integer := 5;
  MARGE: constant Float := 10.0; -- Marge entre les dessins des facettes
  COULEUR_TRAIT: constant String := "FF0000";
  EPAISSEUR_TRAIT: constant Float := 0.1;

  function boiteVersSVG(b: Boite) return String;
end IO;
