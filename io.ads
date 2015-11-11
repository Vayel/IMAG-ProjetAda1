-- Ce module gère l'interaction avec le monde extérieur. IO signifie 
-- Input/Output.


with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Repr;                  use Repr;


package IO is
  ERREUR_COMMANDE : exception;

  type Parametres is record
    cmd: Commande;
    fname: Unbounded_String := to_unbounded_string(""); 
  end record;

  -- Lit l'entrée standard et crée un objet Commande à partir des paramètres
  -- fournis.
  function recupereParametres return Parametres;

  -- Affichage d'une boite

  COULEUR_TRAIT : constant String := "FF0000"; -- Code hexa 
  EPAISSEUR_TRAIT : constant Float := 0.1;

  function facetteVersSVG(f: Facette; x: Integer) return String;
  function pieceVersSVG(p: Piece; x: Integer) return String;
  function boiteVersSVG(b: Boite) return String;
end IO;
