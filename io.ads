-- Ce module gère l'interaction avec le monde extérieur. IO signifie 
-- Input/Output.

with Boite; use Boite;


package IO is
  ERREUR_COMMANDE : exception;

  type Parametres is record
    cmd: Commande;
    fname: String; 
  end record;

  -- Lit l'entrée standard et crée un objet Commande à partir des paramètres
  -- fournis.
  function litParametres return Parametres;

  -- Affichage d'une boite

  COULEUR_TRAIT : constant String := "#FF0000"; -- Hex
  EPAISSEUR_TRAIT : constant String := "0.1";

  function facetteVersSVG(f: Facette; x: Integer) return String;
  function pieceVersSVG(p: Piece; x: Integer) return String;
  function boiteVersSVG(b: Boite) return String;
end IO;
