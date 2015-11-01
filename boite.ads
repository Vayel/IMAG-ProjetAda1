-- Ce module représente les données relatives à une boite et permet de
-- les afficher.

package Boite is
  type Mesure is new Natural;

  type Commande is record
    epaisseur: Mesure := 0;
    longueur: Mesure := 0;
    largeur: Mesure := 0;
    longueurQueues: Mesure := 0;
    hauteurExt: Mesure := 0;
    hauteurInt: Mesure := 0;
  end record;

  -- Une facette désigne ce qu'on découpe, puis qu'on assemble à l'aide des
  -- encoches. Une facette se représente par un polygone (une suite finie de
  -- points).
  type Facette; -- TODO

  -- Une pièce a trois types de facettes : celle formant le fond, celles ayant
  -- pour dimensions la longueur et la hauteur et celles ayant pour dimensions
  -- la largeur et la hauteur.
  type Piece is record
    longueur: Facette;
    largeur: Facette;
    fond: Facette;
  end record;

  -- Une boite a deux types de pièces : celles extérieures et celle
  -- intérieure.
  type Boite is record
    ext: Piece;
    int: Piece;
  end record;

  -- Vérification de la commande

  function commandeInvalide(cmd: Commande) return Boolean;

  -- Création de la boite à partir de la commande

  function creeFacetteLong(cmd: Commande) return Facette;
  function creeFacetteLarg(cmd: Commande) return Facette;
  function creeFacetteFond(cmd: Commande) return Facette;
  function creePieceExt(cmd: Commande) return Piece;
  function creePieceInt(cmd: Commande) return Piece;
  function creeBoite(cmd: Commande) return Boite;

  -- Affichage de la boite

  COULEUR_TRAIT : constant String := "255,0,0"; -- RGB
  EPAISSEUR_TRAIT : constant String := "0.1";

  function facetteVersSVG(f: Facette; x, y: Integer) return String;
  function pieceVersSVG(p: Piece; y: Integer) return String;
  function boiteVersSVG(b: Boite) return String;
end Boite;
