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

  -- Une tuile désigne un trou ou une queue.
  type Tuile is record
    taille: Mesure := 0;
    pleine: Boolean := True; -- Par défaut, c'est une queue.
  end record;

  -- Une facette désigne ce qu'on découpe, puis qu'on assemble à l'aide des
  -- encoches.
  -- Une facette est un rectangle plein entouré de tuiles. Ici, on part d'un
  -- coin et liste les différentes tuiles dans le sens horaire.
  type Facette is array(Integer range <>) of Tuile; 

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

  function commandeIncomplete(cmd: Commande) return Boolean;
  function commandeIrrealisable(cmd: Commande) return Boolean;

  -- Création de la boite à partir de la commande

  function creeFacetteLong(cmd: Commande) return Facette;
  function creeFacetteLarg(cmd: Commande) return Facette;
  function creeFacetteFond(cmd: Commande) return Facette;
  function creePieceExt(cmd: Commande) return Piece;
  function creePieceInt(cmd: Commande) return Piece;
  function creeBoite(cmd: Commande) return Boite;
end Boite;
