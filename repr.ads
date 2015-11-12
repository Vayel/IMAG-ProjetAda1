-- Ce module représente les données relatifs à une boite.

package Repr is
  type Mesure is new Float;

  type Commande is record
    epaisseur: Mesure := 0.0;
    longueur: Mesure := 0.0;
    largeur: Mesure := 0.0;
    longueurQueues: Mesure := 0.0;
    hauteurExt: Mesure := 0.0;
    hauteurInt: Mesure := 0.0;
  end record;

  -- Une facette a quatre coins, chacun étant vide ou plein.
  type tCoins is array(0..3) of Boolean;

  type Extremite is record
    taille: Mesure;
    pleine: Boolean;
  end record;

  -- Un côté est un créneau de longueur indéterminée (une extrémité), puis une
  -- succession de créneaux de longueur q, puis une extrémité.
  type Cote is record
    extremite1: Extremite;
    nbCreneaux: Natural; -- Un créneaux est une queue ou une encoche
    typeCreneau: Boolean; -- Type du créneau majoritaire
    extremite2: Extremite;
  end record;

  type tCotes is array(0..3) of Cote;

  -- Une facette désigne ce qu'on découpe, puis qu'on assemble à l'aide des
  -- encoches.
  type Facette is record
    coins: tCoins;
    cotes: tCotes;
  end record;

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

  function creeFacette(cmd: Commande) return Facette;
  function creePiece(cmd: Commande) return Piece;
  function creeBoite(cmd: Commande) return Boite;
end Repr;
