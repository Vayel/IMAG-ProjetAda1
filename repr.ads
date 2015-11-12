-- Ce module repr�sente les donn�es relatifs � une boite.

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

  -- Une facette a quatre coins, chacun �tant vide ou plein.
  type tCoins is array(0..3) of Boolean;

  type Extremite is record
    taille: Mesure;
    pleine: Boolean;
  end record;

  -- Un c�t� est un cr�neau de longueur ind�termin�e (une extr�mit�), puis une
  -- succession de cr�neaux de longueur q, puis une extr�mit�.
  type Cote is record
    extremite1: Extremite;
    nbCreneaux: Natural; -- Un cr�neaux est une queue ou une encoche
    typeCreneau: Boolean; -- Type du cr�neau majoritaire
    extremite2: Extremite;
  end record;

  type tCotes is array(0..3) of Cote;

  -- Une facette d�signe ce qu'on d�coupe, puis qu'on assemble � l'aide des
  -- encoches.
  type Facette is record
    coins: tCoins;
    cotes: tCotes;
  end record;

  -- Une pi�ce a trois types de facettes : celle formant le fond, celles ayant
  -- pour dimensions la longueur et la hauteur et celles ayant pour dimensions
  -- la largeur et la hauteur.
  type Piece is record
    longueur: Facette;
    largeur: Facette;
    fond: Facette;
  end record;

  -- Une boite a deux types de pi�ces : celles ext�rieures et celle
  -- int�rieure.
  type Boite is record
    ext: Piece;
    int: Piece;
  end record;

  -- V�rification de la commande

  function commandeIncomplete(cmd: Commande) return Boolean;
  function commandeIrrealisable(cmd: Commande) return Boolean;

  -- Cr�ation de la boite � partir de la commande

  function creeFacette(cmd: Commande) return Facette;
  function creePiece(cmd: Commande) return Piece;
  function creeBoite(cmd: Commande) return Boite;
end Repr;
