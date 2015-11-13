-- Ce module repr�sente les donn�es relatives � une boite.

package Repr is
  NB_MIN_CRE : Float := 3.0;

  subtype Mesure is Float;
  
  type Commande is record
    e: Mesure := 0.0; -- �paisseur
    lon: Mesure := 0.0; -- longueur
    lar: Mesure := 0.0; -- largeur
    lonCre: Mesure := 0.0; -- longueur des cr�neaux
    hExt: Mesure := 0.0; -- hauteur ext�rieure
    hInt: Mesure := 0.0; -- hauteur int�rieure
  end record;

  -- Une facette a quatre coins, chacun �tant vide ou plein.
  type tCoins is array(0..3) of Boolean;

  type Creneau is record
    taille: Mesure;
    plein: Boolean;
  end record;

  type Creneaux is array(Integer range <>) of Creneau;

  type CentreCote is record
    nbCre: Natural; -- Un cr�neaux est une queue ou une encoche
    tailleCre: Mesure;
    creExtrPlein: Boolean; -- Si les cr�neaux aux extr�mit�s sont pleins
  end record;

  -- Un c�t� est un cr�neau de longueur ind�termin�e (une extr�mit�), puis une
  -- succession de cr�neaux de longueur q, puis une extr�mit�.
  type Cote is record
    extr1: Creneau;
    centre: CentreCote;
    extr2: Creneau;
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
    enLon: Facette;
    enLar: Facette;
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

  function creePiece(l1, l2, h, e, lonCre: Mesure) return Piece;
  function creeBoite(cmd: Commande) return Boite;

  -- Utilitaires
  function coteVersCreneaux(c: Cote) return Creneaux;
end Repr;
