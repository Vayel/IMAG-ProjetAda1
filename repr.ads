-- Ce module représente les données relatives à une boite.

package Repr is
  NB_MIN_CRE : Float := 3.0;

  subtype Mesure is Float;
  
  type Commande is record
    e: Mesure := 0.0; -- épaisseur
    lon: Mesure := 0.0; -- longueur
    lar: Mesure := 0.0; -- largeur
    lonCre: Mesure := 0.0; -- longueur des créneaux
    hExt: Mesure := 0.0; -- hauteur extérieure
    hInt: Mesure := 0.0; -- hauteur intérieure
  end record;

  -- Une facette a quatre coins, chacun étant vide ou plein.
  type tCoins is array(0..3) of Boolean;

  type Creneau is record
    taille: Mesure;
    plein: Boolean;
  end record;

  type Creneaux is array(Integer range <>) of Creneau;

  type CentreCote is record
    nbCre: Natural; -- Un créneaux est une queue ou une encoche
    tailleCre: Mesure;
    creExtrPlein: Boolean; -- Si les créneaux aux extrémités sont pleins
  end record;

  -- Un côté est un créneau de longueur indéterminée (une extrémité), puis une
  -- succession de créneaux de longueur q, puis une extrémité.
  type Cote is record
    extr1: Creneau;
    centre: CentreCote;
    extr2: Creneau;
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
    enLon: Facette;
    enLar: Facette;
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

  function creePiece(l1, l2, h, e, lonCre: Mesure) return Piece;
  function creeBoite(cmd: Commande) return Boite;

  -- Utilitaires
  function coteVersCreneaux(c: Cote) return Creneaux;
end Repr;
