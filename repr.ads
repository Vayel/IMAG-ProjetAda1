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

  -- Exemple de morceau de facette
  --           
  -- Coin      Côté
  -- <-><------------------>
  --            Centre
  --          <-------->
  --  Extrémité1     Extrémité2 
  --    <---->          <-->    
  --  _        __    __
  -- |_|______|  |__|  |___
  --   |                   |
  --   |                   |

  subtype Coin is Boolean;

  -- Une facette a quatre coins, chacun étant vide (un point) ou plein
  -- (un carré de côté l'épaisseur).
  -- Dans l'exemple ci-dessus, on aurait : [true, false, ...]
  type tCoins is array(1..4) of Coin;

  -- Un Créneau est un trou ou une encoche.
  type Creneau is record
    lon: Mesure;
    plein: Boolean;
  end record;

  type Creneaux is array(Integer range <>) of Creneau;

  -- Le centre d'un côté d'une facette est la partie contenant les n créneaux
  -- de taille connue q.
  type CentreCote is record
    nbCre: Natural; -- Un créneaux est une queue ou une encoche
    creExtrPlein: Boolean; -- Si les créneaux aux extrémités sont pleins.
  end record;

  -- Un côté est un créneau de longueur indéterminée (une extrémité), puis une
  -- succession de créneaux de longueur q, puis une extrémité.
  type Cote is record
    extr1: Creneau;
    centre: CentreCote;
    extr2: Creneau;
  end record;

  -- Le côté dessiné plus haut serait représenté comme ça :
  -- (
  --    extr1 => (taille => 6, plein => false),
  --    centre => (nbCre => 3, tailleCre => 2, creExtrPlein => true),
  --    extr2 => (taille => 3, plein => false)
  -- )

  type tCotes is array(1..4) of Cote;

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

  -- Utilitaires

  function coteVersCreneaux(c: Cote; lonCre: Mesure) return Creneaux;
  function crePrecCoinPlein(f: Facette; nCoin: Natural; lonCre: Mesure) return Boolean;
  function creSuivCoinPlein(f: Facette; nCoin: Natural; lonCre: Mesure) return Boolean;

  -- Création de la boite à partir de la commande

  function creeBoite(cmd: Commande) return Boite;
end Repr;
