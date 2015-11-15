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

  -- Exemple de morceau de facette
  --           
  -- Coin      C�t�
  -- <-><------------------>
  --            Centre
  --          <-------->
  --  Extr�mit�1     Extr�mit�2 
  --    <---->          <-->    
  --  _        __    __
  -- |_|______|  |__|  |___
  --   |                   |
  --   |                   |

  subtype Coin is Boolean;

  -- Une facette a quatre coins, chacun �tant vide (un point) ou plein
  -- (un carr� de c�t� l'�paisseur).
  -- Dans l'exemple ci-dessus, on aurait : [true, false, ...]
  type tCoins is array(1..4) of Coin;

  -- Un Cr�neau est un trou ou une encoche.
  type Creneau is record
    lon: Mesure;
    plein: Boolean;
  end record;

  type Creneaux is array(Integer range <>) of Creneau;

  -- Le centre d'un c�t� d'une facette est la partie contenant les n cr�neaux
  -- de taille connue q.
  type CentreCote is record
    nbCre: Natural; -- Un cr�neaux est une queue ou une encoche
    creExtrPlein: Boolean; -- Si les cr�neaux aux extr�mit�s sont pleins.
  end record;

  -- Un c�t� est un cr�neau de longueur ind�termin�e (une extr�mit�), puis une
  -- succession de cr�neaux de longueur q, puis une extr�mit�.
  type Cote is record
    extr1: Creneau;
    centre: CentreCote;
    extr2: Creneau;
  end record;

  -- Le c�t� dessin� plus haut serait repr�sent� comme �a :
  -- (
  --    extr1 => (taille => 6, plein => false),
  --    centre => (nbCre => 3, tailleCre => 2, creExtrPlein => true),
  --    extr2 => (taille => 3, plein => false)
  -- )

  type tCotes is array(1..4) of Cote;

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

  -- Utilitaires

  function coteVersCreneaux(c: Cote; lonCre: Mesure) return Creneaux;
  function crePrecCoinPlein(f: Facette; nCoin: Natural; lonCre: Mesure) return Boolean;
  function creSuivCoinPlein(f: Facette; nCoin: Natural; lonCre: Mesure) return Boolean;

  -- Cr�ation de la boite � partir de la commande

  function creeBoite(cmd: Commande) return Boite;
end Repr;
