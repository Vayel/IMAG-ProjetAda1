with Ada.Text_IO; use Ada.Text_IO;

package body Repr is
  -- Vérification de la commande

  function commandeIncomplete(cmd: Commande) return Boolean is
  begin
    return (cmd.e = 0.0 or cmd.lon = 0.0 or cmd.lar = 0.0 or cmd.lonCre = 0.0
            or cmd.hExt = 0.0 or cmd.hInt = 0.0);
  end;

  function commandeIrrealisable(cmd: Commande) return Boolean is
    tropEpais, tropHaut, pasAssezHaut, qTropLong: Boolean;
  begin
    tropEpais := cmd.e > (cmd.lon / 2.0);
    tropEpais := tropEpais or cmd.e > (cmd.lar / 2.0);
    tropEpais := tropEpais or cmd.e > (cmd.hExt / 2.0);
    
    tropHaut := cmd.hInt > (cmd.hExt - 2.0 * cmd.e);
    pasAssezHaut := cmd.hInt <= (cmd.hExt / 2.0 - cmd.e);
    
    qTropLong := (cmd.lonCre * NB_MIN_CRE) > (cmd.lon - 2.0 * cmd.e);
    qTropLong := qTropLong or (cmd.lonCre * NB_MIN_CRE) > (cmd.lar - 2.0 * cmd.e);
    qTropLong := qTropLong or (cmd.lonCre * NB_MIN_CRE) > (cmd.hExt / 2.0 - cmd.e);
    
    return tropEpais or tropHaut or pasAssezHaut or qTropLong;
  end;

  -- Utilitaires

  function coinPrecCoteIndice(i: Integer) return Integer is
  begin
    return (i - 2) mod 4 + 1;
  end;

  function creneauNonNul(c: Creneau) return Boolean is
  begin
    return c.lon > 0.0;
  end;

  function calculeNbCreCote(c: Cote) return Integer is
    n: Integer;
  begin
    n := c.centre.nbCre;

    if creneauNonNul(c.extr1) then
      n := n + 2;
    end if;

    return n;
  end;

  function coteVersCreneaux(c: Cote; lonCre: Mesure) return Creneaux is
    n: Integer;
    cre: Creneau;
  begin
    cre.lon := lonCre;
    n := calculeNbCreCote(c);

    declare
      cres: Creneaux(1..n);
    begin
      if creneauNonNul(c.extr1) then
        cres(1) := c.extr1;
        cres(n) := c.extr2;

        for i in 2..(n-1) loop
          if i mod 2 = 0 then
            cre.plein := c.centre.creExtrPlein;
          else
            cre.plein := not c.centre.creExtrPlein;
          end if;

          cres(i) := cre;
        end loop;
      else
        for i in 1..n loop
          if i mod 2 = 1 then
            cre.plein := c.centre.creExtrPlein;
          else
            cre.plein := not c.centre.creExtrPlein;
          end if;

          cres(i) := cre;
        end loop;
      end if;

      return cres;
    end;
  end;

  function creneauComplementaire(cr: Creneau) return Creneau is
    comp: Creneau;
  begin
    comp.lon := cr.lon;
    comp.plein := not cr.plein;

    return comp;
  end;

  function coteComplementaire(c: Cote) return Cote is
    cmp: Cote;
  begin
    -- Les trous de c deviennent des encoches pour cmp et ses encoches des
    -- trous.
    -- Il ne faut pas oublier d'inverser l'ordre des éléments. En effet, le
    -- côté complémentaire se retrouve à l'opposé sur l'autre facette
    -- donc est lu dans l'autre sens.

    cmp.extr1 := creneauComplementaire(c.extr2); 
    cmp.extr2 := creneauComplementaire(c.extr1); 
    cmp.centre := (c.centre.nbCre, not c.centre.creExtrPlein);

    return cmp;
  end;

  function coinComplementaire(c: Coin) return Coin is
  begin
    return not c;
  end;
  
  function calculeNbCreCentre(lCote, lCre: Mesure) return Natural is
    n: Integer;
  begin
    n := Integer(Float'truncation(lCote / lCre));

    if n mod 2 = 0 then
      n := n - 1;
    end if;

    return n;
  end;

  function mesureExtr(lonCote, lonCre: Mesure) return Mesure is
    n: Float;
  begin
    n := Float(calculeNbCreCentre(lonCote, lonCre));

    return (lonCote - n * lonCre) / 2.0;
  end;
    
  function creeCoteSimple(n: Natural; lonExtr: Mesure) return Cote is
    -- Un côté simple est de la forme :
    --     __    __
    -- ___|  |__|  |___
    --
    -- Seuls le nombre de créneaux au centre et la lon commune des
    -- extrémités varient.

    c: Cote;
  begin
    c.extr1 := (lon => lonExtr, plein => false);
    c.centre := (nbCre => n, creExtrPlein => true);
    c.extr2 := (lon => lonExtr, plein => false);

    return c;
  end;

  function creeCotePlat(lon: Mesure; plein: Boolean) return Cote is
    c: Cote;
  begin
    c.extr1 := (lon => lon/2.0, plein => plein);
    c.extr2 := (lon => lon/2.0, plein => plein);
    c.centre := (
      nbCre => 0,
      creExtrPlein => false
    );

    return c;
  end;

  function crePrecCoinPlein(f: Facette; nCoin: Natural; lonCre: Mesure) return Boolean is
    nCote: Natural;
    c: Cote;
  begin
    nCote := coinPrecCoteIndice(nCoin);
    c := f.cotes(nCote);
    
    declare
      cre: Creneaux(1..calculeNbCreCote(c));
    begin
      cre := coteVersCreneaux(f.cotes(nCote), lonCre);
      return cre(cre'last).plein;
    end;
  end;

  function creSuivCoinPlein(f: Facette; nCoin: Natural; lonCre: Mesure) return Boolean is
    nCote: Natural;
    c: Cote;
  begin
    nCote := nCoin;
    c := f.cotes(nCote);
    
    declare
      cre: Creneaux(1..calculeNbCreCote(c));
    begin
      cre := coteVersCreneaux(f.cotes(nCote), lonCre);
      return cre(1).plein;
    end;
  end;

  -- Création de la boite à partir de la commande
  
  function creeFacetteFond(l1, l2, e, lonCre: Mesure) return Facette is
    f: Facette;
    cote1, cote2: Cote;
    lCote1, lCote2: Mesure;
    lonExtr1, lonExtr2: Mesure;
    n1, n2: Natural;
  begin
    lCote1 := l1 - 2.0 * e;
    n1 := calculeNbCreCentre(lCote1, lonCre);
    lonExtr1 := mesureExtr(lCote1, lonCre);
    cote1 := creeCoteSimple(n1, lonExtr1); 

    lCote2 := l2 - 2.0 * e;
    n2 := calculeNbCreCentre(lCote2, lonCre);
    lonExtr2 := mesureExtr(lCote2, lonCre);
    cote2 := creeCoteSimple(n2, lonExtr2); 

    if lonExtr1 > 0.0 then
      f.coins := (others => false);
    else
      f.coins := (others => true);
    end if;

    f.cotes := (cote1, cote2, cote1, cote2);

    return f;
  end;

  function creeFacetteEnLong(lon, h, e, lonCre: Mesure; cBas: Cote; coinBas: Boolean) return Facette is
    f: Facette;
    cVerti: Cote;
  begin
    cVerti := creeCoteSimple(
      calculeNbCreCentre(h - 2.0 * e, lonCre),
      mesureExtr(h - 2.0 * e, lonCre)
    );

    f.cotes := (creeCotePlat(lon - 2.0 * e, true), cVerti, cBas, cVerti);

    -- Par convention, les coins du haut sont pleins
    f.coins := (true, true, coinBas, coinBas);

    return f;
  end;

  function creeFacetteEnLar(lon, e: Mesure; coteVerti, coteBas: Cote) return Facette is
    f: Facette;
  begin
    f.cotes := (creeCotePlat(lon - 2.0 * e, true), coteVerti, coteBas, coteVerti);

    f.coins := (false, false, false, false);

    return f;
  end;

  function creePiece(l1, l2, h, e, lonCre: Mesure) return Piece is
    p: Piece;
  begin
    p.fond := creeFacetteFond(l1, l2, e, lonCre);
    p.enLon := creeFacetteEnLong(
      l1,
      h,
      e,
      lonCre,
      coteComplementaire(p.fond.cotes(1)),
      coinComplementaire(p.fond.coins(1))
    );
    p.enLar := creeFacetteEnLar(
      l2,
      e,
      coteComplementaire(p.enLon.cotes(2)),
      coteComplementaire(p.fond.cotes(2))
    );
    
    return p;
  end;

  function creeBoite(cmd: Commande) return Boite is
    b: Boite;
  begin
    b.int := creePiece(cmd.lon, cmd.lar, cmd.hInt, cmd.e, cmd.lonCre);
    b.ext := creePiece(cmd.lon, cmd.lar, cmd.hExt/2.0, cmd.e, cmd.lonCre);

    return b;
  end;
end Repr;
