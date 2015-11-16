-- Conseil : commencer la lecture par la fonction creeBoite, tout en bas, puis
-- remonter les appels

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

  function indiceCotePrecCoin(iCoin: Integer) return Integer is
  begin
    return (iCoin - 2) mod 4 + 1;
  end;

  function creneauNonNul(c: Creneau) return Boolean is
  begin
    return c.lon > 0.0;
  end;

  function calculeNbCreCote(c: Cote) return Integer is
    n: Integer;
  begin
    n := c.centre.nbCre;

    if creneauNonNul(c.extr) then
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
      if creneauNonNul(c.extr) then
        cres(1) := c.extr;
        cres(n) := c.extr;

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

    cmp.extr := creneauComplementaire(c.extr); 
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

    if n mod 2 = 0 then -- Le nombre de créneaux doit être impair
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
   
  function creeCotePlat(lon: Mesure; plein: Boolean) return Cote is
    c: Cote;
  begin
    c.extr := (lon => lon/2.0, plein => plein);
    c.centre := (
      nbCre => 0,
      creExtrPlein => false
    );

    return c;
  end;

  function creeCoteSimple(n: Natural; lonExtr: Mesure) return Cote is
    -- Un côté simple est de la forme :
    --     __    __
    -- ___|  |__|  |___
    --
    -- Seuls le nombre de créneaux au centre et la longueur commune des
    -- extrémités varient.

    c: Cote;
  begin
    c.extr := (lon => lonExtr, plein => false);
    c.centre := (nbCre => n, creExtrPlein => true);

    return c;
  end;

  function crePrecCoin(f: Facette; nCoin: Natural; lonCre: Mesure) return Creneau is
    nCote: Natural;
    c: Cote;
  begin
    nCote := indiceCotePrecCoin(nCoin);
    c := f.cotes(nCote);
    
    declare
      cre: Creneaux(1..calculeNbCreCote(c));
    begin
      cre := coteVersCreneaux(f.cotes(nCote), lonCre);
      return cre(cre'last);
    end;
  end;

  function creSuivCoin(f: Facette; nCoin: Natural; lonCre: Mesure) return Creneau is
    nCote: Natural;
    c: Cote;
  begin
    nCote := nCoin;
    c := f.cotes(nCote);
    
    declare
      cre: Creneaux(1..calculeNbCreCote(c));
    begin
      cre := coteVersCreneaux(f.cotes(nCote), lonCre);
      return cre(1);
    end;
  end;

  -- Création de la boite à partir de la commande
  
  function creeFacetteFond(l1, l2, e, lonCre: Mesure) return Facette is
    f: Facette;
    coteLon, coteLar: Cote;
    lCoteLon, lCoteLar: Mesure;
    lonExtrLon, lonExtrLar: Mesure;
    nLon, nlar: Natural;
  begin
    -- Création du côté en longueur
    lCoteLon := l1 - 2.0 * e;
    nLon := calculeNbCreCentre(lCoteLon, lonCre);
    lonExtrLon := mesureExtr(lCoteLon, lonCre);
    coteLon := creeCoteSimple(nLon, lonExtrLon); 

    -- Création du côté en largeur
    lCoteLar := l2 - 2.0 * e;
    nLar := calculeNbCreCentre(lCoteLar, lonCre);
    lonExtrLar := mesureExtr(lCoteLar, lonCre);
    cotelar := creeCoteSimple(nLar, lonExtrLar); 

    -- On fait en sorte qu'un coin plein ne peut être entouré de deux creux :
    --  _
    -- |_|__
    --   |
    --
    -- On préfère :
    --
    --  ___
    -- |   |_
    -- |_
    --   |
    if lonExtrLon > 0.0 then
      f.coins := (others => false);
    else
      f.coins := (others => true);
    end if;

    f.cotes := (1|3 => coteLon, 2|4 => coteLar);

    return f;
  end;

  function creeFacetteEnLon(lon, h, e, lonCre: Mesure; coteBas: Cote; coinBas: Boolean) return Facette is
    f: Facette;
    coteVerti: Cote;
  begin
    coteVerti := creeCoteSimple(
      calculeNbCreCentre(h - 2.0 * e, lonCre),
      mesureExtr(h - 2.0 * e, lonCre)
    );

    f.cotes := (creeCotePlat(lon - 2.0 * e, true), coteVerti, coteBas, coteVerti);

    -- Par convention, les coins du haut sont pleins
    -- Le coin du bas est le complémentaire du coin correspondant sur la
    -- facette du fond
    f.coins := (1|2 => true, 3|4 => coinBas);

    return f;
  end;

  function creeFacetteEnLar(lon, e: Mesure; coteVerti, coteBas: Cote) return Facette is
    f: Facette;
  begin
    f.cotes := (
      creeCotePlat(lon - 2.0 * e, true),
      coteVerti, coteBas, coteVerti
    );

    f.coins := (others => false);

    return f;
  end;

  function creePiece(lon, lar, h, e, lonCre: Mesure) return Piece is
    p: Piece;
  begin
    p.lon := lon;
    p.lar := lar;
    p.h := h;

    p.fond := creeFacetteFond(lon, lar, e, lonCre);

    p.enLon := creeFacetteEnLon(
      lon, h, e, lonCre,
      coteComplementaire(p.fond.cotes(1)),
      coinComplementaire(p.fond.coins(1))
    );

    p.enLar := creeFacetteEnLar(
      lar, e,
      coteComplementaire(p.enLon.cotes(2)),
      coteComplementaire(p.fond.cotes(2))
    );
    
    return p;
  end;

  function creeBoite(cmd: Commande) return Boite is
    b: Boite;
  begin
    b.e := cmd.e;
    b.lonCre := cmd.lonCre;
    b.int := creePiece(cmd.lon - 2.0 * cmd.e, cmd.lar - 2.0 * cmd.e, cmd.hInt, cmd.e, cmd.lonCre);
    b.ext := creePiece(cmd.lon, cmd.lar, cmd.hExt/2.0, cmd.e, cmd.lonCre);

    return b;
  end;
end Repr;
