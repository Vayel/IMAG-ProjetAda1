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

  function coteVersCreneaux(c: Cote) return Creneaux is
    n: Integer;
    cre: Creneau;
  begin
    n := 2 + c.centre.nbCre; -- Deux extrémités + des créneaux classiques
    cre.taille := c.centre.tailleCre;

    declare
      cres: Creneaux(1..n);
    begin
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

      return cres;
    end;
  end;

  function creneauComplementaire(cr: Creneau) return Creneau is
    comp: Creneau;
  begin
    comp.taille := cr.taille;
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
    cmp.centre := (c.centre.nbCre, c.centre.tailleCre, not c.centre.creExtrPlein);

    return cmp;
  end;

  function coinComplementaire(c: Coin) return Coin is
  begin
    return not c;
  end;
  
  function calculeNbCre(lCote, lCre: Mesure) return Natural is
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
    n := Float(calculeNbCre(lonCote, lonCre));

    return (lonCote - n * lonCre) / 2.0;
  end;
    
  function creeCoteSimple(n: Natural; lonCre: Mesure; tailleExtr: Mesure) return Cote is
    -- Un côté simple est de la forme :
    --     __    __
    -- ___|  |__|  |___
    --
    -- Seuls le nombre de créneaux au centre et la taille commune des
    -- extrémités varient.

    c: Cote;
  begin
    c.extr1 := (taille => tailleExtr, plein => false);
    c.centre := (nbCre => n, tailleCre => lonCre, creExtrPlein => true);
    c.extr2 := (taille => tailleExtr, plein => false);

    return c;
  end;

  function creeCotePlat(lon: Mesure; plein: Boolean) return Cote is
    c: Cote;
  begin
    c.extr1 := (taille => 0.0, plein => false);
    c.extr2 := (taille => 0.0, plein => false);
    c.centre := (
      nbCre => 1,
      tailleCre => lon,
      creExtrPlein => plein
    );

    return c;
  end;

  -- Création de la boite à partir de la commande
  
  function creeFacetteFond(l1, l2, e, lonCre: Mesure) return Facette is
    f: Facette;
    coteHori, coteVerti: Cote;
    lCote1, lCote2: Mesure;
    tailleExtr: Mesure;
    n: Natural;
  begin
    f.coins := (true, true, true, true); -- Par convention, tous les coins sont pleins 
    
    lCote1 := l1 - 2.0 * e;
    lCote2 := l2 - 2.0 * e;

    n := calculeNbCre(lCote1, lonCre);
    tailleExtr := mesureExtr(lCote1, lonCre);
    coteHori := creeCoteSimple(n, lonCre, tailleExtr); 

    n := calculeNbCre(lCote2, lonCre);
    tailleExtr := mesureExtr(lCote2, lonCre);
    coteVerti := creeCoteSimple(n, lonCre, tailleExtr); 

    f.cotes := (coteHori, coteVerti, coteHori, coteVerti);

    return f;
  end;

  function creeFacetteEnLong(lon, h, lonCre: Mesure; cBas: Cote) return Facette is
    f: Facette;
    cVerti: Cote;
    n: Natural;
    tailleExtr: Mesure;
  begin
    n := calculeNbCre(h, lonCre);
    tailleExtr := mesureExtr(h, lonCre);
    cVerti := creeCoteSimple(n, lonCre, tailleExtr);

    f.cotes := (creeCotePlat(lon, true), cVerti, cBas, cVerti);

    -- Par convention, les coins du haut sont pleins, ceux du bas vides, pour
    -- coïncider avec la facette du fond
    f.coins := (true, true, false, false);

    return f;
  end;

  function creeFacetteEnLar(lon: Mesure; coteVerti, coteBas: Cote) return Facette is
    f: Facette;
  begin
    f.cotes := (creeCotePlat(lon, true), coteVerti, coteBas, coteVerti);

    f.coins := (false, false, false, false);

    return f;
  end;

  function creePiece(l1, l2, h, e, lonCre: Mesure) return Piece is
    p: Piece;
  begin
    p.fond := creeFacetteFond(l1, l2, e, lonCre);
    p.enLon := creeFacetteEnLong(l1, h, lonCre, coteComplementaire(p.fond.cotes(1)));
    p.enLar := creeFacetteEnLar(
      l2,
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
