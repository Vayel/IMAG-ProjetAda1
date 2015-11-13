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

  -- Création de la boite à partir de la commande
  
  function creePiece(l1, l2, h, e, lonCre: Mesure) return Piece is
    p: Piece;
  begin
    return p;
  end;

  function creeBoite(cmd: Commande) return Boite is
    b: Boite;
  begin
    b.int := creePiece(cmd.lon, cmd.lar, cmd.hInt, cmd.e, cmd.lonCre);
    b.ext := creePiece(cmd.lon, cmd.lar, cmd.hExt, cmd.e, cmd.lonCre);

    return b;
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
end Repr;
