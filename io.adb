-- Conseil : commencer la lecture par la dernière fonction du fichier et
-- remonter les appels

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line;      use Ada.Command_Line;
with SVG;                   use SVG;
with Repr;                  use Repr;


package body IO is
  -- Un indicateur pour savoir où se situe le dernier point tracé par rapport
  -- au bord intérieur. Par exemple :
  --
  -- ____ : bord intérieur
  --      _
  -- ____|  : bord extérieur
  surBordExt: Boolean;

  procedure completeCommande(cmd: in out Commande; flag: Character; mes: Mesure) is
  begin
    case flag is
      when 't' => cmd.e := mes;
      when 'l' => cmd.lon := mes;
      when 'w' => cmd.lar := mes;
      when 'q' => cmd.lonCre := mes;
      when 'h' => cmd.hExt := mes;
      when 'b' => cmd.hInt := mes;
      when others => raise ERREUR_COMMANDE with "Le paramètre " & flag & " n'existe pas.";
    end case;
  end completeCommande;

  function recupereParametres return Parametres is
    param: Parametres;
  begin
    for i in 1..argument_count loop
      if i mod 2 = 1 then
        if argument(i) = "-f" then
          param.fname := to_unbounded_string(argument(i+1));
        else
          completeCommande(param.cmd, argument(i)(2), Mesure'value(argument(i+1)));
        end if;
      end if;
    end loop;

    if commandeIncomplete(param.cmd) or to_string(param.fname) = "" then
      raise ERREUR_COMMANDE with "La commande est incomplète.";
    elsif commandeIrrealisable(param.cmd) then
      raise ERREUR_COMMANDE with "Les mesures rendent la commande irréalisable.";
    end if;

    return param;
  end recupereParametres;

  -- Utilitaires

  function lonDessinFac(l: Mesure) return Mesure is
  begin
    return l + MARGE;
  end;

  function lonDessinPiece(p: Piece) return Mesure is
  begin
    return 3.0 * lonDessinFac(p.lon) + 2.0 * lonDessinFac(p.lar);
  end;

  function lonDessinBoite(b: Boite) return Mesure is
  begin
    return 2.0 * lonDessinPiece(b.ext) + lonDessinPiece(b.int);
  end;

  -- SVG

  procedure avancer(nCote: Natural; lon: Mesure) is
    dx, dy: Float := 0.0;
  begin
    if nCote = 1 then dx := lon;
    elsif nCote = 2 then dy := lon;
    elsif nCote = 3 then dx := -lon;
    elsif nCote = 4 then dy := -lon;
    end if;

    addRelPolyPoint(dx, dy);
  end;

  procedure sEcarter(nCote: Natural; e: Mesure; rappr: Boolean := false) is
    dx, dy: Float := 0.0;
    fact: Float := 1.0;
  begin
    if rappr then
      fact := -1.0;
    end if;

    if nCote = 1 then dy:= -fact * e;
    elsif nCote = 2 then dx:= fact * e;
    elsif nCote = 3 then dy:= fact * e;
    elsif nCote = 4 then dx:= -fact * e;
    end if;

    addRelPolyPoint(dx, dy);
    surBordExt := not rappr;
  end;
  
  procedure seRapprocher(nCote: Natural; e: Mesure) is
  begin
    sEcarter(nCote, e, true);
  end;

  procedure dessineCoin(n: Natural; e: Mesure; crPrevPlein, crSuivPlein: Boolean) is
    i: Integer;
  begin
    i := indiceCotePrecCoin(n);

    if not crPrevPlein then
      -- Si le créneau précédent n'est pas plein, il faut d'abord s'écarter
      --  __
      -- |__  
      --    |
      --    |
      --
      -- Sinon, on prolonge :
      --  __
      -- |
      -- |
      -- |
      sEcarter(i, e);
    end if;

    avancer(i, e);
    avancer(n, e);

    if not crSuivPlein then
      -- Si le créneau suivant n'est pas plein, il faut se rapprocher :
      --  __
      -- |  |
      -- |
      --
      -- Sinon, on prolongera au moment de dessiner le créneau :
      --  ____
      -- |    
      -- |
      seRapprocher(n, e);
      surBordExt := false;
    else
      surBordExt := true;
    end if;
  end;

  procedure dessineCrePlein(nCote: Natural; lon: Mesure; e: Mesure) is
  begin
    -- On dessine un créneau du genre :
    --
    --     __
    -- ___|
    --
    -- On ne le ferme pas pour éviter les dessins comme :
    --     __ __
    -- ___|  |
    --
    -- A la place, on a :
    --     ____
    -- ___|
    if not surBordExt then
      sEcarter(nCote, e);
    end if;

    avancer(nCote, lon);
  end;

  procedure dessineCreCreux(nCote: Natural; lon: Mesure; e: Mesure) is
    dx, dy: Float := 0.0;
  begin
    if surBordExt then -- Un creux se fait toujours sur le bord intérieur
      seRapprocher(nCote, e);
    end if;

    if nCote = 1 then dx := lon;
    elsif nCote = 2 then dy := lon;
    elsif nCote = 3 then dx := -lon;
    elsif nCote = 4 then dy := -lon;
    end if;

    addRelPolyPoint(dx, dy);
  end;

  procedure dessineCote(n: Natural; c: Creneaux; e: Mesure) is
  begin
    for i in 1..c'last loop
      if c(i).plein then
        dessineCrePlein(n, c(i).lon, e);
      else
        dessineCreCreux(n, c(i).lon, e);
      end if;
    end loop;
  end;

  procedure facetteVersSVG(f: Facette; e, lonCre: Mesure; x0: Float) is
  begin
    startPolygon(EPAISSEUR_TRAIT, COULEUR_TRAIT);
    
    -- Si le créneau précdent le premier coin est plein, tout comme le premier
    -- coin, on part directement de l'extérieur, pour éviter un dessin du
    -- genre :
    --  ____
    -- |__  |__
    -- |
    if crePrecCoin(f, 1, lonCre).plein and f.coins(1) then
      surBordExt := true;
      addPolyPoint(x0, e);
    else
      surBordExt := false;
      addPolyPoint(e + x0, e);
    end if;

    for i in 1..4 loop
      if f.coins(i) then
        dessineCoin(
          i, e, 
          crePrecCoin(f, i, lonCre).plein,
          creSuivCoin(f, i, lonCre).plein
        );
      elsif surBordExt then -- Le créneau précédent était plein, il faut le terminer
        sEcarter(indiceCotePrecCoin(i), e, true);
        surBordExt := false;
      end if;
      
      dessineCote(i, coteVersCreneaux(f.cotes(i), lonCre), e);
    end loop;

    endPolygon;
  end;

  procedure pieceVersSVG(p: Piece; e, lonCre: Mesure; x0: Float) is
    x: Float;
  begin
    x := x0;

    facetteVersSVG(p.fond, e, lonCre, x);
    x := x + lonDessinFac(p.lon);

    for i in 1..2 loop
      facetteVersSVG(p.enLon, e, lonCre, x);
      x := x + lonDessinFac(p.lon);

      facetteVersSVG(p.enLar, e, lonCre, x);
      x := x + lonDessinFac(p.lar);
    end loop;
  end;

  procedure piecesVersSVG(b: Boite) is
  begin
    pieceVersSVG(b.ext, b.e, b.lonCre, 0.0);
    pieceVersSVG(b.ext, b.e, b.lonCre, lonDessinPiece(b.ext));
    pieceVersSVG(b.int, b.e, b.lonCre, 2.0 * lonDessinPiece(b.ext));
  end;

  function boiteVersSVG(b: Boite) return String is
    hMax: Mesure;
  begin
    hMax := Mesure'max(b.ext.lar, b.int.h);

    init;
    header(Integer(lonDessinBoite(b)), Integer(hMax));
    piecesVersSVG(b);
    footer;

    return get_contents;
  end;
end IO;
