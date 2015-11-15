with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line;      use Ada.Command_Line;
with SVG;                   use SVG;
with Repr;                  use Repr;

with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;


package body IO is
  lonMaxPoly: Mesure; -- Espace réservé pour dessiner une facette
  surBordExt: Boolean := false; -- Si le dernier point du polygone courant est sur le bord extérieur de la facette

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

  -- SVG

  procedure dessineCoin(n: Natural; e: Mesure; crPrevPlein: Boolean) is
  begin
    if n = 1 then
      addRelPolyPoint(0.0, -e);
      addRelPolyPoint(e, 0.0);
    elsif n = 2 then
      addRelPolyPoint(e, 0.0);
      addRelPolyPoint(0.0, e);
    elsif n = 3 then
      addRelPolyPoint(0.0, e);
      addRelPolyPoint(-e, 0.0);
    elsif n = 4 then
      addRelPolyPoint(-e, 0.0);
      addRelPolyPoint(0.0, -e);
    end if;

    surBordExt := true;
  end;

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

  procedure sEcarter(nCote: Natural; e: Mesure; rappr: Boolean) is
    dx, dy: Float := 0.0;
    fact: Float := 1.0;
  begin
    if (rappr and not surBordExt) or (not rappr and surBordExt) then
      raise ERREUR_DESSIN with "Mauvais positionnement par rapport au centre.";
    end if;

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
  
  procedure dessineCrePlein(nCote: Natural; lon: Mesure; e: Mesure) is
  begin
    if not surBordExt then
      sEcarter(nCote, e, false);
    end if;

    avancer(nCote, lon);
  end;

  procedure dessineCreCreux(nCote: Natural; lon: Mesure; e: Mesure) is
    dx, dy: Float := 0.0;
  begin
    if surBordExt then
      sEcarter(nCote, e, true);
    end if;

    if nCote = 1 then dx := lon;
    elsif nCote = 2 then dy := lon;
    elsif nCote = 3 then dx := -lon;
    elsif nCote = 4 then dy := -lon;
    end if;

    addRelPolyPoint(dx, dy);
  end;

  procedure dessineCote(n: Natural; c: Creneaux; cmd: Commande) is
  begin
    for i in 1..c'last loop
      if c(i).plein then
        dessineCrePlein(n, c(i).lon, cmd.e);
      else
        dessineCreCreux(n, c(i).lon, cmd.e);
      end if;
    end loop;
  end;

  procedure facetteVersSVG(f: Facette; cmd: Commande; x0: Float) is
  begin
    startPolygon(EPAISSEUR_TRAIT, COULEUR_TRAIT);

    if f.coins(1) then
      addPolyPoint(x0, cmd.e + Y0);
    else
      addPolyPoint(cmd.e + x0, cmd.e + Y0);
    end if;

    for i in 1..4 loop
      if f.coins(i) then
        dessineCoin(i, cmd.e, crePrecCoinPlein(f, i, cmd.lonCre));
      end if;
      
      dessineCote(i, coteVersCreneaux(f.cotes(i), cmd.lonCre), cmd);
    end loop;

    endPolygon;
  end;

  procedure pieceVersSVG(p: Piece; cmd: Commande; x0: Float) is
    x: Float;
  begin
    x := x0;

    facetteVersSVG(p.fond, cmd, x);

    -- x := x + lonMaxPoly;

    -- for i in 1..2 loop
    --   facetteVersSVG(p.enLon, cmd, x);
    --   facetteVersSVG(p.enLar, cmd, x + lonMaxPoly);

    --   x := x + 2.0 * lonMaxPoly;
    -- end loop;
  end;

  procedure piecesVersSVG(b: Boite; cmd: Commande) is
  begin
    pieceVersSVG(b.ext, cmd, 0.0);
    -- pieceVersSVG(b.ext, cmd, Float(NB_FAC_PAR_PIECE) * lonMaxPoly);
    -- pieceVersSVG(b.int, cmd, Float(NB_FAC_PAR_PIECE) * 2.0 * lonMaxPoly);
  end;

  function boiteVersSVG(b: Boite; cmd: Commande) return String is
  begin
    lonMaxPoly := Mesure'max(cmd.lon, Mesure'max(cmd.hInt,cmd.lar)) + MARGE;

    init;
    header(Integer(lonMaxPoly) * NB_FACETTES, Integer(lonMaxPoly + Y0));
    piecesVersSVG(b, cmd);
    footer;

    return get_contents;
  end;
end IO;
