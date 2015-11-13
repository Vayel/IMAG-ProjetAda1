with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line;      use Ada.Command_Line;


package body IO is
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
          param.fname := to_unbounded_string(argument(i));
        else
          completeCommande(param.cmd, argument(i)(2), Mesure'value(argument(i+1)));
        end if;
      end if;
    end loop;

    if commandeIncomplete(param.cmd) or to_string(param.fname) = "" then
      raise ERREUR_COMMANDE with "La commande est incomplète.";
    else 
      if commandeIrrealisable(param.cmd) then
        raise ERREUR_COMMANDE with "Les mesures rendent la commande irréalisable.";
      end if;
    end if;

    return param;
  end recupereParametres;

  -- SVG

  function facetteVersSVG(f: Facette; x: Integer) return String is
  begin
    return "";
  end;

  function pieceVersSVG(p: Piece; x: Integer) return String is
  begin
    return "";
  end;

  function boiteVersSVG(b: Boite) return String is
  begin
    return "";
  end;
end IO;
