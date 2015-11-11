with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;


package body IO is
  procedure completeCommande(cmd: in out Commande; flag: Character; mes: Mesure) is
  begin
    case flag is
      when 't' => cmd.epaisseur := mes;
      when 'l' => cmd.longueur := mes;
      when 'w' => cmd.largeur := mes;
      when 'q' => cmd.longueurQueues := mes;
      when 'h' => cmd.hauteurExt := mes;
      when 'b' => cmd.hauteurInt := mes;
      when others => raise ERREUR_COMMANDE with "Le paramètre " & flag & " n'existe pas.";
    end case;
  end completeCommande;

  function recupereParametres return Parametres is
    param: Parametres;
    flag: String(1..2);
    val: String(1..100);
  begin
    for i in 1..7 loop
      get(flag);
      get(val);

      if flag = "-f" then
        param.fname := to_unbounded_string(val);
      else
        completeCommande(param.cmd, flag(1), Mesure'value(val));
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
