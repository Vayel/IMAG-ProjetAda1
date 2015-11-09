with Ada.Text_IO; use Ada.Text_IO;


package body IO is
  procedure completeCommande(cmd: in out Commande; flag: Character; mes: Mesure) is
  begin
    case flag is
      when "t" => cmd.epaisseur := mes;
      when "l" => cmd.longueur := mes;
      when "w" => cmd.largeur := mes;
      when "q" => cmd.longueurQueue := mes;
      when "h" => cmd.hauteurExt := mes;
      when "b" => cmd.hauteurInt := mes;
      when others => raise ERREUR_COMMANDE with "Le paramètre " & flag & " n'existe pas.";
    end case;
  end completeCommande;

  function creeCommande is
    param: Parametres;
    flag: String;
    val: String;
  begin
    for i in 1..7 loop
      get(flag);
      get(val);

      if flag = "-f" then
        param.fname := val;
      else
        completeCommande(param.cmd, flag(1), Integer'value(val));
      end if;
    end loop;

    if commandeIncomplete(param.cmd) or param.fname = "" then
      raise ERREUR_COMMANDE with "La commande est incomplète.";
    else if commandeIrrealisable(param.cmd) then
      raise ERREUR_COMMANDE with "Les mesures rendent la commande irréalisable.";
    end if;

    return param;
  end creeCommande;
end IO;
