with Ada.Text_IO; use Ada.Text_IO;


package body IO is
  fname: String := "";

  procedure completeCommande(cmd: in out Commande; flag: String; val: String) is
    mes: Mesure;
  begin
    mes := Integer'value(val);

    case flag is
      when "-t" => cmd.epaisseur := mes;
      when "-l" => cmd.longueur := mes;
      when "-w" => cmd.largeur := mes;
      when "-q" => cmd.longueurQueue := mes;
      when "-h" => cmd.hauteurExt := mes;
      when "-b" => cmd.hauteurInt := mes;
      when others => raise ERREUR_COMMANDE with "Le paramètre " & flag & " n'existe pas.";
    end case;
  end completeCommande;
  
  function creeCommande() is
    cmd: Commande;
    flag: String;
    val: String;
  begin
    for i in 1..7 loop
      get(flag);
      get(val);

      if flag = "-f" then
        fname := val;
      else
        completeCommande(cmd, flag, val);
      end if;
    end loop;

    if commandeInvalide(cmd) or fname = "" then
      raise ERREUR_COMMANDE with "La commande est incomplète.";
    end if;

    return cmd;
  end creeCommande;

  procedure boiteVersFichier(svg: String) is
    f: File_type;
  begin
    create(f, name => fname);
    put(f, svg);
    close(f);
  end;
end IO;
