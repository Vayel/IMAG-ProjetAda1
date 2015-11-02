with Ada.Text_IO; use Ada.Text_IO;


package body IO is
  fname: String := "";

  procedure completeCommande(cmd: in out Commande; flag: String; val: String) is
    mes: Mesure;
  begin
    mes := Integer'value(val);

    if flag = "-t" then
      cmd.epaisseur := mes;
    else if flag = "-l" then
      cmd.longueur := mes;
    else if flag = "-w" then
      cmd.largeur := mes;
    else if flag = "-q" then
      cmd.longueurQueue := mes;
    else if flag = "-h" then
      cmd.hauteurExt := mes;
    else if flag = "-b" then
      cmd.hauteurInt := mes;
    else
      raise ERREUR_COMMANDE with "Le paramètre " & flag & " n'existe pas.";
    end if;
  end completeCommande;
  
  procedure litCommande(cmd: in out Commande) is
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
  end litCommande;
end IO;
