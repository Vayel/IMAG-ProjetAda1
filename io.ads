-- Ce module gère l'interaction avec le monde extérieur. IO signifie 
-- Input/Output.

with Boite; use Boite;


package IO is
  ERREUR_COMMANDE : exception;

  -- Lit l'entrée standard et crée un objet Commande à partir des paramètres
  -- fournis.
  function creeCommande return Commande;

  procedure boiteVersFichier(b: String);
end IO;
