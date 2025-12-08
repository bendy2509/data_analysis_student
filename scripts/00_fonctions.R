# 00_fonctions.R - Fonctions personnalisees pour le projet
# Auteur: Bendy SERVILUS
# Date: 07/12/2024

# Fonction 1: Calculer moyenne generale d'un etudiant
# Parametres:
#   - notes_s1: dataframe ou matrice avec notes S1
#   - notes_s2: dataframe ou matrice avec notes S2
# Retourne: liste avec moyennes S1 et S2
calcul_moyenne <- function(notes_s1, notes_s2) {
  # On ignore les valeurs manquantes (na.rm = TRUE)
  moyenne_s1 <- rowMeans(notes_s1, na.rm = TRUE)
  moyenne_s2 <- rowMeans(notes_s2, na.rm = TRUE)
  
  # Remplace NaN par NA si toutes les notes sont NA
  moyenne_s1[is.nan(moyenne_s1)] <- NA
  moyenne_s2[is.nan(moyenne_s2)] <- NA
  
  return(list(moyenne_s1 = moyenne_s1, moyenne_s2 = moyenne_s2))
}

# Fonction 2: Categoriser les etudiants par age
# Parametres:
#   - age: vecteur d'ages
# Retourne: vecteur de categories d'age
categoriser_age <- function(age) {
  # Verification des valeurs
  if (any(age < 18 | age > 26, na.rm = TRUE)) {
    warning("Certains ages sont hors de l'intervalle [18, 26]")
  }
  
  # Creation des categories
  categorie <- cut(
    age,
    breaks = c(18, 21, 23, 27),
    # 27 pour inclure 26
    labels = c("18-20", "21-22", "23-26"),
    right = FALSE,
    # Intervalles [a,b)
    include.lowest = FALSE
  )
  
  return(categorie)
}

# Fonction 3: Exporter un tableau en CSV
# Parametres:
#   - tableau: dataframe a exporter
#   - nom_fichier: nom du fichier CSV
#   - dossier: dossier de destination (par defaut: "output/tables")
exporter_tableau <-
  function(tableau, nom_fichier, dossier = "output/tables") {
    # Verifier si le dossier existe
    if (!dir.exists(dossier)) {
      dir.create(dossier, recursive = TRUE)
    }
    
    # Creer le chemin complet
    chemin <- file.path(dossier, nom_fichier)
    
    # Exporter
    write.csv(tableau, chemin, row.names = FALSE)
    cat(
      "[EXPORT] Tableau exporte:",
      chemin,
      sprintf("(%d lignes x %d colonnes)\n",
              nrow(tableau), ncol(tableau))
    )
    
    return(chemin)
  }

# Fonction 4: Creer un graphique et l'exporter
# Parametres:
#   - graphique: objet ggplot
#   - nom_fichier: nom du fichier PNG
#   - largeur: largeur en pouces (defaut: 10)
#   - hauteur: hauteur en pouces (defaut: 6)
#   - dossier: dossier de destination (defaut: "output/figures")
exporter_graphique <- function(graphique,
                               nom_fichier,
                               largeur = 10,
                               hauteur = 6,
                               dossier = "output/figures") {
  # Verifier si ggplot2 est charge
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package ggplot2 requis pour exporter les graphiques")
  }
  
  # Verifier si le dossier existe
  if (!dir.exists(dossier)) {
    dir.create(dossier, recursive = TRUE)
  }
  
  # Creer le chemin complet
  chemin <- file.path(dossier, nom_fichier)
  
  # Exporter avec haute resolution
  ggplot2::ggsave(
    chemin,
    graphique,
    width = largeur,
    height = hauteur,
    dpi = 300,
    bg = "white"
  )
  
  cat(
    "[EXPORT] Graphique exporte:",
    chemin,
    sprintf("(%dx%d pixels, 300 DPI)\n",
            largeur * 300, hauteur * 300)
  )
  
  return(chemin)
}

# Fonction 5: Calculer taux de reussite
# Parametres:
#   - notes: vecteur de notes
#   - seuil: seuil de reussite (defaut: 10)
#   - pourcentage: TRUE pour retourner en %, FALSE pour proportion
calcul_taux_reussite <-
  function(notes,
           seuil = 10,
           pourcentage = TRUE) {
    # Calculer la proportion de reussite
    prop_reussite <- mean(notes >= seuil, na.rm = TRUE)
    
    # Convertir en pourcentage si demande
    if (pourcentage) {
      resultat <- round(prop_reussite * 100, 1)
    } else {
      resultat <- round(prop_reussite, 3)
    }
    
    return(resultat)
  }

# Fonction 6: Generer un resume statistique rapide
# Parametres:
#   - vecteur: vecteur numerique
# Retourne: dataframe avec statistiques
resume_statistique <- function(vecteur) {
  stats_df <- data.frame(
    n = sum(!is.na(vecteur)),
    n_manquants = sum(is.na(vecteur)),
    moyenne = mean(vecteur, na.rm = TRUE),
    mediane = median(vecteur, na.rm = TRUE),
    ecart_type = sd(vecteur, na.rm = TRUE),
    minimum = min(vecteur, na.rm = TRUE),
    maximum = max(vecteur, na.rm = TRUE),
    q1 = quantile(vecteur, 0.25, na.rm = TRUE),
    q3 = quantile(vecteur, 0.75, na.rm = TRUE)
  )
  
  # Arrondir les valeurs
  stats_df[, 3:ncol(stats_df)] <-
    round(stats_df[, 3:ncol(stats_df)], 2)
  
  return(stats_df)
}

# Fonction 7: Verifier et nettoyer les notes
# Parametres:
#   - notes: vecteur de notes
#   - min_val: valeur minimale autorisee (defaut: 0)
#   - max_val: valeur maximale autorisee (defaut: 20)
nettoyer_notes <- function(notes,
                           min_val = 0,
                           max_val = 20) {
  # Identifier les valeurs hors intervalle
  hors_intervalle <- notes < min_val | notes > max_val
  
  if (any(hors_intervalle, na.rm = TRUE)) {
    nb_problemes <- sum(hors_intervalle, na.rm = TRUE)
    warning(sprintf(
      "%d notes hors de l'intervalle [%d, %d]",
      nb_problemes,
      min_val,
      max_val
    ))
    
    # Remplacer par NA
    notes[hors_intervalle] <- NA
  }
  
  # Identifier les valeurs manquantes
  nb_manquants <- sum(is.na(notes))
  if (nb_manquants > 0) {
    cat(sprintf("Note: %d valeurs manquantes dans les notes\n", nb_manquants))
  }
  
  return(notes)
}

# Fonction 8: Calculer la variation S1 -> S2
# Parametres:
#   - notes_s1: notes semestre 1
#   - notes_s2: notes semestre 2
calcul_variation <- function(notes_s1, notes_s2) {
  # Verifier que les vecteurs ont la meme longueur
  if (length(notes_s1) != length(notes_s2)) {
    stop("Les vecteurs de notes doivent avoir la meme longueur")
  }
  
  # Calculer la variation
  variation <- notes_s2 - notes_s1
  
  # Statistiques sur la variation
  stats_variation <- list(
    variation = variation,
    moyenne_variation = mean(variation, na.rm = TRUE),
    mediane_variation = median(variation, na.rm = TRUE),
    sd_variation = sd(variation, na.rm = TRUE),
    pourcentage_progression = mean(variation > 0, na.rm = TRUE) * 100,
    pourcentage_regression = mean(variation < 0, na.rm = TRUE) * 100
  )
  
  return(stats_variation)
}
