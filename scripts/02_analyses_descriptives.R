# Analyse 1 : Statistiques descriptives
cat("\n", rep("=", 60), "\n")
cat("ANALYSE 1 : STATISTIQUES DESCRIPTIVES\n")
cat(rep("=", 60), "\n")

# ============================================================================
# CHARGEMENT DES DONNEES
# ============================================================================

cat("\n1. Chargement des donnees...\n")
donnees <- read.csv("data/processed/donnees_nettoyees.csv")

# Convertir les variables categorielles en facteurs
variables_facteurs <- c("genre", "filiere", "ville_origine", "boursier",
                        "categorie_age", "categorie_heures", "categorie_absences",
                        "reussite_s1", "reussite_s2")
for (var in variables_facteurs) {
  if (var %in% names(donnees)) {
    donnees[[var]] <- as.factor(donnees[[var]])
  }
}

cat("Donnees chargees:", nrow(donnees), "etudiants,", ncol(donnees), "variables\n")

# ============================================================================
# 3.1.1 DEMOGRAPHIE
# ============================================================================

cat("\n", rep("-", 60), "\n")
cat("3.1.1 DEMOGRAPHIE\n")
cat(rep("-", 60), "\n")

# a) Distribution de l'age
cat("\na) Distribution de l'age:\n")
age_stats <- resume_statistique(donnees$age)
print(age_stats)

# b) Repartition par genre
cat("\nb) Repartition par genre:\n")
genre_table <- table(donnees$genre)
genre_pct <- prop.table(genre_table) * 100
genre_df <- data.frame(
  Genre = names(genre_table),
  Effectif = as.numeric(genre_table),
  Pourcentage = round(as.numeric(genre_pct), 1),
  stringsAsFactors = FALSE
)
print(genre_df)

# c) Repartition par filiere
cat("\nc) Repartition par filiere:\n")
filiere_table <- table(donnees$filiere)
filiere_pct <- prop.table(filiere_table) * 100
filiere_df <- data.frame(
  Filiere = names(filiere_table),
  Effectif = as.numeric(filiere_table),
  Pourcentage = round(as.numeric(filiere_pct), 1),
  stringsAsFactors = FALSE
)
print(filiere_df)

# d) Distribution geographique
cat("\nd) Distribution geographique (top 10 villes):\n")
ville_table <- table(donnees$ville_origine)
ville_df <- data.frame(
  Ville = names(ville_table),
  Effectif = as.numeric(ville_table),
  Pourcentage = round(as.numeric(prop.table(ville_table)) * 100, 1),
  stringsAsFactors = FALSE
)
ville_df <- ville_df[order(-ville_df$Effectif), ]
print(head(ville_df, 10))

# e) Repartition par statut boursier
if ("boursier" %in% names(donnees)) {
  cat("\ne) Repartition par statut boursier:\n")
  boursier_table <- table(donnees$boursier)
  boursier_pct <- prop.table(boursier_table) * 100
  boursier_df <- data.frame(
    Statut = names(boursier_table),
    Effectif = as.numeric(boursier_table),
    Pourcentage = round(as.numeric(boursier_pct), 1),
    stringsAsFactors = FALSE
  )
  print(boursier_df)
}

# ============================================================================
# 3.1.2 COMPORTEMENT ACADEMIQUE
# ============================================================================

cat("\n", rep("-", 60), "\n")
cat("3.1.2 COMPORTEMENT ACADEMIQUE\n")
cat(rep("-", 60), "\n")

# a) Statistiques sur les heures d'etude
cat("\na) Heures d'etude par semaine:\n")
heures_stats <- resume_statistique(donnees$heures_etude_semaine)
print(heures_stats)

# b) Statistiques sur les absences
cat("\nb) Absences semestre 1:\n")
abs_s1_stats <- resume_statistique(donnees$nb_absences_s1)
print(abs_s1_stats)

cat("\nAbsences semestre 2:\n")
abs_s2_stats <- resume_statistique(donnees$nb_absences_s2)
print(abs_s2_stats)

cat("\nEvolution des absences S1 -> S2:\n")
abs_variation <- calcul_variation(donnees$nb_absences_s1, donnees$nb_absences_s2)
cat("  Variation moyenne:", round(abs_variation$moyenne_variation, 2), "absences\n")
cat("  Pourcentage avec augmentation:", round(abs_variation$pourcentage_progression, 1), "%\n")
cat("  Pourcentage avec diminution:", round(abs_variation$pourcentage_regression, 1), "%\n")

# c) Relation heures d'etude / absences
cat("\nc) Correlation heures d'etude / absences:\n")
cor_heures_abs_s1 <- cor(donnees$heures_etude_semaine, donnees$nb_absences_s1, use = "complete.obs")
cor_heures_abs_s2 <- cor(donnees$heures_etude_semaine, donnees$nb_absences_s2, use = "complete.obs")

# Interpretation
interpret_correlation <- function(r) {
  abs_r <- abs(r)
  if (abs_r < 0.1) return("negligeable")
  if (abs_r < 0.3) return("faible")
  if (abs_r < 0.5) return("moderee")
  if (abs_r < 0.7) return("forte")
  return("tres forte")
}

cat("  Correlation avec absences S1:", round(cor_heures_abs_s1, 3), 
    "(", interpret_correlation(cor_heures_abs_s1), ")\n")
cat("  Correlation avec absences S2:", round(cor_heures_abs_s2, 3), 
    "(", interpret_correlation(cor_heures_abs_s2), ")\n")

# ============================================================================
# 3.1.3 PERFORMANCE ACADEMIQUE
# ============================================================================

cat("\n", rep("-", 60), "\n")
cat("3.1.3 PERFORMANCE ACADEMIQUE\n")
cat(rep("-", 60), "\n")

# Liste des matieres
matieres <- c("math", "info", "physique", "economie", "anglais")
noms_matieres <- c("Mathematiques", "Informatique", "Physique", "Economie", "Anglais")

# a) Statistiques pour chaque matiere (S1 et S2)
cat("\na) Statistiques par matiere:\n")

for (i in seq_along(matieres)) {
  matiere <- matieres[i]
  nom_matiere <- noms_matieres[i]
  
  cat("\n", rep("*", 40), "\n")
  cat(nom_matiere, "\n")
  cat(rep("*", 40), "\n")
  
  # Variables
  var_s1 <- paste0("note_", matiere, "_s1")
  var_s2 <- paste0("note_", matiere, "_s2")
  
  if (var_s1 %in% names(donnees) && var_s2 %in% names(donnees)) {
    # Statistiques S1
    cat("\nSemestre 1:\n")
    stats_s1 <- resume_statistique(donnees[[var_s1]])
    print(stats_s1)
    
    # Statistiques S2
    cat("\nSemestre 2:\n")
    stats_s2 <- resume_statistique(donnees[[var_s2]])
    print(stats_s2)
    
    # Evolution
    variation_matiere <- calcul_variation(donnees[[var_s1]], donnees[[var_s2]])
    cat("\nEvolution S1 -> S2:\n")
    cat("  Variation moyenne:", round(variation_matiere$moyenne_variation, 2), "points\n")
    cat("  Progression:", round(variation_matiere$pourcentage_progression, 1), "%\n")
    cat("  Regression:", round(variation_matiere$pourcentage_regression, 1), "%\n")
    
    # Taux de reussite
    taux_s1 <- calcul_taux_reussite(donnees[[var_s1]], seuil = 10)
    taux_s2 <- calcul_taux_reussite(donnees[[var_s2]], seuil = 10)
    cat("  Taux reussite S1:", taux_s1, "%\n")
    cat("  Taux reussite S2:", taux_s2, "%\n")
  }
}

# b) Distribution des moyennes generales
cat("\n", rep("*", 60), "\n")
cat("b) Distribution des moyennes generales\n")
cat(rep("*", 60), "\n")

cat("\nMoyenne generale S1:\n")
moyenne_s1_stats <- resume_statistique(donnees$moyenne_s1)
print(moyenne_s1_stats)

cat("\nMoyenne generale S2:\n")
moyenne_s2_stats <- resume_statistique(donnees$moyenne_s2)
print(moyenne_s2_stats)

# Comparaison S1 vs S2
cat("\nComparaison S1 vs S2:\n")
comparaison_moyennes <- data.frame(
  Semestre = c("S1", "S2", "Difference"),
  Moyenne = c(mean(donnees$moyenne_s1, na.rm = TRUE),
              mean(donnees$moyenne_s2, na.rm = TRUE),
              mean(donnees$moyenne_s2, na.rm = TRUE) - mean(donnees$moyenne_s1, na.rm = TRUE)),
  Mediane = c(median(donnees$moyenne_s1, na.rm = TRUE),
              median(donnees$moyenne_s2, na.rm = TRUE),
              median(donnees$moyenne_s2, na.rm = TRUE) - median(donnees$moyenne_s1, na.rm = TRUE))
)
print(comparaison_moyennes)

# c) Taux de reussite global
cat("\n", rep("*", 60), "\n")
cat("c) Taux de reussite global (>=10/20)\n")
cat(rep("*", 60), "\n")

# Utiliser la fonction personnalisee
taux_reussite_s1 <- calcul_taux_reussite(donnees$moyenne_s1, seuil = 10)
taux_reussite_s2 <- calcul_taux_reussite(donnees$moyenne_s2, seuil = 10)

cat("\nTaux de reussite par semestre:\n")
reussite_df <- data.frame(
  Semestre = c("S1", "S2", "Evolution"),
  Taux_reussite = c(taux_reussite_s1, taux_reussite_s2, taux_reussite_s2 - taux_reussite_s1),
  Reussis = c(sum(donnees$moyenne_s1 >= 10, na.rm = TRUE),
              sum(donnees$moyenne_s2 >= 10, na.rm = TRUE),
              NA),
  Echoues = c(sum(donnees$moyenne_s1 < 10, na.rm = TRUE),
              sum(donnees$moyenne_s2 < 10, na.rm = TRUE),
              NA)
)
print(reussite_df)

# d) Identification des matieres les plus difficiles
cat("\n", rep("*", 60), "\n")
cat("d) Classement des matieres par difficulte (S1)\n")
cat(rep("*", 60), "\n")

# Calculer les moyennes par matiere S1
moyennes_matieres <- data.frame(
  Matiere = noms_matieres,
  Code = matieres,
  Moyenne_S1 = sapply(matieres, function(m) {
    var <- paste0("note_", m, "_s1")
    if (var %in% names(donnees)) {
      mean(donnees[[var]], na.rm = TRUE)
    } else NA
  }),
  Taux_reussite_S1 = sapply(matieres, function(m) {
    var <- paste0("note_", m, "_s1")
    if (var %in% names(donnees)) {
      calcul_taux_reussite(donnees[[var]], seuil = 10)
    } else NA
  }),
  Moyenne_S2 = sapply(matieres, function(m) {
    var <- paste0("note_", m, "_s2")
    if (var %in% names(donnees)) {
      mean(donnees[[var]], na.rm = TRUE)
    } else NA
  }),
  Taux_reussite_S2 = sapply(matieres, function(m) {
    var <- paste0("note_", m, "_s2")
    if (var %in% names(donnees)) {
      calcul_taux_reussite(donnees[[var]], seuil = 10)
    } else NA
  }),
  stringsAsFactors = FALSE
)

# Trier par difficulte (moyenne S1 la plus basse)
moyennes_matieres <- moyennes_matieres[order(moyennes_matieres$Moyenne_S1), ]
moyennes_matieres$Rang <- 1:nrow(moyennes_matieres)

# Formater l'affichage
moyennes_matieres$Moyenne_S1 <- round(moyennes_matieres$Moyenne_S1, 2)
moyennes_matieres$Moyenne_S2 <- round(moyennes_matieres$Moyenne_S2, 2)
moyennes_matieres$Taux_reussite_S1 <- round(moyennes_matieres$Taux_reussite_S1, 1)
moyennes_matieres$Taux_reussite_S2 <- round(moyennes_matieres$Taux_reussite_S2, 1)

print(moyennes_matieres[, c("Rang", "Matiere", "Moyenne_S1", "Taux_reussite_S1", 
                            "Moyenne_S2", "Taux_reussite_S2")])

# Identifier la matiere la plus difficile
matiere_plus_difficile <- moyennes_matieres[1, ]
cat("\nMatiere la plus difficile S1:", matiere_plus_difficile$Matiere, 
    "(moyenne:", matiere_plus_difficile$Moyenne_S1, "/20)\n")

# Identifier la matiere la plus facile
matiere_plus_facile <- moyennes_matieres[nrow(moyennes_matieres), ]
cat("Matiere la plus facile S1:", matiere_plus_facile$Matiere, 
    "(moyenne:", matiere_plus_facile$Moyenne_S1, "/20)\n")

# ============================================================================
# EXPORTATION DES RESULTATS
# ============================================================================

cat("\n", rep("=", 60), "\n")
cat("EXPORTATION DES RESULTATS\n")
cat(rep("=", 60), "\n")

# Utiliser les fonctions personnalisees pour exporter
cat("\nExportation des tables...\n")

# Statistiques demographiques
exporter_tableau(age_stats, "stats_age.csv")
exporter_tableau(genre_df, "repartition_genre.csv")
exporter_tableau(filiere_df, "repartition_filiere.csv")
exporter_tableau(ville_df, "repartition_villes.csv")

# Statistiques comportementales
exporter_tableau(heures_stats, "stats_heures_etude.csv")
exporter_tableau(abs_s1_stats, "stats_absences_s1.csv")
exporter_tableau(abs_s2_stats, "stats_absences_s2.csv")

# Statistiques academiques
exporter_tableau(comparaison_moyennes, "comparaison_moyennes_s1_s2.csv")
exporter_tableau(reussite_df, "taux_reussite.csv")
exporter_tableau(moyennes_matieres, "classement_matieres.csv")

# Creer un resume global
resume_global <- data.frame(
  Metrique = c("Nombre_etudiants", "Age_moyen", "Heures_etude_moy", 
               "Absences_moy_S1", "Absences_moy_S2",
               "Moyenne_S1", "Moyenne_S2", 
               "Taux_reussite_S1", "Taux_reussite_S2"),
  Valeur = c(nrow(donnees),
             round(mean(donnees$age, na.rm = TRUE), 1),
             round(mean(donnees$heures_etude_semaine, na.rm = TRUE), 1),
             round(mean(donnees$nb_absences_s1, na.rm = TRUE), 1),
             round(mean(donnees$nb_absences_s2, na.rm = TRUE), 1),
             round(mean(donnees$moyenne_s1, na.rm = TRUE), 2),
             round(mean(donnees$moyenne_s2, na.rm = TRUE), 2),
             taux_reussite_s1,
             taux_reussite_s2)
)

exporter_tableau(resume_global, "resume_global.csv")

