# Analyses 2, 3 et 4 : Evolution, Groupes, Facteurs

cat("\n", rep("=", 70), "\n")
cat("ANALYSES COMPARATIVES - EVOLUTION, GROUPES ET FACTEURS DE REUSSITE\n")
cat(rep("=", 70), "\n")

# ============================================================================
# INITIALISATION
# ============================================================================

cat("\n1. Chargement des donnees nettoyees...\n")
donnees <- read.csv("data/processed/donnees_completes.csv")

# Convertir les variables categorielles en facteurs
variables_facteurs <- c("genre", "filiere", "ville_origine", "boursier",
                        "categorie_age", "categorie_heures", "categorie_absences",
                        "reussite_s1", "reussite_s2", "evolution", "groupe_absences")
for (var in variables_facteurs) {
  if (var %in% names(donnees)) {
    donnees[[var]] <- as.factor(donnees[[var]])
  }
}

cat("OK Donnees chargees:", nrow(donnees), "etudiants,", ncol(donnees), "variables\n")

# ============================================================================
# ANALYSE 2 : EVOLUTION S1 -> S2 (20 points)
# ============================================================================

cat("\n", rep("=", 70), "\n")
cat("ANALYSE 2 : EVOLUTION DES PERFORMANCES S1 -> S2\n")
cat(rep("=", 70), "\n")

# 3.2.1 Evolution Globale (10 points)
cat("\n", rep("-", 60), "\n")
cat("3.2.1 EVOLUTION GLOBALE\n")
cat(rep("-", 60), "\n")

# a) Comparaison des moyennes S1 vs S2
cat("\na) COMPARAISON DES MOYENNES S1 vs S2:\n")

comparaison_moyennes <- data.frame(
  Semestre = c("S1", "S2", "Variation"),
  Moyenne = c(mean(donnees$moyenne_s1, na.rm = TRUE),
              mean(donnees$moyenne_s2, na.rm = TRUE),
              mean(donnees$variation_moyenne, na.rm = TRUE)),
  Mediane = c(median(donnees$moyenne_s1, na.rm = TRUE),
              median(donnees$moyenne_s2, na.rm = TRUE),
              median(donnees$variation_moyenne, na.rm = TRUE)),
  SD = c(sd(donnees$moyenne_s1, na.rm = TRUE),
         sd(donnees$moyenne_s2, na.rm = TRUE),
         sd(donnees$variation_moyenne, na.rm = TRUE))
)
print(comparaison_moyennes)

# Test t apparie pour comparer S1 et S2
cat("\nTEST T APPARIE POUR MOYENNES S1 vs S2:\n")
test_t <- t.test(donnees$moyenne_s1, donnees$moyenne_s2, paired = TRUE)
cat("  t =", round(test_t$statistic, 3), "\n")
cat("  degres de liberte =", test_t$parameter, "\n")
cat("  p-value =", format.pval(test_t$p.value, digits = 3), "\n")
cat("  intervalle de confiance 95%: [", 
    round(test_t$conf.int[1], 3), ", ", 
    round(test_t$conf.int[2], 3), "]\n", sep = "")

if (test_t$p.value < 0.05) {
  cat("  CONCLUSION: Difference SIGNIFICATIVE au seuil de 5%\n")
  cat("              Les moyennes S1 et S2 sont statistiquement differentes\n")
} else {
  cat("  CONCLUSION: Difference NON SIGNIFICATIVE au seuil de 5%\n")
  cat("              Pas de difference statistique entre S1 et S2\n")
}

# b) Distribution des variations de notes
cat("\nb) DISTRIBUTION DES VARIATIONS DE NOTES:\n")
variation_stats <- resume_statistique(donnees$variation_moyenne)
print(variation_stats)

# Calculer les proportions de progression/regression
progression_pct <- calcul_taux_reussite(donnees$variation_moyenne, seuil = 0, pourcentage = TRUE)
regression_pct <- 100 - progression_pct

cat("  Pourcentage de progression (variation > 0):", progression_pct, "%\n")
cat("  Pourcentage de regression (variation < 0):", regression_pct, "%\n")

# c) Identification des progressions et regressions
cat("\nc) IDENTIFICATION DES PROGRESSIONS ET REGRESSIONS:\n")

# Definir les categories d'evolution
donnees$categorie_evolution <- cut(donnees$variation_moyenne,
                                   breaks = c(-Inf, -2, -0.5, 0.5, 2, Inf),
                                   labels = c("Forte regression", "Legere regression", 
                                              "Stable", "Legere progression", "Forte progression"),
                                   include.lowest = TRUE)

evolution_table <- table(donnees$categorie_evolution)
evolution_df <- data.frame(
  Categorie = names(evolution_table),
  Effectif = as.numeric(evolution_table),
  Pourcentage = round(as.numeric(prop.table(evolution_table)) * 100, 1),
  stringsAsFactors = FALSE
)
print(evolution_df)

# d) Top 10 des progressions
cat("\nd) TOP 10 DES PROGRESSIONS LES PLUS FORTES:\n")
top_progression <- donnees[order(-donnees$variation_moyenne), 
                           c("id_etudiant", "nom", "filiere", 
                             "moyenne_s1", "moyenne_s2", "variation_moyenne")]
top_progression$variation_moyenne <- round(top_progression$variation_moyenne, 2)
print(head(top_progression, 10))

# e) Top 10 des regressions
cat("\ne) TOP 10 DES REGRESSIONS LES PLUS FORTES:\n")
top_regression <- donnees[order(donnees$variation_moyenne), 
                          c("id_etudiant", "nom", "filiere",
                            "moyenne_s1", "moyenne_s2", "variation_moyenne")]
top_regression$variation_moyenne <- round(top_regression$variation_moyenne, 2)
print(head(top_regression, 10))

# 3.2.2 Evolution par Matiere (10 points)
cat("\n", rep("-", 60), "\n")
cat("3.2.2 EVOLUTION PAR MATIERE\n")
cat(rep("-", 60), "\n")

# Liste des matieres
matieres <- c("math", "info", "physique", "economie", "anglais")
noms_matieres <- c("Mathematiques", "Informatique", "Physique", "Economie", "Anglais")

# a) Comparaison S1 vs S2 pour chaque matiere
cat("\na) COMPARAISON S1 vs S2 PAR MATIERE:\n")
evolution_matiere <- data.frame()

for (i in seq_along(matieres)) {
  matiere <- matieres[i]
  nom_matiere <- noms_matieres[i]
  
  var_s1 <- paste0("note_", matiere, "_s1")
  var_s2 <- paste0("note_", matiere, "_s2")
  
  if (var_s1 %in% names(donnees) & var_s2 %in% names(donnees)) {
    # Statistiques S1
    stats_s1 <- resume_statistique(donnees[[var_s1]])
    moyenne_s1 <- mean(donnees[[var_s1]], na.rm = TRUE)
    
    # Statistiques S2
    stats_s2 <- resume_statistique(donnees[[var_s2]])
    moyenne_s2 <- mean(donnees[[var_s2]], na.rm = TRUE)
    
    # Variation
    variation <- moyenne_s2 - moyenne_s1
    
    # Test t pour la matiere
    test_t_matiere <- t.test(donnees[[var_s1]], donnees[[var_s2]], paired = TRUE)
    
    # Taux de reussite
    taux_s1 <- calcul_taux_reussite(donnees[[var_s1]], seuil = 10)
    taux_s2 <- calcul_taux_reussite(donnees[[var_s2]], seuil = 10)
    
    evolution_matiere <- rbind(evolution_matiere, data.frame(
      Matiere = nom_matiere,
      Code = toupper(matiere),
      Moyenne_S1 = round(moyenne_s1, 2),
      Moyenne_S2 = round(moyenne_s2, 2),
      Variation = round(variation, 2),
      Taux_reussite_S1 = taux_s1,
      Taux_reussite_S2 = taux_s2,
      p_value = round(test_t_matiere$p.value, 4),
      Significatif = ifelse(test_t_matiere$p.value < 0.05, "Oui", "Non")
    ))
  }
}

print(evolution_matiere, row.names = FALSE)

# b) Matieres en progression / regression
cat("\nb) CLASSEMENT DES MATIERES PAR PROGRESSION:\n")
evolution_ordonnee <- evolution_matiere[order(-evolution_matiere$Variation), 
                                        c("Matiere", "Moyenne_S1", "Moyenne_S2", 
                                          "Variation", "Significatif")]
print(evolution_ordonnee, row.names = FALSE)

cat("\nRESUME DE L'EVOLUTION PAR MATIERE:\n")
cat("- Matiere avec plus forte progression:", 
    evolution_ordonnee$Matiere[1], 
    "(+", evolution_ordonnee$Variation[1], "points)\n")
cat("- Matiere avec plus forte regression:", 
    evolution_ordonnee$Matiere[nrow(evolution_ordonnee)], 
    "(", evolution_ordonnee$Variation[nrow(evolution_ordonnee)], "points)\n")
cat("- Nombre de matieres avec progression significative:", 
    sum(evolution_matiere$Significatif == "Oui" & evolution_matiere$Variation > 0), "\n")

# ============================================================================
# ANALYSE 3 : PERFORMANCE PAR GROUPES (25 points)
# ============================================================================

cat("\n", rep("=", 70), "\n")
cat("ANALYSE 3 : PERFORMANCE PAR GROUPES DEMOGRAPHIQUES\n")
cat(rep("=", 70), "\n")

# 3.3.1 Par Filiere (10 points)
cat("\n", rep("-", 60), "\n")
cat("3.3.1 PERFORMANCE PAR FILIERE\n")
cat(rep("-", 60), "\n")

# a) Statistiques descriptives par filiere
cat("\na) STATISTIQUES PAR FILIERE:\n")

performance_filiere <- donnees %>%
  group_by(filiere) %>%
  summarise(
    Effectif = n(),
    Pourcentage = round(n()/nrow(donnees)*100, 1),
    Moyenne_S1 = round(mean(moyenne_s1, na.rm = TRUE), 2),
    Moyenne_S2 = round(mean(moyenne_s2, na.rm = TRUE), 2),
    Variation = round(Moyenne_S2 - Moyenne_S1, 2),
    Taux_reussite_S1 = calcul_taux_reussite(moyenne_s1, seuil = 10),
    Taux_reussite_S2 = calcul_taux_reussite(moyenne_s2, seuil = 10),
    Heures_etude_moy = round(mean(heures_etude_semaine, na.rm = TRUE), 1),
    Absences_moy_S1 = round(mean(nb_absences_s1, na.rm = TRUE), 1),
    Absences_moy_S2 = round(mean(nb_absences_s2, na.rm = TRUE), 1)
  ) %>%
  arrange(desc(Moyenne_S2))

print(as.data.frame(performance_filiere), row.names = FALSE)

# b) Comparaison des moyennes entre filieres
cat("\nb) COMPARAISON ENTRE FILIERES:\n")

# Test ANOVA pour comparer les filieres
cat("   Test ANOVA pour differences entre filieres (S2):\n")
anova_filiere <- aov(moyenne_s2 ~ filiere, data = donnees)
anova_summary <- summary(anova_filiere)
print(anova_summary)

if (anova_summary[[1]][["Pr(>F)"]][1] < 0.05) {
  cat("   CONCLUSION: Difference SIGNIFICATIVE entre les filieres au seuil de 5%\n")
  
  # Test post-hoc de Tukey
  cat("   \n   Comparaisons multiples (test de Tukey HSD):\n")
  tukey_result <- TukeyHSD(anova_filiere)
  print(tukey_result$filiere)
} else {
  cat("   CONCLUSION: Pas de difference SIGNIFICATIVE entre les filieres au seuil de 5%\n")
}

# Classement des filieres
cat("\nc) CLASSEMENT DES FILIERES PAR PERFORMANCE S2:\n")
classement_filiere <- performance_filiere[, c("filiere", "Moyenne_S2", "Taux_reussite_S2", "Effectif")]
classement_filiere$Rang <- rank(-classement_filiere$Moyenne_S2, ties.method = "min")
print(classement_filiere[order(classement_filiere$Rang), ], row.names = FALSE)

# 3.3.2 Par Genre (8 points)
cat("\n", rep("-", 60), "\n")
cat("3.3.2 PERFORMANCE PAR GENRE\n")
cat(rep("-", 60), "\n")

# a) Comparaison des performances H/F
cat("\na) COMPARAISON HOMMES vs FEMMES:\n")

performance_genre <- donnees %>%
  group_by(genre) %>%
  summarise(
    Effectif = n(),
    Pourcentage = round(n()/nrow(donnees)*100, 1),
    Moyenne_S1 = round(mean(moyenne_s1, na.rm = TRUE), 2),
    Moyenne_S2 = round(mean(moyenne_s2, na.rm = TRUE), 2),
    Variation = round(Moyenne_S2 - Moyenne_S1, 2),
    Taux_reussite_S1 = calcul_taux_reussite(moyenne_s1, seuil = 10),
    Taux_reussite_S2 = calcul_taux_reussite(moyenne_s2, seuil = 10),
    Heures_etude_moy = round(mean(heures_etude_semaine, na.rm = TRUE), 1),
    Absences_moy = round(mean(nb_absences_s1 + nb_absences_s2, na.rm = TRUE)/2, 1)
  )

print(as.data.frame(performance_genre), row.names = FALSE)

# Test t pour comparer les genres
cat("\nb) TEST STATISTIQUE POUR DIFFERENCE ENTRE GENRES:\n")
hommes_s2 <- donnees$moyenne_s2[donnees$genre == "Homme"]
femmes_s2 <- donnees$moyenne_s2[donnees$genre == "Femme"]

test_t_genre <- t.test(hommes_s2, femmes_s2, var.equal = TRUE)
cat("   Test t pour moyenne S2:\n")
cat("   t =", round(test_t_genre$statistic, 3), "\n")
cat("   p-value =", format.pval(test_t_genre$p.value, digits = 3), "\n")

if (test_t_genre$p.value < 0.05) {
  cat("   CONCLUSION: Difference SIGNIFICATIVE entre hommes et femmes au seuil de 5%\n")
  difference <- mean(hommes_s2, na.rm = TRUE) - mean(femmes_s2, na.rm = TRUE)
  cat("               Difference de moyenne:", round(difference, 2), "points\n")
} else {
  cat("   CONCLUSION: Pas de difference SIGNIFICATIVE entre hommes et femmes au seuil de 5%\n")
}

# 3.3.3 Par Categorie d'Age (7 points)
cat("\n", rep("-", 60), "\n")
cat("3.3.3 PERFORMANCE PAR CATEGORIE D'AGE\n")
cat(rep("-", 60), "\n")

# a) Performance par categorie d'age
cat("\na) PERFORMANCE PAR CATEGORIE D'AGE:\n")

performance_age <- donnees %>%
  group_by(categorie_age) %>%
  summarise(
    Effectif = n(),
    Pourcentage = round(n()/nrow(donnees)*100, 1),
    Moyenne_S1 = round(mean(moyenne_s1, na.rm = TRUE), 2),
    Moyenne_S2 = round(mean(moyenne_s2, na.rm = TRUE), 2),
    Variation = round(Moyenne_S2 - Moyenne_S1, 2),
    Taux_reussite_S1 = calcul_taux_reussite(moyenne_s1, seuil = 10),
    Taux_reussite_S2 = calcul_taux_reussite(moyenne_s2, seuil = 10),
    Heures_etude_moy = round(mean(heures_etude_semaine, na.rm = TRUE), 1),
    Absences_moy = round(mean(nb_absences_s1 + nb_absences_s2, na.rm = TRUE)/2, 1)
  ) %>%
  arrange(categorie_age)

print(as.data.frame(performance_age), row.names = FALSE)

# Test ANOVA pour categories d'age
cat("\nb) TEST ANOVA POUR DIFFERENCES ENTRE CATEGORIES D'AGE:\n")
anova_age <- aov(moyenne_s2 ~ categorie_age, data = donnees)
anova_age_summary <- summary(anova_age)
print(anova_age_summary)

if (anova_age_summary[[1]][["Pr(>F)"]][1] < 0.05) {
  cat("   CONCLUSION: Difference SIGNIFICATIVE entre categories d'age au seuil de 5%\n")
} else {
  cat("   CONCLUSION: Pas de difference SIGNIFICATIVE entre categories d'age au seuil de 5%\n")
}

# ============================================================================
# ANALYSE 4 : FACTEURS DE REUSSITE (15 points)
# ============================================================================

cat("\n", rep("=", 70), "\n")
cat("ANALYSE 4 : FACTEURS ASSOCIES A LA REUSSITE ACADEMIQUE\n")
cat(rep("=", 70), "\n")

# 3.4.1 Impact des Heures d'Etude (8 points)
cat("\n", rep("-", 60), "\n")
cat("3.4.1 IMPACT DES HEURES D'ETUDE\n")
cat(rep("-", 60), "\n")

# a) Correlation heures d'etude / performance
cat("\na) CORRELATION HEURES D'ETUDE / PERFORMANCE:\n")

cor_heures_s1 <- cor(donnees$heures_etude_semaine, donnees$moyenne_s1, use = "complete.obs")
cor_heures_s2 <- cor(donnees$heures_etude_semaine, donnees$moyenne_s2, use = "complete.obs")

cat("   Correlation avec moyenne S1:", round(cor_heures_s1, 3), "\n")
cat("   Correlation avec moyenne S2:", round(cor_heures_s2, 3), "\n")

# Interpretation de la correlation
interpret_correlation <- function(r) {
  abs_r <- abs(r)
  if (abs_r < 0.1) return("negligeable")
  if (abs_r < 0.3) return("faible")
  if (abs_r < 0.5) return("moderee")
  if (abs_r < 0.7) return("forte")
  return("tres forte")
}

cat("   Interpretation S1:", interpret_correlation(cor_heures_s1), "\n")
cat("   Interpretation S2:", interpret_correlation(cor_heures_s2), "\n")

# Test de significativite
test_cor_s1 <- cor.test(donnees$heures_etude_semaine, donnees$moyenne_s1)
test_cor_s2 <- cor.test(donnees$heures_etude_semaine, donnees$moyenne_s2)

cat("   Significativite S1: p =", format.pval(test_cor_s1$p.value, digits = 3), 
    ifelse(test_cor_s1$p.value < 0.05, "(significatif)", "(non significatif)"), "\n")
cat("   Significativite S2: p =", format.pval(test_cor_s2$p.value, digits = 3), 
    ifelse(test_cor_s2$p.value < 0.05, "(significatif)", "(non significatif)"), "\n")

# b) Analyse par tranche d'heures d'etude
cat("\nb) ANALYSE PAR TRANCHE D'HEURES D'ETUDE:\n")

if (!"tranche_heures" %in% names(donnees)) {
  donnees$tranche_heures <- cut(donnees$heures_etude_semaine,
                                breaks = c(0, 10, 15, 20, 25, 30, 40),
                                labels = c("0-10h", "11-15h", "16-20h", 
                                           "21-25h", "26-30h", "31-40h"),
                                include.lowest = TRUE)
}

performance_heures <- donnees %>%
  group_by(tranche_heures) %>%
  summarise(
    Effectif = n(),
    Pourcentage = round(n()/nrow(donnees)*100, 1),
    Moyenne_S1 = round(mean(moyenne_s1, na.rm = TRUE), 2),
    Moyenne_S2 = round(mean(moyenne_s2, na.rm = TRUE), 2),
    Taux_reussite_S1 = calcul_taux_reussite(moyenne_s1, seuil = 10),
    Taux_reussite_S2 = calcul_taux_reussite(moyenne_s2, seuil = 10),
    Heures_moy = round(mean(heures_etude_semaine, na.rm = TRUE), 1),
    Absences_moy = round(mean(nb_absences_s1 + nb_absences_s2, na.rm = TRUE)/2, 1)
  ) %>%
  arrange(tranche_heures)

print(as.data.frame(performance_heures), row.names = FALSE)

# c) Identification d'un seuil optimal
cat("\nc) IDENTIFICATION D'UN SEUIL OPTIMAL D'HEURES D'ETUDE:\n")

# Chercher la tranche avec la meilleure performance
meilleure_tranche <- performance_heures[which.max(performance_heures$Moyenne_S2), ]
cat("   Tranche avec meilleure performance S2:", as.character(meilleure_tranche$tranche_heures), "\n")
cat("   Moyenne S2:", meilleure_tranche$Moyenne_S2, "/20\n")
cat("   Taux reussite S2:", meilleure_tranche$Taux_reussite_S2, "%\n")
cat("   Heures moyennes dans cette tranche:", meilleure_tranche$Heures_moy, "h/semaine\n")

# Analyser le retour sur investissement (gain par heure)
cat("\n   ANALYSE DU RETOUR SUR INVESTISSEMENT (gain par heure):\n")
for (i in 1:nrow(performance_heures)) {
  tranche <- performance_heures[i, ]
  heures_mid <- mean(as.numeric(unlist(strsplit(gsub("h", "", as.character(tranche$tranche_heures)), "-"))))
  gain_par_heure <- tranche$Moyenne_S2 / heures_mid
  cat("   ", as.character(tranche$tranche_heures), ": ", round(gain_par_heure, 3), 
      " points/heure\n", sep = "")
}

# 3.4.2 Impact des Absences (7 points)
cat("\n", rep("-", 60), "\n")
cat("3.4.2 IMPACT DES ABSENCES\n")
cat(rep("-", 60), "\n")

# a) Correlation absences / performance
cat("\na) CORRELATION ABSENCES / PERFORMANCE:\n")

cor_abs_s1 <- cor(donnees$nb_absences_s1, donnees$moyenne_s1, use = "complete.obs")
cor_abs_s2 <- cor(donnees$nb_absences_s2, donnees$moyenne_s2, use = "complete.obs")
cor_abs_total <- cor(donnees$nb_absences_s1 + donnees$nb_absences_s2, 
                     donnees$moyenne_s1 + donnees$moyenne_s2, use = "complete.obs")

cat("   Correlation absences S1 / moyenne S1:", round(cor_abs_s1, 3), "\n")
cat("   Correlation absences S2 / moyenne S2:", round(cor_abs_s2, 3), "\n")
cat("   Correlation absences totales / moyenne totale:", round(cor_abs_total, 3), "\n")

# Test de significativite
test_abs_s1 <- cor.test(donnees$nb_absences_s1, donnees$moyenne_s1)
test_abs_s2 <- cor.test(donnees$nb_absences_s2, donnees$moyenne_s2)

cat("   Significativite S1: p =", format.pval(test_abs_s1$p.value, digits = 3), 
    ifelse(test_abs_s1$p.value < 0.05, "(significatif)", "(non significatif)"), "\n")
cat("   Significativite S2: p =", format.pval(test_abs_s2$p.value, digits = 3), 
    ifelse(test_abs_s2$p.value < 0.05, "(significatif)", "(non significatif)"), "\n")

# b) Comparaison faibles vs fortes absences
cat("\nb) COMPARAISON FAIBLES vs FORTES ABSENCES:\n")

if (!"groupe_absences" %in% names(donnees)) {
  seuil_absences <- median(donnees$nb_absences_s1 + donnees$nb_absences_s2, na.rm = TRUE)
  donnees$groupe_absences <- ifelse((donnees$nb_absences_s1 + donnees$nb_absences_s2) <= seuil_absences,
                                    "Faibles absences", "Fortes absences")
}

comparaison_absences <- donnees %>%
  group_by(groupe_absences) %>%
  summarise(
    Effectif = n(),
    Pourcentage = round(n()/nrow(donnees)*100, 1),
    Moyenne_S1 = round(mean(moyenne_s1, na.rm = TRUE), 2),
    Moyenne_S2 = round(mean(moyenne_s2, na.rm = TRUE), 2),
    Taux_reussite_S1 = calcul_taux_reussite(moyenne_s1, seuil = 10),
    Taux_reussite_S2 = calcul_taux_reussite(moyenne_s2, seuil = 10),
    Heures_etude_moy = round(mean(heures_etude_semaine, na.rm = TRUE), 1),
    Absences_moy_S1 = round(mean(nb_absences_s1, na.rm = TRUE), 1),
    Absences_moy_S2 = round(mean(nb_absences_s2, na.rm = TRUE), 1)
  )

print(as.data.frame(comparaison_absences), row.names = FALSE)

# Test t pour comparer les groupes d'absences
cat("\nc) TEST STATISTIQUE POUR DIFFERENCE ENTRE GROUPES D'ABSENCES:\n")
faibles <- donnees$moyenne_s2[donnees$groupe_absences == "Faibles absences"]
fortes <- donnees$moyenne_s2[donnees$groupe_absences == "Fortes absences"]

test_t_abs <- t.test(faibles, fortes, var.equal = TRUE)
cat("   Test t pour moyenne S2:\n")
cat("   t =", round(test_t_abs$statistic, 3), "\n")
cat("   p-value =", format.pval(test_t_abs$p.value, digits = 3), "\n")
cat("   Difference de moyenne:", round(mean(faibles, na.rm = TRUE) - mean(fortes, na.rm = TRUE), 2), "points\n")

if (test_t_abs$p.value < 0.05) {
  cat("   CONCLUSION: Difference SIGNIFICATIVE entre les groupes au seuil de 5%\n")
  cat("               Les etudiants avec moins d'absences ont de meilleurs resultats\n")
} else {
  cat("   CONCLUSION: Pas de difference SIGNIFICATIVE entre les groupes au seuil de 5%\n")
}

# c) Evolution des absences S1 -> S2
cat("\nd) EVOLUTION DES ABSENCES S1 -> S2:\n")

evolution_absences <- data.frame(
  Semestre = c("S1", "S2", "Variation"),
  Moyenne = c(mean(donnees$nb_absences_s1, na.rm = TRUE),
              mean(donnees$nb_absences_s2, na.rm = TRUE),
              mean(donnees$nb_absences_s2, na.rm = TRUE) - mean(donnees$nb_absences_s1, na.rm = TRUE)),
  Mediane = c(median(donnees$nb_absences_s1, na.rm = TRUE),
              median(donnees$nb_absences_s2, na.rm = TRUE),
              median(donnees$nb_absences_s2, na.rm = TRUE) - median(donnees$nb_absences_s1, na.rm = TRUE)),
  SD = c(sd(donnees$nb_absences_s1, na.rm = TRUE),
         sd(donnees$nb_absences_s2, na.rm = TRUE),
         sd(donnees$nb_absences_s2 - donnees$nb_absences_s1, na.rm = TRUE))
)
print(evolution_absences)

# Test t pour evolution des absences
test_abs_evolution <- t.test(donnees$nb_absences_s1, donnees$nb_absences_s2, paired = TRUE)
cat("\n   Test t apparie pour evolution des absences:\n")
cat("   t =", round(test_abs_evolution$statistic, 3), "\n")
cat("   p-value =", format.pval(test_abs_evolution$p.value, digits = 3), "\n")

if (test_abs_evolution$p.value < 0.05) {
  cat("   CONCLUSION: Evolution SIGNIFICATIVE des absences au seuil de 5%\n")
  if (evolution_absences$Moyenne[3] > 0) {
    cat("               Augmentation significative des absences S1 -> S2\n")
  } else {
    cat("               Diminution significative des absences S1 -> S2\n")
  }
} else {
  cat("   CONCLUSION: Pas d'evolution SIGNIFICATIVE des absences au seuil de 5%\n")
}

# ============================================================================
# EXPORTATION DES RESULTATS
# ============================================================================

cat("\n", rep("=", 70), "\n")
cat("EXPORTATION DES RESULTATS DES ANALYSES COMPARATIVES\n")
cat(rep("=", 70), "\n")

cat("\nExportation des tables...\n")

# Utiliser la fonction personnalisee pour exporter
exporter_tableau(comparaison_moyennes, "comparaison_s1_s2.csv")
exporter_tableau(variation_stats, "stats_variation_moyennes.csv")
exporter_tableau(evolution_df, "categorie_evolution.csv")
exporter_tableau(head(top_progression, 10), "top10_progression.csv")
exporter_tableau(head(top_regression, 10), "top10_regression.csv")
exporter_tableau(evolution_matiere, "evolution_matiere.csv")
exporter_tableau(performance_filiere, "performance_filiere.csv")
exporter_tableau(performance_genre, "performance_genre.csv")
exporter_tableau(performance_age, "performance_age.csv")
exporter_tableau(performance_heures, "performance_heures.csv")
exporter_tableau(comparaison_absences, "comparaison_absences.csv")
exporter_tableau(evolution_absences, "evolution_absences.csv")

# Exporter la matrice de correlation
correlation_matrix <- cor(donnees[, c("heures_etude_semaine", "nb_absences_s1", 
                                      "nb_absences_s2", "moyenne_s1", "moyenne_s2")], 
                          use = "complete.obs")
correlation_df <- as.data.frame(correlation_matrix)
exporter_tableau(correlation_df, "matrice_correlation.csv")

