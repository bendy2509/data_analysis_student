# Analyses 2, 3 et 4 : Evolution, Groupes, Facteurs

cat("\n=== ANALYSES COMPARATIVES ===\n")

# Charger les donnees nettoyees
donnees <- read.csv("data/processed/donnees_nettoyees.csv")

# Convertir en facteurs si necessaire
donnees$genre <- as.factor(donnees$genre)
donnees$filiere <- as.factor(donnees$filiere)
donnees$categorie_age <- as.factor(donnees$categorie_age)
donnees$reussite_s1 <- as.factor(donnees$reussite_s1)
donnees$reussite_s2 <- as.factor(donnees$reussite_s2)

# ============================================================================
# ANALYSE 2 : EVOLUTION S1 -> S2
# ============================================================================

cat("\n=== ANALYSE 2 : EVOLUTION S1 -> S2 ===\n")

# 3.2.1 Evolution Globale
cat("\n--- 3.2.1 EVOLUTION GLOBALE ---\n")

# a) Comparaison des moyennes S1 vs S2
cat("\na) Comparaison moyenne S1 vs S2:\n")
comparaison_moyennes <- data.frame(
  Semestre = c("S1", "S2", "Variation"),
  Moyenne = c(mean(donnees$moyenne_s1, na.rm = TRUE),
              mean(donnees$moyenne_s2, na.rm = TRUE),
              mean(donnees$moyenne_s2, na.rm = TRUE) - mean(donnees$moyenne_s1, na.rm = TRUE)),
  Mediane = c(median(donnees$moyenne_s1, na.rm = TRUE),
              median(donnees$moyenne_s2, na.rm = TRUE),
              median(donnees$moyenne_s2, na.rm = TRUE) - median(donnees$moyenne_s1, na.rm = TRUE))
)
print(comparaison_moyennes)

# Test t apparie pour comparer S1 et S2
cat("\nTest t apparie pour moyennes S1 vs S2:\n")
test_t <- t.test(donnees$moyenne_s1, donnees$moyenne_s2, paired = TRUE)
cat("t =", round(test_t$statistic, 3), "\n")
cat("p-value =", round(test_t$p.value, 4), "\n")
if (test_t$p.value < 0.05) {
  cat("Difference significative au seuil de 5%\n")
} else {
  cat("Difference non significative au seuil de 5%\n")
}

# b) Distribution des variations de notes
cat("\nb) Distribution des variations de moyennes:\n")
variation_stats <- data.frame(
  Moyenne_variation = mean(donnees$variation_moyenne, na.rm = TRUE),
  Mediane_variation = median(donnees$variation_moyenne, na.rm = TRUE),
  SD_variation = sd(donnees$variation_moyenne, na.rm = TRUE),
  Min_variation = min(donnees$variation_moyenne, na.rm = TRUE),
  Max_variation = max(donnees$variation_moyenne, na.rm = TRUE),
  Pourcentage_progression = mean(donnees$variation_moyenne > 0, na.rm = TRUE) * 100,
  Pourcentage_regression = mean(donnees$variation_moyenne < 0, na.rm = TRUE) * 100
)
print(variation_stats)

# c) Identification des progressions et regressions
cat("\nc) Etudiants en progression/regression:\n")
donnees$evolution <- ifelse(donnees$variation_moyenne > 0.5, "Progression",
                            ifelse(donnees$variation_moyenne < -0.5, "Regression", "Stable"))

# Faison une table pour construit le dataframe
evolution_table <- table(donnees$evolution)
evolution_df <- data.frame(
  Categorie = names(evolution_table),
  Effectif = as.numeric(evolution_table),
  Pourcentage = round(as.numeric(prop.table(evolution_table)) * 100, 1)
)
print(evolution_df)

# d) Top 10 des progressions
cat("\nd) Top 10 des plus fortes progressions:\n")
top_progression <- donnees[order(-donnees$variation_moyenne), c("id_etudiant", "nom", "moyenne_s1", "moyenne_s2", "variation_moyenne")]
print(head(top_progression, 10))

# e) Top 10 des regressions
cat("\ne) Top 10 des plus fortes regressions:\n")
top_regression <- donnees[order(donnees$variation_moyenne), c("id_etudiant", "nom", "moyenne_s1", "moyenne_s2", "variation_moyenne")]
print(head(top_regression, 10))

# 3.2.2 Evolution par Matiere
cat("\n--- 3.2.2 EVOLUTION PAR MATIERE ---\n")

# Liste des matieres
matieres <- c("math", "info", "physique", "economie", "anglais")

# a) Comparaison S1 vs S2 pour chaque matiere
cat("\na) Evolution par matiere:\n")
evolution_matiere <- data.frame()

for (matiere in matieres) {
  var_s1 <- paste0("note_", matiere, "_s1")
  var_s2 <- paste0("note_", matiere, "_s2")
  
  if (var_s1 %in% names(donnees) & var_s2 %in% names(donnees)) {
    moyenne_s1 <- mean(donnees[[var_s1]], na.rm = TRUE)
    moyenne_s2 <- mean(donnees[[var_s2]], na.rm = TRUE)
    variation <- moyenne_s2 - moyenne_s1
    
    evolution_matiere <- rbind(evolution_matiere, data.frame(
      Matiere = toupper(matiere),
      Moyenne_S1 = round(moyenne_s1, 2),
      Moyenne_S2 = round(moyenne_s2, 2),
      Variation = round(variation, 2),
      Statut = ifelse(variation > 0, "Progression", 
                      ifelse(variation < 0, "Regression", "Stable"))
    ))
  }
}

print(evolution_matiere, row.names = FALSE)

# b) Matieres en progression / regression
cat("\nb) Matieres avec la plus forte progression:\n")
print(evolution_matiere[order(-evolution_matiere$Variation), ], row.names = FALSE)

# ============================================================================
# ANALYSE 3 : PERFORMANCE PAR GROUPES (25 points)
# ============================================================================

cat("\n\n=== ANALYSE 3 : PERFORMANCE PAR GROUPES ===\n")

# 3.3.1 Par Filiere
cat("\n--- 3.3.1 PERFORMANCE PAR FILIERE ---\n")

# a) Statistiques descriptives par filiere
cat("\na) Statistiques par filiere:\n")
performance_filiere <- donnees %>%
  group_by(filiere) %>%
  summarise(
    Effectif = n(),
    Moyenne_S1 = round(mean(moyenne_s1, na.rm = TRUE), 2),
    Moyenne_S2 = round(mean(moyenne_s2, na.rm = TRUE), 2),
    Variation = round(Moyenne_S2 - Moyenne_S1, 2),
    Taux_reussite_S1 = round(mean(moyenne_s1 >= 10, na.rm = TRUE) * 100, 1),
    Taux_reussite_S2 = round(mean(moyenne_s2 >= 10, na.rm = TRUE) * 100, 1),
    Heures_etude_moy = round(mean(heures_etude_semaine, na.rm = TRUE), 1),
    Absences_moy_S1 = round(mean(nb_absences_s1, na.rm = TRUE), 1),
    Absences_moy_S2 = round(mean(nb_absences_s2, na.rm = TRUE), 1)
  ) %>%
  arrange(desc(Moyenne_S2))

print(as.data.frame(performance_filiere))

# b) Comparaison des moyennes entre filieres
cat("\nb) Filiere la plus performante S2:", 
    as.character(performance_filiere$filiere[which.max(performance_filiere$Moyenne_S2)]),
    "avec moyenne =", max(performance_filiere$Moyenne_S2), "\n")

cat("Filiere la moins performante S2:", 
    as.character(performance_filiere$filiere[which.min(performance_filiere$Moyenne_S2)]),
    "avec moyenne =", min(performance_filiere$Moyenne_S2), "\n")

# 3.3.2 Par Genre (8 points)
cat("\n--- 3.3.2 PERFORMANCE PAR GENRE ---\n")

# a) Comparaison des performances H/F
cat("\na) Comparaison Hommes vs Femmes:\n")
performance_genre <- donnees %>%
  group_by(genre) %>%
  summarise(
    Effectif = n(),
    Pourcentage = round(n()/nrow(donnees)*100, 1),
    Moyenne_S1 = round(mean(moyenne_s1, na.rm = TRUE), 2),
    Moyenne_S2 = round(mean(moyenne_s2, na.rm = TRUE), 2),
    Variation = round(Moyenne_S2 - Moyenne_S1, 2),
    Taux_reussite_S1 = round(mean(moyenne_s1 >= 10, na.rm = TRUE) * 100, 1),
    Taux_reussite_S2 = round(mean(moyenne_s2 >= 10, na.rm = TRUE) * 100, 1),
    Heures_etude_moy = round(mean(heures_etude_semaine, na.rm = TRUE), 1)
  )

print(as.data.frame(performance_genre), row.names = FALSE)


# 3.3.3 Par Categorie d'Age
cat("\n--- 3.3.3 PERFORMANCE PAR CATEGORIE D'AGE ---\n")

# Les categories sont deja creees dans le nettoyage
cat("\na) Performance par categorie d'age:\n")
performance_age <- donnees %>%
  group_by(categorie_age) %>%
  summarise(
    Effectif = n(),
    Pourcentage = round(n()/nrow(donnees)*100, 1),
    Moyenne_S1 = round(mean(moyenne_s1, na.rm = TRUE), 2),
    Moyenne_S2 = round(mean(moyenne_s2, na.rm = TRUE), 2),
    Variation = round(Moyenne_S2 - Moyenne_S1, 2),
    Taux_reussite_S1 = round(mean(moyenne_s1 >= 10, na.rm = TRUE) * 100, 1),
    Taux_reussite_S2 = round(mean(moyenne_s2 >= 10, na.rm = TRUE) * 100, 1),
    Heures_etude_moy = round(mean(heures_etude_semaine, na.rm = TRUE), 1),
    Absences_moy = round(mean(nb_absences_s1 + nb_absences_s2, na.rm = TRUE)/2, 1)
  ) %>%
  arrange(categorie_age)

print(as.data.frame(performance_age))

# ============================================================================
# ANALYSE 4 : FACTEURS DE REUSSITE
# ============================================================================

cat("\n\n=== ANALYSE 4 : FACTEURS DE REUSSITE ===\n")

# 3.4.1 Impact des Heures d'Etude (8 points)
cat("\n--- 3.4.1 IMPACT DES HEURES D'ETUDE ---\n")

# a) Correlation heures d'etude / performance
cat("\na) Correlation heures d'etude avec performance:\n")
cor_heures_s1 <- cor(donnees$heures_etude_semaine, donnees$moyenne_s1, use = "complete.obs")
cor_heures_s2 <- cor(donnees$heures_etude_semaine, donnees$moyenne_s2, use = "complete.obs")

cat("Correlation avec moyenne S1:", round(cor_heures_s1, 3), "\n")
cat("Correlation avec moyenne S2:", round(cor_heures_s2, 3), "\n")

# Interpretation
if (abs(cor_heures_s1) > 0.3) {
  cat("Correlation notable avec S1\n")
}
if (abs(cor_heures_s2) > 0.3) {
  cat("Correlation notable avec S2\n")
}

# b) Analyse par tranche d'heures d'etude
cat("\nb) Performance par tranche d'heures d'etude:\n")

# Creer des tranches d'heures
donnees$tranche_heures <- cut(
  donnees$heures_etude_semaine,
  breaks = c(0, 10, 15, 20, 25, 30, 40),
  labels = c("0-10h", "11-15h", "16-20h", "21-25h", "26-30h", "31-40h"),
  include.lowest = TRUE, na.rm = TRUE
)

performance_heures <- donnees %>%
  group_by(tranche_heures) %>%
  summarise(
    Effectif = n(),
    Pourcentage = round(n()/nrow(donnees)*100, 1),
    Moyenne_S1 = round(mean(moyenne_s1, na.rm = TRUE), 2),
    Moyenne_S2 = round(mean(moyenne_s2, na.rm = TRUE), 2),
    Taux_reussite_S2 = round(mean(moyenne_s2 >= 10, na.rm = TRUE) * 100, 1),
    Absences_moy = round(mean(nb_absences_s1 + nb_absences_s2, na.rm = TRUE)/2, 1)
  ) %>%
  arrange(tranche_heures)

print(as.data.frame(performance_heures))


#TODO: Je vais y retourner
# c) Identification d'un seuil optimal
cat("\nc) Seuil d'heures d'etude optimal:\n")
# Chercher la tranche avec la meilleure performance
meilleure_tranche <- performance_heures[which.max(performance_heures$Moyenne_S2), ]
cat("Tranche avec meilleure performance S2:", as.character(meilleure_tranche$tranche_heures),
    "(moyenne =", meilleure_tranche$Moyenne_S2, ")\n")

# 3.4.2 Impact des Absences
cat("\n--- 3.4.2 IMPACT DES ABSENCES ---\n")

# a) Correlation absences / performance
cat("\na) Correlation absences avec performance:\n")
cor_abs_s1_s1 <- cor(donnees$nb_absences_s1, donnees$moyenne_s1, use = "complete.obs")
cor_abs_s2_s2 <- cor(donnees$nb_absences_s2, donnees$moyenne_s2, use = "complete.obs")
cor_abs_tot <- cor(donnees$nb_absences_s1 + donnees$nb_absences_s2, 
                   donnees$moyenne_s1 + donnees$moyenne_s2, use = "complete.obs")

cat("Correlation absences S1 / moyenne S1:", round(cor_abs_s1_s1, 3), "\n")
cat("Correlation absences S2 / moyenne S2:", round(cor_abs_s2_s2, 3), "\n")
cat("Correlation absences totales / moyenne totale:", round(cor_abs_tot, 3), "\n")

# b) Comparaison faibles vs fortes absences
cat("\nb) Comparaison etudiants avec faibles vs fortes absences:\n")

# Definir seuil : mediane des absences totales
seuil_absences <- median(donnees$nb_absences_s1 + donnees$nb_absences_s2, na.rm = TRUE)
donnees$groupe_absences <- ifelse((donnees$nb_absences_s1 + donnees$nb_absences_s2) <= seuil_absences,
                                  "Faibles absences", "Fortes absences")

comparaison_absences <- donnees %>%
  group_by(groupe_absences) %>%
  summarise(
    Effectif = n(),
    Pourcentage = round(n()/nrow(donnees)*100, 1),
    Moyenne_S1 = round(mean(moyenne_s1, na.rm = TRUE), 2),
    Moyenne_S2 = round(mean(moyenne_s2, na.rm = TRUE), 2),
    Taux_reussite_S2 = round(mean(moyenne_s2 >= 10, na.rm = TRUE) * 100, 1),
    Heures_etude_moy = round(mean(heures_etude_semaine, na.rm = TRUE), 1)
  )

print(as.data.frame(comparaison_absences))


# c) Evolution des absences S1 -> S2
cat("\nd) Evolution des absences S1 -> S2:\n")
evolution_absences <- data.frame(
  Semestre = c("S1", "S2", "Variation"),
  Moyenne_absences = c(mean(donnees$nb_absences_s1, na.rm = TRUE),
                       mean(donnees$nb_absences_s2, na.rm = TRUE),
                       mean(donnees$nb_absences_s2, na.rm = TRUE) - mean(donnees$nb_absences_s1, na.rm = TRUE)),
  Mediane_absences = c(median(donnees$nb_absences_s1, na.rm = TRUE),
                       median(donnees$nb_absences_s2, na.rm = TRUE),
                       median(donnees$nb_absences_s2, na.rm = TRUE) - median(donnees$nb_absences_s1, na.rm = TRUE))
)
print(evolution_absences)

# ============================================================================
# EXPORTATION DES RESULTATS
# ============================================================================

cat("\n=== EXPORTATION DES RESULTATS ===\n")

# Assurer que le dossier existe
if (!dir.exists("output/tables")) dir.create("output/tables", recursive = TRUE)

# Exporter les tables importantes
write.csv(comparaison_moyennes, "output/tables/comparaison_s1_s2.csv", row.names = FALSE)
write.csv(evolution_matiere, "output/tables/evolution_matiere.csv", row.names = FALSE)
write.csv(as.data.frame(performance_filiere), "output/tables/performance_filiere.csv", row.names = FALSE)
write.csv(as.data.frame(performance_genre), "output/tables/performance_genre.csv", row.names = FALSE)
write.csv(as.data.frame(performance_age), "output/tables/performance_age.csv", row.names = FALSE)
write.csv(as.data.frame(performance_heures), "output/tables/performance_heures.csv", row.names = FALSE)
write.csv(as.data.frame(comparaison_absences), "output/tables/comparaison_absences.csv", row.names = FALSE)

# Exporter les top 10
write.csv(head(top_progression, 10), "output/tables/top10_progression.csv", row.names = FALSE)
write.csv(head(top_regression, 10), "output/tables/top10_regression.csv", row.names = FALSE)

# Sauvegarder aussi les donnees avec les nouvelles variables
write.csv(donnees, "data/processed/donnees_completes.csv", row.names = FALSE)

cat("\nAnalyses comparatives terminees!\n")
cat("Resultats exportes dans 'output/tables/'\n")
cat("Donnees completes sauvegardees: 'data/processed/donnees_completes.csv'\n")
