# Analyse 1 : Statistiques descriptives

cat("\n=== ANALYSE 1 : STATISTIQUES DESCRIPTIVES ===\n")

# Charger les donnees nettoyees
donnees <- read.csv("data/processed/donnees_nettoyees.csv")

# 3.1.1 Demographie
cat("\n--- 3.1.1 DEMOGRAPHIE ---\n")

# a) Distribution de l'age
cat("\na) Distribution de l'age:\n")
age_stats <- data.frame(
  Moyenne = mean(donnees$age, na.rm = TRUE),
  Mediane = median(donnees$age, na.rm = TRUE),
  Ecart_type = sd(donnees$age, na.rm = TRUE),
  Minimum = min(donnees$age, na.rm = TRUE),
  Maximum = max(donnees$age, na.rm = TRUE)
)
print(age_stats)

# b) Repartition par genre
cat("\nb) Repartition par genre:\n")
genre_table <- table(donnees$genre)
genre_pct <- prop.table(genre_table) * 100
genre_df <- data.frame(
  Effectif = as.numeric(genre_table),
  Pourcentage = round(as.numeric(genre_pct), 1)
)

# Ajoute les noms des lignes dans le df genre_df
rownames(genre_df) <- names(genre_table)
print(genre_df)

# c) Repartition par filiere
cat("\nc) Repartition par filiere:\n")
filiere_table <- table(donnees$filiere)
filiere_pct <- prop.table(filiere_table) * 100
filiere_df <- data.frame(
  Effectif = as.numeric(filiere_table),
  Pourcentage = round(as.numeric(filiere_pct), 1)
)
rownames(filiere_df) <- names(filiere_table)
print(filiere_df)

# d) Distribution geographique
cat("\nd) Distribution geographique (top 5 villes):\n")
ville_table <- table(donnees$ville_origine)
ville_df <- data.frame(
  Ville = names(ville_table),
  Effectif = as.numeric(ville_table),
  Pourcentage = round(as.numeric(prop.table(ville_table)) * 100, 2)
)
ville_df <- ville_df[order(-ville_df$Effectif), ]
print(head(ville_df, 5))

# 3.1.2 Comportement academique
cat("\n--- 3.1.2 COMPORTEMENT ACADEMIQUE ---\n")

# a) Statistiques sur les heures d'etude
cat("\na) Heures d'etude par semaine:\n")
heures_stats <- data.frame(
  Moyenne = mean(donnees$heures_etude_semaine, na.rm = TRUE),
  Mediane = median(donnees$heures_etude_semaine, na.rm = TRUE),
  Ecart_type = sd(donnees$heures_etude_semaine, na.rm = TRUE),
  Minimum = min(donnees$heures_etude_semaine, na.rm = TRUE),
  Maximum = max(donnees$heures_etude_semaine, na.rm = TRUE)
)
print(heures_stats)

# b) Statistiques sur les absences
cat("\nJe vais fais en sorte de donner les statistiques sur S1 et sur S2")
cat("\nb) Absences semestre 1:\n")
abs_s1_stats <- data.frame(
  Moyenne = mean(donnees$nb_absences_s1, na.rm = TRUE),
  Mediane = median(donnees$nb_absences_s1, na.rm = TRUE),
  Ecart_type = sd(donnees$nb_absences_s1, na.rm = TRUE),
  Minimum = min(donnees$nb_absences_s1, na.rm = TRUE),
  Maximum = max(donnees$nb_absences_s1, na.rm = TRUE)
)
print(abs_s1_stats)

cat("\nAbsences semestre 2:\n")
abs_s2_stats <- data.frame(
  Moyenne = mean(donnees$nb_absences_s2, na.rm = TRUE),
  Mediane = median(donnees$nb_absences_s2, na.rm = TRUE),
  Ecart_type = sd(donnees$nb_absences_s2, na.rm = TRUE),
  Minimum = min(donnees$nb_absences_s2, na.rm = TRUE),
  Maximum = max(donnees$nb_absences_s2, na.rm = TRUE)
)
print(abs_s2_stats)

# c) Relation heures d'etude / absences
cat("\nc) Correlation heures d'etude / absences:\n")
cor_heures_abs_s1 <- cor(donnees$heures_etude_semaine, donnees$nb_absences_s1, use = "complete.obs")
cor_heures_abs_s2 <- cor(donnees$heures_etude_semaine, donnees$nb_absences_s2, use = "complete.obs")
cat("Correlation avec absences S1:", round(cor_heures_abs_s1, 3), "\n")
cat("Correlation avec absences S2:", round(cor_heures_abs_s2, 3), "\n")

# 3.1.3 Performance academique
cat("\n--- 3.1.3 PERFORMANCE ACADEMIQUE ---\n")

# Liste des matieres
matieres <- c("math", "info", "physique", "economie", "anglais")

# a) Statistiques pour chaque matiere
cat("\na) Statistiques par matiere (S1):\n")
for (matiere in matieres) {
  var_s1 <- paste0("note_", matiere, "_s1")
  if (var_s1 %in% names(donnees)) {
    cat("\n", toupper(matiere), "S1:\n")
    stats <- data.frame(
      Moyenne = mean(donnees[[var_s1]], na.rm = TRUE),
      Mediane = median(donnees[[var_s1]], na.rm = TRUE),
      Ecart_type = sd(donnees[[var_s1]], na.rm = TRUE),
      Minimum = min(donnees[[var_s1]], na.rm = TRUE),
      Maximum = max(donnees[[var_s1]], na.rm = TRUE)
    )
    print(stats)
  }
}


# b) Distribution des moyennes generales
cat("\nb) Distribution des moyennes generales:\n")
moyennes_stats <- data.frame(
  Semestre = c("S1", "S2"),
  Moyenne = c(mean(donnees$moyenne_s1, na.rm = TRUE), mean(donnees$moyenne_s2, na.rm = TRUE)),
  Mediane = c(median(donnees$moyenne_s1, na.rm = TRUE), median(donnees$moyenne_s2, na.rm = TRUE)),
  Ecart_type = c(sd(donnees$moyenne_s1, na.rm = TRUE), sd(donnees$moyenne_s2, na.rm = TRUE)),
  Minimum = c(min(donnees$moyenne_s1, na.rm = TRUE), min(donnees$moyenne_s2, na.rm = TRUE)),
  Maximum = c(max(donnees$moyenne_s1, na.rm = TRUE), max(donnees$moyenne_s2, na.rm = TRUE)),
  row.names = TRUE
)
print(moyennes_stats)

# c) Taux de reussite global
cat("\nc) Taux de reussite global (>= 10/20) :\n")
taux_reussite_s1 <- mean(donnees$moyenne_s1 >= 10, na.rm = TRUE) * 100

taux_reussite_s2 <- mean(donnees$moyenne_s2 >= 10, na.rm = TRUE) * 100
cat("Semestre 1:", round(taux_reussite_s1, 1), "%\n")
cat("Semestre 2:", round(taux_reussite_s2, 1), "%\n")

# d) Identification des matieres les plus difficiles
cat("\nd) Matieres les plus difficiles (moyenne la plus basse S1):\n")
moyennes_matieres_s1 <- sapply(matieres, function(m) {
  var <- paste0("note_", m, "_s1")
  if (var %in% names(donnees)) {
    mean(donnees[[var]], na.rm = TRUE)
  } else NA
})

matieres_difficiles <- data.frame(
  Matiere = matieres,
  Moyenne_S1 = round(moyennes_matieres_s1, 2)
)
matieres_difficiles <- matieres_difficiles[order(matieres_difficiles$Moyenne_S1), ]
row.names(matieres_difficiles) <- NULL
print(matieres_difficiles)

# Exporter les resultats
cat("\n=== EXPORTATION DES RESULTATS ===\n")

# Creer un dossier pour les tables si inexistant
if (!dir.exists("output/tables")) dir.create("output/tables", recursive = TRUE)

# Exporter les statistiques descriptives
write.csv(age_stats, "output/tables/stats_age.csv", row.names = FALSE)
write.csv(genre_df, "output/tables/repartition_genre.csv", row.names = TRUE)
write.csv(filiere_df, "output/tables/repartition_filiere.csv", row.names = TRUE)
write.csv(heures_stats, "output/tables/stats_heures_etude.csv", row.names = FALSE)
write.csv(moyennes_stats, "output/tables/stats_moyennes.csv", row.names = FALSE)
write.csv(matieres_difficiles, "output/tables/matieres_difficiles.csv", row.names = FALSE)

cat("\nAnalyse descriptive terminee et resultats exportes!\n")
