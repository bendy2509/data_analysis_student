# Importer les donnees
chemin_fichier <- file.path("data", "raw", "etudiants_performance.csv")

# Lire le fichier CSV avec read.csv
donnees_brutes <- read.csv(chemin_fichier)

# Verifications basiques
cat("=== VERIFICATION DES DONNEES ===\n")
cat("Nombre d'etudiants:", nrow(donnees_brutes), "\n")
cat("Nombre de variables:", ncol(donnees_brutes), "\n")

# Afficher les premieres lignes pour le test
cat("\nPremieres lignes :\n")
print(head(donnees_brutes))

# Nettoyage minimal
# Verifier les valeurs manquantes
cat("\nValeurs manquantes par colonne:\n")
for (col in names(donnees_brutes)) {
  nb_na <- sum(is.na(donnees_brutes[[col]]))
  if (nb_na > 0) {
    cat(col, ":", nb_na, "valeurs manquantes\n")
  }
}

# Calculer les moyennes si pas presentes
if (!"moyenne_s1" %in% names(donnees_brutes)) {
  notes_s1 <- c("note_math_s1", "note_info_s1", "note_physique_s1", 
                "note_economie_s1", "note_anglais_s1")
  donnees_brutes$moyenne_s1 <- rowMeans(donnees_brutes[, notes_s1], na.rm = TRUE)
}

if (!"moyenne_s2" %in% names(donnees_brutes)) {
  notes_s2 <- c("note_math_s2", "note_info_s2", "note_physique_s2",
                "note_economie_s2", "note_anglais_s2")
  donnees_brutes$moyenne_s2 <- rowMeans(donnees_brutes[, notes_s2], na.rm = TRUE)
}

# Ajouter variation
donnees_brutes$variation_moyenne <- donnees_brutes$moyenne_s2 - donnees_brutes$moyenne_s1

# 3.4 Categoriser age simplement
donnees_brutes$categorie_age <- ifelse(donnees_brutes$age <= 20, "18-20",
                                       ifelse(donnees_brutes$age <= 22, "21-22", "23-26"))

# Reussite
donnees_brutes$reussite_s1 <- ifelse(donnees_brutes$moyenne_s1 >= 10, "Reussi", "Echoue")
donnees_brutes$reussite_s2 <- ifelse(donnees_brutes$moyenne_s2 >= 10, "Reussi", "Echoue")

# Sauvegarder
donnees <- donnees_brutes
write.csv(donnees, "data/processed/donnees_nettoyees.csv", row.names = FALSE)

cat("\n=== DONNEES NETTOYEES ET SAUVEGARDEES ===\n")
cat("Fichier sauvegarde: data/processed/donnees_nettoyees.csv\n")
cat("Variables ajoutees: moyenne_s1, moyenne_s2, variation_moyenne, categorie_age, reussite_s1, reussite_s2\n")