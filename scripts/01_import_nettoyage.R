# Importer, nettoyer et preparer les donnees
cat("\n", rep("=", 60), "\n", sep = "")
cat("IMPORTATION ET NETTOYAGE DES DONNEES\n")
cat(rep("=", 60), "\n")

# ============================================================================
# ETAPE 1: IMPORTATION DES DONNEES
# ============================================================================

cat("\n1. Importation des donnees brutes...\n")

# Definir le chemin du fichier
chemin_fichier <-
  file.path("data", "raw", "etudiants_performance.csv")

# Verifier si le fichier existe
if (!file.exists(chemin_fichier)) {
  stop(
    "ERREUR: Fichier ",
    chemin_fichier,
    " introuvable!\n",
    "Verifiez que le fichier est dans data/raw/"
  )
}

# Lire le fichier CSV
donnees_brutes <- read.csv(chemin_fichier, stringsAsFactors = FALSE)

cat("Fichier importe:", chemin_fichier, "\n")
cat("   Dimensions:",
    nrow(donnees_brutes),
    "lignes x",
    ncol(donnees_brutes),
    "colonnes\n")

# ============================================================================
# ETAPE 2: VERIFICATION INITIALE
# ============================================================================

cat("\n2. Verification initiale des donnees...\n")

# Apercu des donnees
cat("\nStructure des donnees:\n")
print(str(donnees_brutes, vec.len = 2))

cat("\nApercu des 5 premieres lignes:\n")
print(head(donnees_brutes, 5))

# Resume statistique des variables numeriques
cat("\nResume statistique:\n")
print(summary(donnees_brutes))

# ============================================================================
# ETAPE 3: NETTOYAGE DES DONNEES
# ============================================================================

cat("\n3. Nettoyage des donnees...\n")

# 3.1 Verifier les valeurs manquantes
cat("\n3.1 Valeurs manquantes par variable:\n")
valeurs_manquantes <-
  sapply(donnees_brutes, function(x)
    sum(is.na(x)))
valeurs_manquantes_df <- data.frame(
  Variable = names(valeurs_manquantes),
  NAs = as.numeric(valeurs_manquantes),
  Pourcentage = round(as.numeric(valeurs_manquantes) / nrow(donnees_brutes) * 100, 1)
)

# Afficher seulement les variables avec NAs
print(valeurs_manquantes_df[valeurs_manquantes_df$NAs > 0, ], row.names = FALSE)

# 3.2 Verifier les types de variables
cat("\n3.2 Verification des types de variables:\n")

# Convertir les variables categorielles en facteurs
variables_categorielles <-
  c("genre", "filiere", "ville_origine", "boursier")
for (var in variables_categorielles) {
  if (var %in% names(donnees_brutes)) {
    donnees_brutes[[var]] <- as.factor(donnees_brutes[[var]])
    cat("   ",
        var,
        "converti en facteur (",
        length(levels(donnees_brutes[[var]])),
        "niveaux)\n")
  }
}

# 3.3 Nettoyer les notes avec la fonction personnalisee
cat("\n3.3 Nettoyage des notes...\n")

# Liste des variables de notes
notes_vars <- c(
  "note_math_s1",
  "note_info_s1",
  "note_physique_s1",
  "note_economie_s1",
  "note_anglais_s1",
  "note_math_s2",
  "note_info_s2",
  "note_physique_s2",
  "note_economie_s2",
  "note_anglais_s2"
)

# Appliquer la fonction nettoyer_notes a chaque variable de note
for (var in notes_vars) {
  if (var %in% names(donnees_brutes)) {
    donnees_brutes[[var]] <-
      nettoyer_notes(donnees_brutes[[var]], min_val = 0, max_val = 20)
  }
}

# 3.4 Verifier les intervalles de valeurs
cat("\n3.4 Verification des intervalles de valeurs:\n")

# Age (devrait etre entre 18 et 26)
age_hors_intervalle <-
  sum(donnees_brutes$age < 18 |
        donnees_brutes$age > 26, na.rm = TRUE)
if (age_hors_intervalle > 0) {
  cat("   ATTENTION:", age_hors_intervalle, "ages hors de [18, 26]\n")
} else {
  cat("   Age: toutes les valeurs dans [18, 26]\n")
}

# Heures d'etude (devrait etre entre 5 et 40 selon la description)
heures_hors_intervalle <-
  sum(
    donnees_brutes$heures_etude_semaine < 5 |
      donnees_brutes$heures_etude_semaine > 40,
    na.rm = TRUE
  )
if (heures_hors_intervalle > 0) {
  cat("   ATTENTION:",
      heures_hors_intervalle,
      "heures d'etude hors de [5, 40]\n")
} else {
  cat("   Heures d'etude: toutes les valeurs dans [5, 40]\n")
}

# Absences (devraient etre positives ou nulles)
absences_negatives <- sum(donnees_brutes$nb_absences_s1 < 0 |
                            donnees_brutes$nb_absences_s2 < 0,
                          na.rm = TRUE)
if (absences_negatives > 0) {
  cat("   ATTENTION:",
      absences_negatives,
      "valeurs d'absences negatives\n")
  # Corriger les valeurs negatives
  donnees_brutes$nb_absences_s1[donnees_brutes$nb_absences_s1 < 0] <-
    0
  donnees_brutes$nb_absences_s2[donnees_brutes$nb_absences_s2 < 0] <-
    0
}

# ============================================================================
# ETAPE 3.5: TRAITEMENT DES VALEURS MANQUANTES
# ============================================================================

cat("\n3.5 Traitement des valeurs manquantes...\n")

# Compter le nombre total de NA avant traitement
na_avant <- sum(is.na(donnees_brutes))
cat("   Valeurs manquantes avant traitement:", na_avant, "\n")

# 3.5.1 Traitement des notes (utiliser la fonction traiter_na)
cat("\n   a) Traitement des notes manquantes:\n")
notes_vars <- c(
  "note_math_s1",
  "note_info_s1",
  "note_physique_s1",
  "note_economie_s1",
  "note_anglais_s1",
  "note_math_s2",
  "note_info_s2",
  "note_physique_s2",
  "note_economie_s2",
  "note_anglais_s2"
)

for (var in notes_vars) {
  if (var %in% names(donnees_brutes)) {
    donnees_brutes[[var]] <-
      traiter_na(donnees_brutes[[var]], methode = "mediane")
  }
}

# 3.5.2 Traitement des autres variables numeriques
cat("\n   b) Traitement des autres variables numeriques:\n")

# Age : mediane
donnees_brutes$age <-
  traiter_na(donnees_brutes$age, methode = "mediane")

# Heures d'etude : mediane
donnees_brutes$heures_etude_semaine <-
  traiter_na(donnees_brutes$heures_etude_semaine, methode = "mediane")

# Absences : zero (supposition: pas d'enregistrement = pas d'absence)
donnees_brutes$nb_absences_s1 <-
  traiter_na(donnees_brutes$nb_absences_s1, methode = "zero")
donnees_brutes$nb_absences_s2 <-
  traiter_na(donnees_brutes$nb_absences_s2, methode = "zero")

# 3.5.3 Traitement des variables categorielles
cat("\n   c) Traitement des variables categorielles:\n")
vars_cat <- c("genre", "filiere", "ville_origine", "boursier")

for (var in vars_cat) {
  if (var %in% names(donnees_brutes)) {
    # Convertir temporairement en caractere pour le traitement
    donnees_brutes[[var]] <- as.character(donnees_brutes[[var]])
    donnees_brutes[[var]] <-
      traiter_na(donnees_brutes[[var]], methode = "mode")
    # Reconvertir en facteur
    donnees_brutes[[var]] <- as.factor(donnees_brutes[[var]])
  }
}

# 3.5.4 Verification apres traitement
cat("\n   d) Verification apres traitement:\n")
na_apres <- sum(is.na(donnees_brutes))
cat("   Valeurs manquantes apres traitement:", na_apres, "\n")
cat("   Reduction de", na_avant - na_apres, "valeurs manquantes\n")

if (na_apres == 0) {
  cat("   SUCCES: Plus aucune valeur manquante!\n")
} else {
  # Afficher les variables avec NA restants
  na_restants <- sapply(donnees_brutes, function(x)
    sum(is.na(x)))
  if (sum(na_restants) > 0) {
    cat("\n   Variables avec NA restants:\n")
    for (var in names(na_restants[na_restants > 0])) {
      cat("   -", var, ":", na_restants[var], "NA\n")
    }
  }
}

# 3.5.5 Verification des moyennes (si elles existent deja)
cat("\n   e) Verification de la coherence des moyennes:\n")
if ("moyenne_s1" %in% names(donnees_brutes) &&
    "moyenne_s2" %in% names(donnees_brutes)) {
  # Verifier si les moyennes contiennent des NA
  na_moyenne_s1 <- sum(is.na(donnees_brutes$moyenne_s1))
  na_moyenne_s2 <- sum(is.na(donnees_brutes$moyenne_s2))
  
  if (na_moyenne_s1 > 0 || na_moyenne_s2 > 0) {
    cat("   ATTENTION: Moyennes avec NA - recalcul necessaire\n")
    # Recalculer les moyennes si necessaire
    notes_s1 <-
      donnees_brutes[, c(
        "note_math_s1",
        "note_info_s1",
        "note_physique_s1",
        "note_economie_s1",
        "note_anglais_s1"
      )]
    notes_s2 <-
      donnees_brutes[, c(
        "note_math_s2",
        "note_info_s2",
        "note_physique_s2",
        "note_economie_s2",
        "note_anglais_s2"
      )]
    
    moyennes <- calcul_moyenne(notes_s1, notes_s2)
    donnees_brutes$moyenne_s1 <- moyennes$moyenne_s1
    donnees_brutes$moyenne_s2 <- moyennes$moyenne_s2
    
    cat("   Moyennes recalculées après traitement des NA\n")
  } else {
    cat("   OK: Aucun NA dans les moyennes existantes\n")
  }
}


# ============================================================================
# ETAPE 4: CREATION DE NOUVELLES VARIABLES
# ============================================================================

cat("\n4. Creation de nouvelles variables...\n")

# 4.1 Calculer les moyennes si necessaire
cat("\n4.1 Calcul des moyennes generales...\n")

# Verifier si les moyennes existent deja
if (!all(c("moyenne_s1", "moyenne_s2") %in% names(donnees_brutes))) {
  # Preparer les matrices de notes
  notes_s1 <-
    donnees_brutes[, c(
      "note_math_s1",
      "note_info_s1",
      "note_physique_s1",
      "note_economie_s1",
      "note_anglais_s1"
    )]
  notes_s2 <-
    donnees_brutes[, c(
      "note_math_s2",
      "note_info_s2",
      "note_physique_s2",
      "note_economie_s2",
      "note_anglais_s2"
    )]
  
  # Utiliser la fonction personnalisee
  moyennes <- calcul_moyenne(notes_s1, notes_s2)
  
  donnees_brutes$moyenne_s1 <- moyennes$moyenne_s1
  donnees_brutes$moyenne_s2 <- moyennes$moyenne_s2
  
  cat("   Moyennes calculees avec la fonction calcul_moyenne()\n")
} else {
  cat("   Moyennes deja presentes dans les donnees\n")
}

# 4.2 Categoriser l'age avec la fonction personnalisee
cat("\n4.2 Categorisation de l'age...\n")
donnees_brutes$categorie_age <- categoriser_age(donnees_brutes$age)
cat("   Age categorise avec la fonction categoriser_age()\n")
cat("   Distribution:\n")
print(table(donnees_brutes$categorie_age))

# 4.3 Calculer la variation des moyennes
cat("\n4.3 Calcul de la variation S1 -> S2...\n")
variation <-
  calcul_variation(donnees_brutes$moyenne_s1, donnees_brutes$moyenne_s2)
donnees_brutes$variation_moyenne <- variation$variation

cat("   Variation moyenne:",
    round(variation$moyenne_variation, 2),
    "points\n")
cat("   Progression:",
    round(variation$pourcentage_progression, 1),
    "%\n")
cat("   Regression:",
    round(variation$pourcentage_regression, 1),
    "%\n")

# 4.4 Determiner le statut de reussite avec la fonction personnalisee
cat("\n4.4 Determination du statut de reussite...\n")
donnees_brutes$reussite_s1 <-
  ifelse(donnees_brutes$moyenne_s1 >= 10, "Reussi", "Echoue")
donnees_brutes$reussite_s2 <-
  ifelse(donnees_brutes$moyenne_s2 >= 10, "Reussi", "Echoue")

# Calculer les taux de reussite
taux_s1 <-
  calcul_taux_reussite(donnees_brutes$moyenne_s1,
                       seuil = 10,
                       pourcentage = TRUE)
taux_s2 <-
  calcul_taux_reussite(donnees_brutes$moyenne_s2,
                       seuil = 10,
                       pourcentage = TRUE)

cat("   Taux de reussite S1:", taux_s1, "%\n")
cat("   Taux de reussite S2:", taux_s2, "%\n")

# ============================================================================
# 4.5 TRAITEMENT RAPIDE DES HEURES > 40H
# ============================================================================

cat("\n4.5 Traitement des heures d'etude > 40h...\n")

# 1. Identifier les valeurs > 40h
indices_problemes <- which(donnees_brutes$heures_etude_semaine > 40)

if (length(indices_problemes) > 0) {
  cat("   ",
      length(indices_problemes),
      "etudiants avec >40h d'etude/semaine\n")
  
  # Afficher la liste
  problem_df <- donnees_brutes[indices_problemes,
                               c("id_etudiant", "nom", "heures_etude_semaine")]
  cat("   Liste:\n")
  print(problem_df)
  
  # 2. Corriger : limiter à 40h
  cat("\n   Correction: limitation a 40h (maximum realiste)\n")
  donnees_brutes$heures_etude_semaine[indices_problemes] <- 40
  cat("   Valeurs corrigees\n")
}

cat("\n4.5 Categorisation des heures d'etude...\n")
donnees_brutes$categorie_heures <-
  cut(
    donnees_brutes$heures_etude_semaine,
    breaks = c(0, 10, 15, 20, 25, 30, 40),
    labels = c("0-10h", "11-15h", "16-20h",
               "21-25h", "26-30h", "31-40h"),
    include.lowest = TRUE
  )

cat("   Distribution des heures d'etude:\n")
print(table(donnees_brutes$categorie_heures))

# 4.6 Calculer le total d'absences
cat("\n4.6 Calcul du total d'absences...\n")
donnees_brutes$total_absences <-
  donnees_brutes$nb_absences_s1 + donnees_brutes$nb_absences_s2
donnees_brutes$categorie_absences <-
  cut(
    donnees_brutes$total_absences,
    breaks = c(-1, 3, 6, 10, 100),
    labels = c("Faible (0-3)", "Modere (4-6)",
               "Eleve (7-10)", "Tres eleve (>10)"),
    include.lowest = TRUE
  )

cat("   Distribution des absences totales:\n")
print(table(donnees_brutes$categorie_absences))


# ============================================================================
# ETAPE 6: SAUVEGARDE DES DONNEES NETTOYEES
# ============================================================================

cat("\n6. Sauvegarde des donnees nettoyees...\n")
donnees <- donnees_brutes

# Utiliser la fonction personnalisee pour exporter
chemin_nettoye <-
  exporter_tableau(donnees, "donnees_nettoyees.csv", dossier = "data/processed")

# Sauvegarder aussi une version avec toutes les variables
chemin_complet <-
  exporter_tableau(donnees, "donnees_completes.csv", dossier = "data/processed")


cat("Valeurs manquantes totales:", sum(is.na(donnees)), "\n")
