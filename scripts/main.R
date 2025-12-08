# Projet 1 - Analyse de Performance Etudiante

# Message de demarrage
cat("=== DEBUT DU PROJET - ANALYSE DE PERFORMANCE ETUDIANTE ===\n")
cat("=== PREPARE PAR : Bendy SERVILUS ===\n")
cat("Date :", format(Sys.Date(), "%d/%m/%Y"), "\n")
cat("Heure :", format(Sys.time(), "%H:%M"), "\n\n")

# Definir le repertoire de travail
# Utiliser here() pour des chemins relatifs
projet_dir <- here::here()
cat("Repertoire du projet:", projet_dir, "\n\n")

# Etape 1: Installer packages
cat("ETAPE 1: Installation des packages...\n")
packages <- c("dplyr", "ggplot2", "readr", "here", "reshape2")
packages_manquants <- packages[!packages %in% installed.packages()[,"Package"]]

if (length(packages_manquants) > 0) {
  cat("Installation des packages manquants:", paste(packages_manquants, collapse = ", "), "\n")
  install.packages(packages_manquants, dependencies = FALSE, quiet = TRUE)
  cat("Installation terminee.\n")
} else {
  cat("Tous les packages sont deja installes.\n")
}

# Etape 2: Charger les packages
cat("\nETAPE 2: Chargement des packages...\n")
library(dplyr)
library(ggplot2)
library(readr)
library(here)
cat("Packages charges avec succes.\n")

# Etape 3: Charger les fonctions personnalisees
cat("\nETAPE 3: Chargement des fonctions personnalisees...\n")
if (file.exists("scripts/00_fonctions.R")) {
  source("scripts/00_fonctions.R")
  cat("Fonctions personnalisees chargees.\n")
} else {
  stop("ERREUR: Fichier scripts/00_fonctions.R introuvable!")
}

# Etape 4: Importer et nettoyer les donnees
cat("\nETAPE 4: Importation et nettoyage des donnees...\n")
if (file.exists("scripts/01_import_nettoyage.R")) {
  source("scripts/01_import_nettoyage.R")
} else {
  stop("ERREUR: Fichier scripts/01_import_nettoyage.R introuvable!")
}

# Etape 5: Executer les analyses descriptives
cat("\nETAPE 5: Analyse descriptive...\n")
if (file.exists("scripts/02_analyses_descriptives.R")) {
  source("scripts/02_analyses_descriptives.R")
} else {
  stop("ERREUR: Fichier scripts/02_analyses_descriptives.R introuvable!")
}

# Etape 6: Executer les analyses comparatives
cat("\nETAPE 6: Analyses comparatives...\n")
if (file.exists("scripts/03_analyses_comparatives.R")) {
  source("scripts/03_analyses_comparatives.R")
} else {
  stop("ERREUR: Fichier scripts/03_analyses_comparatives.R introuvable!")
}

# Etape 7: Generer les visualisations
cat("\nETAPE 7: Generation des visualisations...\n")
if (file.exists("scripts/04_visualisations.R")) {
  # Verifier si reshape2 est installe pour les visualisations
  if (!requireNamespace("reshape2", quietly = TRUE)) {
    install.packages("reshape2", dependencies = FALSE, quiet = TRUE)
  }
  library(reshape2)
  
  source("scripts/04_visualisations.R")
} else {
  stop("ERREUR: Fichier scripts/04_visualisations.R introuvable!")
}

# Etape 8: Message de fin
cat("\n" + rep("=", 60) + "\n")
cat("=== PROJET TERMINE AVEC SUCCES ===\n")
cat(rep("=", 60) + "\n\n")

cat("RESUME DES LIVRABLES CREES:\n")
cat("-" * 40, "\n")

# Compter les fichiers crees
if (dir.exists("output/tables")) {
  tables <- list.files("output/tables", pattern = "\\.csv$")
  cat("Tables CSV:", length(tables), "fichiers\n")
}

if (dir.exists("output/figures")) {
  figures <- list.files("output/figures", pattern = "\\.png$")
  cat("Graphiques PNG:", length(figures), "fichiers\n")
}

if (dir.exists("data/processed")) {
  processed <- list.files("data/processed", pattern = "\\.csv$")
  cat("Donnees traitees:", length(processed), "fichiers\n")
}

cat("\nCHEMINS IMPORTANTS:\n")
cat("-" * 40, "\n")
cat("Donnees nettoyees: data/processed/donnees_nettoyees.csv\n")
cat("Donnees completes: data/processed/donnees_completes.csv\n")
cat("Resultats: output/tables/\n")
cat("Visualisations: output/figures/\n")

cat("\nINSTRUCTIONS POUR LE RAPPORT:\n")
cat("-" * 40, "\n")
cat("1. Ouvrez 'rapport.Rmd' dans RStudio\n")
cat("2. Cliquez sur 'Knit' pour generer le PDF\n")
cat("3. Verifiez que tous les resultats sont inclus\n")

cat("\n" + rep("*", 60) + "\n")
cat("Duree totale:", format(Sys.time() - start_time), "\n")
cat(rep("*", 60) + "\n")
