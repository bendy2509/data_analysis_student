# Projet 1 - Analyse de Performance Etudiante

# Message de demarrage
cat("=== DEBUT DU PROJET - ANALYSE DE PERFORMANCE ETUDIANTE ===\n")
cat("=== PREPARE PAR : Bendy SERVILUS ===\n")
cat("Date : 08/12/2025\n")

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
cat("\n", rep("=", 60), "\n")
cat("=== PROJET TERMINE AVEC SUCCES ===\n")
cat(rep("=", 60), "\n\n")


