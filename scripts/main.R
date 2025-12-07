# Message de demarrage
cat("=== DEBUT DU PROJET - ANALYSE DE PERFORMANCE ETUDIANTE ===\n")
cat("=== PREPARE PAR : Bendy SERVILUS ===\n")
cat("Date :", format(Sys.Date(), "%d/%m/%Y"), "\n\n")

# Etape 0: Installer packages essentiels seulement
packages <- c("dplyr", "ggplot2", "readr", "here")
packages_manquants <- packages[!packages %in% installed.packages()[,"Package"]]

if (length(packages_manquants) > 0) {
  cat("Installation des packages manquants:", paste(packages_manquants, collapse = ", "), "\n")
  install.packages(packages_manquants, dependencies = FALSE)
}

# Charger les packages
library(dplyr)
library(ggplot2)
library(readr)
library(here)

# Etape 1: Charger les fonctions personnalisees
cat("1. Chargement des fonctions personnalisees...\n")
source("scripts/00_fonctions.R")

# Etape 2: Importer et nettoyer les donnees
cat("2. Importation et nettoyage des donnees...\n")
source("scripts/01_import_nettoyage.R")

# Etape 3: Executer les analyses descriptives
cat("3. Analyse descriptive...\n")
source("scripts/02_analyses_descriptives.R")

# Etape 4: Executer les analyses comparatives
cat("4. Analyses comparatives...\n")
source("scripts/03_analyses_comparatives.R")

# Etape 5: Generer les visualisations
cat("5. Generation des visualisations...\n")
source("scripts/04_visualisations.R")

# Etape 6: Message de fin
cat("\n=== PROJET TERMINE AVEC SUCCES ===\n")
cat("Tous les resultats sont disponibles dans le dossier 'output/'\n")