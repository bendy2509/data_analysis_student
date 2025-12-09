# PROJET 1 - ANALYSE DE PERFORMANCE ÉTUDIANTE
# Auteur : Bendy SERVILUS
# Date : 08/12/2024

## INSTRUCTIONS D'EXÉCUTION

### PRÉREQUIS
- R version 4.0 ou supérieure
- RStudio (recommandé)
- Connexion internet pour installation des packages

### ÉTAPES D'EXÉCUTION

1. TÉLÉCHARGER ET PRÉPARER LE PROJET
   - Décompresser l'archive dans un dossier de votre choix
   - Ouvrir RStudio
   - Dans RStudio : File > Open Project > Sélectionner le dossier du projet

2. EXÉCUTER L'ANALYSE COMPLÈTE
   - Dans la console RStudio, exécuter :
     source("scripts/main.R")
   - Laissez le script s'exécuter complètement (2-3 minutes)
   - Suivez les messages dans la console

3. VÉRIFIER LES RÉSULTATS
   - Les résultats seront générés dans les dossiers :
     * output/tables/ : fichiers CSV avec les résultats
     * output/figures/ : graphiques PNG (300 DPI)
     * data/processed/ : données nettoyées

### STRUCTURE DES DOSSIERS

data_analysis_student/
├── data/                    # Données
│   ├── raw/                # Données brutes
│   └── processed/          # Données nettoyées
├── scripts/                # Code R
│   ├── 00_fonctions.R     # Fonctions personnalisées
│   ├── 01_import_nettoyage.R
│   ├── 02_analyses_descriptives.R
│   ├── 03_analyses_comparatives.R
│   ├── 04_visualisations.R
│   └── main.R             # Script principal
├── output/                 # Résultats
│   ├── figures/           # Graphiques
│   └── tables/            # Tableaux
├── rapport.pdf           # Rapport sur le projet
└── README.txt            # Ce fichier


### PACKAGES REQUIS
Les packages suivants seront installés automatiquement :
- dplyr     : Manipulation de données
- ggplot2   : Création de graphiques
- readr     : Importation de données
- here      : Chemins relatifs  comme demandé
- reshape2  : Transformation de données

### EN CAS DE PROBLÈME

1. ERREUR D'INSTALLATION DES PACKAGES

   # Installer manuellement :
   install.packages(c("dplyr", "ggplot2", "readr", "here", "reshape2"))

2. FICHIER DE DONNÉES MANQUANT
   - Vérifier que etudiants_performance.csv est dans data/raw/
   - Télécharger depuis : https://github.com/bendy2509/data_analysis_student.git
   
3. SCRIPTS NON TROUVÉS
   - Vérifier la structure des dossiers
   - S'assurer d'être dans le bon répertoire de travail

### ANALYSES EFFECTUÉES

Le projet exécute 5 analyses principales :

1. STATISTIQUES DESCRIPTIVES
   - Démographie, comportement, performance

2. ÉVOLUTION S1 → S2
   - Comparaison des semestres
   - Progressions/régressions

3. PERFORMANCE PAR GROUPES
   - Par filière, genre, âge

4. FACTEURS DE RÉUSSITE
   - Impact heures d'étude et absences

5. VISUALISATIONS
   - 6 graphiques professionnels

### SORTIES GÉNÉRÉES

Après exécution, vous obtiendrez :
- 15+ fichiers CSV avec les résultats
- 6 graphiques PNG en haute résolution
- 2 fichiers de données nettoyées
- Messages détaillés dans la console

---
Note : Ce projet a été développé avec R version 4.2.3
Date limite : Jeudi 08 Décembre 2025 à 23h59
