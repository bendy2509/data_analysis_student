# Fonction 1: Calculer moyenne generale
calcul_moyenne <- function(notes_s1, notes_s2) {
  moyenne_s1 <- rowMeans(notes_s1, na.rm = TRUE)
  moyenne_s2 <- rowMeans(notes_s2, na.rm = TRUE)
  return(list(moyenne_s1 = moyenne_s1, moyenne_s2 = moyenne_s2))
}

# Fonction 2: Categoriser les etudiants par age
categoriser_age <- function(age) {
  categorie <- cut(age,
                   breaks = c(17, 20, 22, 26),
                   labels = c("18-20", "21-22", "23-26"),
                   include.lowest = TRUE)
  return(categorie)
}

# Fonction 3: Exporter un tableau en CSV
exporter_tableau <- function(tableau, nom_fichier) {
  chemin <- file.path("output/tables", nom_fichier)
  write.csv(tableau, chemin, row.names = FALSE)
  cat("Tableau exporte:", chemin, "\n")
}

# Fonction 4: Creer un graphique et l'exporter
exporter_graphique <- function(graphique, nom_fichier, largeur = 10, hauteur = 6) {
  chemin <- file.path("output/figures", nom_fichier)
  ggsave(chemin, graphique, width = largeur, height = hauteur, dpi = 300)
  cat("Graphique exporte:", chemin, "\n")
}

# Fonction 5: Calculer taux de reussite
calcul_taux_reussite <- function(notes, seuil = 10) {
  reussite <- mean(notes >= seuil, na.rm = TRUE) * 100
  return(round(reussite, 1))
}