# 04_visualisations.R
# Analyse 5 : Visualisations (15 points)

cat("\n=== ANALYSE 5 : VISUALISATIONS ===\n")

# Charger les libraries
library(ggplot2)
library(dplyr)
library(reshape2)

# Charger les donnees completes
donnees <- read.csv("data/processed/donnees_completes.csv")

# Convertir en facteurs
donnees$genre <- as.factor(donnees$genre)
donnees$filiere <- as.factor(donnees$filiere)
donnees$categorie_age <- as.factor(donnees$categorie_age)
donnees$reussite_s1 <- as.factor(donnees$reussite_s1)
donnees$reussite_s2 <- as.factor(donnees$reussite_s2)

# Creer le dossier pour les figures si inexistant
if (!dir.exists("output/figures")) {
  dir.create("output/figures", recursive = TRUE)
}

# ============================================================================
# GRAPHIQUE 1 : Histogramme - Distribution des moyennes (3 points)
# ============================================================================

cat("\n1. Creation de l'histogramme des moyennes...\n")

# Preparer les donnees pour histogramme
donnees_long <- data.frame(
  Semestre = rep(c("S1", "S2"), each = nrow(donnees)),
  Moyenne = c(donnees$moyenne_s1, donnees$moyenne_s2)
)

# Histogramme avec densite
g1 <- ggplot(donnees_long, aes(x = Moyenne, fill = Semestre)) +
  geom_histogram(aes(y = ..density..), 
                 alpha = 0.6, 
                 position = "identity",
                 bins = 15,
                 color = "black") +
  geom_density(alpha = 0.3, adjust = 1.5) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "red", linewidth = 1) +
  
  # Personnalisation
  scale_fill_manual(values = c("S1" = "#4E79A7", "S2" = "#F28E2B")) +
  labs(title = "Distribution des moyennes generales par semestre",
       subtitle = paste("S1: n =", nrow(donnees), "etudiants | S2: n =", nrow(donnees), "etudiants"),
       x = "Moyenne generale (/20)",
       y = "Densite",
       caption = "Ligne rouge: seuil de reussite (10/20)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

# Exporter
ggsave("output/figures/01_distribution_moyennes.png", 
       g1, width = 10, height = 6, dpi = 300)
cat("Histogramme exporte: output/figures/01_distribution_moyennes.png\n")

# ============================================================================
# GRAPHIQUE 2 : Boxplot - Moyennes par filiere (3 points)
# ============================================================================

cat("\n2. Creation du boxplot par filiere...\n")

# Calculer les stats pour annotations
stats_filiere <- donnees %>%
  group_by(filiere) %>%
  summarise(
    mediane = median(moyenne_s2, na.rm = TRUE),
    moyenne = mean(moyenne_s2, na.rm = TRUE),
    q1 = quantile(moyenne_s2, 0.25, na.rm = TRUE),
    q3 = quantile(moyenne_s2, 0.75, na.rm = TRUE),
    n = n()
  ) %>%
  mutate(label = paste0("n=", n, "\nmed=", round(mediane, 1)))

# Boxplot
g2 <- ggplot(donnees, aes(x = filiere, y = moyenne_s2, fill = filiere)) +
  geom_boxplot(alpha = 0.8, outlier.color = "red", outlier.size = 2) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1.5) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red", linewidth = 1) +
  
  # Ajouter les annotations de mediane
  geom_text(data = stats_filiere, 
            aes(x = filiere, y = q3 + 0.3, label = label),
            size = 3, fontface = "bold") +
  
  # Personnalisation
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Distribution des moyennes S2 par filiere",
       subtitle = paste("Comparaison des performances academiques"),
       x = "Filiere",
       y = "Moyenne S2 (/20)",
       fill = "Filiere",
       caption = "Points: etudiants individuels | Ligne rouge: seuil de reussite") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

# Exporter
ggsave("output/figures/02_comparaison_filieres.png", 
       g2, width = 10, height = 7, dpi = 300)
cat("Boxplot exporte: output/figures/02_comparaison_filieres.png\n")

# ============================================================================
# GRAPHIQUE 3 : Scatter plot - Heures d'etude vs Performance (3 points)
# ============================================================================

cat("\n3. Creation du scatter plot heures d'etude vs performance...\n")

# Calculer la regression lineaire
lm_model <- lm(moyenne_s2 ~ heures_etude_semaine, data = donnees)
r_squared <- summary(lm_model)$r.squared
p_value <- summary(lm_model)$coefficients[2, 4]

# Scatter plot avec regression
g3 <- ggplot(donnees, aes(x = heures_etude_semaine, y = moyenne_s2)) +
  geom_point(aes(color = filiere, shape = genre), 
             size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", 
              se = TRUE, 
              color = "darkred", 
              fill = "lightcoral",
              alpha = 0.2) +
  
  # Ajouter equation de regression
  geom_text(x = max(donnees$heures_etude_semaine, na.rm = TRUE) * 0.7,
            y = max(donnees$moyenne_s2, na.rm = TRUE) * 0.9,
            label = paste0("y = ", 
                           round(coef(lm_model)[1], 2), 
                           " + ", 
                           round(coef(lm_model)[2], 3), 
                           "x\nR² = ", round(r_squared, 3),
                           "\np = ", format(p_value, scientific = TRUE, digits = 3)),
            size = 4, 
            fontface = "bold",
            color = "darkred") +
  
  # Personnalisation
  scale_color_brewer(palette = "Set1") +
  labs(title = "Relation entre heures d'etude et performance academique",
       subtitle = paste("Semestre 2 | Correlation:",
                        round(cor(donnees$heures_etude_semaine, donnees$moyenne_s2, use = "complete.obs"), 3)),
       x = "Heures d'etude par semaine",
       y = "Moyenne generale S2 (/20)",
       color = "Filiere",
       shape = "Genre",
       caption = "Regression lineaire avec intervalle de confiance 95%") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    legend.position = "right",
    legend.box = "vertical",
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

# Exporter
ggsave("output/figures/03_heures_etude_vs_performance.png", 
       g3, width = 11, height = 7, dpi = 300)
cat("Scatter plot exporte: output/figures/03_heures_etude_vs_performance.png\n")

# ============================================================================
# GRAPHIQUE 4 : Graphique d'evolution - S1 vs S2 (3 points)
# ============================================================================

cat("\n4. Creation du graphique d'evolution S1 vs S2...\n")

# Preparer les donnees pour les fleches
# Prendre un echantillon pour lisibilite (20 etudiants)
set.seed(123)
echantillon_idx <- sample(1:nrow(donnees), min(20, nrow(donnees)))
donnees_echantillon <- donnees[echantillon_idx, ]

# Graphique avec fleches
g4 <- ggplot() +
  # Points S1
  geom_point(data = donnees_echantillon,
             aes(x = "Semestre 1", y = moyenne_s1, color = filiere),
             size = 4, alpha = 0.7) +
  # Points S2
  geom_point(data = donnees_echantillon,
             aes(x = "Semestre 2", y = moyenne_s2, color = filiere),
             size = 4, alpha = 0.7) +
  # Fleches pour montrer l'evolution
  geom_segment(data = donnees_echantillon,
               aes(x = "Semestre 1", xend = "Semestre 2",
                   y = moyenne_s1, yend = moyenne_s2, color = filiere),
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               alpha = 0.5) +
  # Ligne horizontale pour seuil
  geom_hline(yintercept = 10, linetype = "dashed", color = "red", linewidth = 1) +
  
  # Personnalisation
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Evolution des moyennes S1 → S2 (echantillon de 20 etudiants)",
       subtitle = paste("Variation moyenne:",
                        round(mean(donnees$variation_moyenne, na.rm = TRUE), 2),
                        "points |",
                        round(mean(donnees$variation_moyenne > 0, na.rm = TRUE) * 100, 1),
                        "% en progression"),
       x = "Semestre",
       y = "Moyenne generale (/20)",
       color = "Filiere",
       caption = "Fleches: evolution individuelle | Ligne rouge: seuil de reussite") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  ylim(min(c(donnees$moyenne_s1, donnees$moyenne_s2), na.rm = TRUE) - 0.5,
       max(c(donnees$moyenne_s1, donnees$moyenne_s2), na.rm = TRUE) + 0.5)

# Exporter
ggsave("output/figures/04_evolution_s1_s2.png", 
       g4, width = 10, height = 7, dpi = 300)
cat("Graphique d'evolution exporte: output/figures/04_evolution_s1_s2.png\n")

# ============================================================================
# GRAPHIQUE 5 : Graphique creatif et pertinent (3 points)
# ============================================================================

cat("\n5. Creation du graphique creatif...\n")

# Graphique 5: Heatmap des correlations entre variables
# Preparer les donnees pour correlation
variables_corr <- donnees %>%
  select(moyenne_s1, moyenne_s2, heures_etude_semaine, 
         nb_absences_s1, nb_absences_s2, age)

# Calculer la matrice de correlation
matrice_corr <- cor(variables_corr, use = "complete.obs")

# Convertir en format long pour ggplot
corr_melt <- melt(matrice_corr)

# Heatmap des correlations
g5 <- ggplot(corr_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(value, 2)), 
            color = "black", 
            size = 4, 
            fontface = "bold") +
  
  # Personnalisation des couleurs
  scale_fill_gradient2(low = "#D73027", 
                       mid = "white", 
                       high = "#1A9850",
                       midpoint = 0,
                       limits = c(-1, 1),
                       name = "Correlation") +
  
  labs(title = "Matrice de correlation entre variables cles",
       subtitle = "Analyse des relations entre facteurs academiques",
       x = "",
       y = "",
       caption = "Valeurs positives (vert): correlation positive\nValeurs negatives (rouge): correlation negative") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  coord_fixed()  # Pour avoir des carres parfaits

# Exporter
ggsave("output/figures/05_matrice_correlation.png", 
       g5, width = 9, height = 7, dpi = 300)
cat("Heatmap des correlations exporte: output/figures/05_matrice_correlation.png\n")

# ============================================================================
# RESUME ET VERIFICATION
# ============================================================================

cat("\n=== RESUME DES VISUALISATIONS ===\n")
cat("1. Histogramme distribution des moyennes: OK\n")
cat("2. Boxplot moyennes par filiere: OK\n")
cat("3. Scatter plot heures d'etude vs performance: OK\n")
cat("4. Graphique d'evolution S1 vs S2: OK\n")
cat("5. Heatmap des correlations: OK\n")

cat("Tous les graphiques sont sauvegardes dans 'output/figures/'\n")
cat("Resolution: 300 DPI\n")
cat("Format: PNG\n")

# Afficher les chemins des fichiers crees
fichiers_figures <- list.files("output/figures", pattern = "\\.png$", full.names = TRUE)
cat("\nFichiers crees:\n")
for (f in fichiers_figures) {
  file_info <- file.info(f)
  cat(basename(f), "|", 
      round(file_info$size/1024, 1), "KB |",
      format(file_info$mtime, "%H:%M"), "\n")
}

cat("\nToutes les visualisations sont terminees avec succes!\n")

