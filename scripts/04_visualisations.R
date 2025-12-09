# Analyse 5 : Visualisations

cat("\n", rep("=", 70), "\n")
cat("ANALYSE 5 : VISUALISATIONS DES RESULTATS\n")
cat(rep("-", 70), "\n")

# ============================================================================
# INITIALISATION
# ============================================================================

cat("\n1. Chargement des donnees et packages...\n")

# Charger les libraries
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2", dependencies = FALSE)
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr", dependencies = FALSE)
}
if (!requireNamespace("reshape2", quietly = TRUE)) {
  install.packages("reshape2", dependencies = FALSE)
}
if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr", dependencies = FALSE)
}

library(ggplot2)
library(tidyr)
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

cat("OK Packages charges et donnees pretes pour visualisation\n")

# ============================================================================
# GRAPHIQUE 1 : Histogramme - Distribution des moyennes
# ============================================================================

cat("\n", rep("-", 60),  "\n")
cat("GRAPHIQUE 1 : HISTOGRAMME - DISTRIBUTION DES MOYENNES\n")
cat(rep("-", 60), "\n")

# Preparer les donnees pour histogramme
donnees_long <- data.frame(
  Semestre = rep(c("S1", "S2"), each = nrow(donnees)),
  Moyenne = c(donnees$moyenne_s1, donnees$moyenne_s2)
)

# Calculer les statistiques pour annotations
stats_s1 <- resume_statistique(donnees$moyenne_s1)
stats_s2 <- resume_statistique(donnees$moyenne_s2)

# Histogramme avec densite
g1 <- ggplot(donnees_long, aes(x = Moyenne, fill = Semestre)) +
  geom_histogram(aes(y = ..density..), 
                 alpha = 0.6, 
                 position = "identity",
                 bins = 20,
                 color = "black",
                 boundary = 0) +
  geom_density(alpha = 0.3, adjust = 1.5) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "red", linewidth = 1) +
  
  # Ajouter les statistiques
  geom_vline(xintercept = stats_s1$moyenne, linetype = "dotted", 
             color = "#4E79A7", linewidth = 0.8, alpha = 0.7) +
  geom_vline(xintercept = stats_s2$moyenne, linetype = "dotted", 
             color = "#F28E2B", linewidth = 0.8, alpha = 0.7) +
  
  # Personnalisation
  scale_fill_manual(values = c("S1" = "#4E79A7", "S2" = "#F28E2B"),
                    name = "Semestre") +
  labs(title = "DISTRIBUTION DES MOYENNES GENERALES PAR SEMESTRE",
       subtitle = paste("S1: moyenne =", round(stats_s1$moyenne, 2), 
                        "/20 | S2: moyenne =", round(stats_s2$moyenne, 2), "/20"),
       x = "Moyenne generale (sur 20)",
       y = "Densite de probabilite",
       caption = paste("Ligne rouge: seuil de reussite (10/20)",
                       "\nLignes pointillees: moyennes par semestre")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    plot.caption = element_text(size = 9, color = "gray40", hjust = 0)
  )

# Exporter avec la fonction personnalisee
chemin_g1 <- exporter_graphique(g1, "01_distribution_moyennes.png", 
                                largeur = 11, hauteur = 7)
cat("-> Histogramme exporte:", basename(chemin_g1), "\n")

# ============================================================================
# GRAPHIQUE 2 : Boxplot - Moyennes par filiere
# ============================================================================

cat("\n", rep("-", 60),  "\n")
cat("GRAPHIQUE 2 : BOXPLOT - MOYENNES PAR FILIERE\n")
cat(rep("-", 60), "\n")

# Calculer les statistiques pour annotations
stats_filiere <- donnees %>%
  group_by(filiere) %>%
  summarise(
    mediane = median(moyenne_s2, na.rm = TRUE),
    moyenne = mean(moyenne_s2, na.rm = TRUE),
    q1 = quantile(moyenne_s2, 0.25, na.rm = TRUE),
    q3 = quantile(moyenne_s2, 0.75, na.rm = TRUE),
    n = n(),
    taux_reussite = calcul_taux_reussite(moyenne_s2, seuil = 10)
  ) %>%
  mutate(label = paste0("n=", n, "\nmed=", round(mediane, 1), "\ntx=", taux_reussite, "%"))

# Boxplot avec jitter
g2 <- ggplot(donnees, aes(x = filiere, y = moyenne_s2, fill = filiere)) +
  geom_boxplot(alpha = 0.85, outlier.color = "#D55E00", 
               outlier.size = 2.5, outlier.shape = 16) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1.8, color = "gray30") +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red", linewidth = 1.2) +
  
  # Ajouter les annotations
  geom_text(data = stats_filiere, 
            aes(x = filiere, y = q3 + 0.5, label = label),
            size = 3.2, fontface = "bold", lineheight = 0.9) +
  
  # Personnalisation
  scale_fill_brewer(palette = "Set2", name = "Filiere") +
  labs(title = "COMPARAISON DES PERFORMANCES PAR FILIERE (SEMESTRE 2)",
       subtitle = "Distribution des moyennes generales S2",
       x = "Filiere universitaire",
       y = "Moyenne generale S2 (sur 20)",
       caption = paste("Points: etudiants individuels",
                       "\nLigne rouge: seuil de reussite (10/20)",
                       "\nAnnotations: n=effectif, med=mediane, tx=taux reussite %")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),
    axis.text.y = element_text(size = 10),
    legend.position = "none",
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    plot.caption = element_text(size = 9, color = "gray40", hjust = 0)
  ) +
  ylim(min(donnees$moyenne_s2, na.rm = TRUE) - 1, 
       max(donnees$moyenne_s2, na.rm = TRUE) + 2)

# Exporter avec la fonction personnalisee
chemin_g2 <- exporter_graphique(g2, "02_comparaison_filieres.png", 
                                largeur = 12, hauteur = 8)
cat("-> Boxplot exporte:", basename(chemin_g2), "\n")

# ============================================================================
# GRAPHIQUE 3 : Scatter plot - Heures d'etude vs Performance
# ============================================================================

cat("\n", rep("-", 60),  "\n")
cat("GRAPHIQUE 3 : SCATTER PLOT - HEURES D'ETUDE VS PERFORMANCE\n")
cat(rep("-", 60), "\n")

# Calculer la regression lineaire
lm_model <- lm(moyenne_s2 ~ heures_etude_semaine, data = donnees)
r_squared <- summary(lm_model)$r.squared
p_value <- summary(lm_model)$coefficients[2, 4]
correlation <- cor(donnees$heures_etude_semaine, donnees$moyenne_s2, use = "complete.obs")

# Scatter plot avec regression
g3 <- ggplot(donnees, aes(x = heures_etude_semaine, y = moyenne_s2)) +
  geom_point(aes(color = filiere, shape = genre), 
             size = 3.5, alpha = 0.75) +
  geom_smooth(method = "lm", 
              se = TRUE, 
              color = "#D32F2F", 
              fill = "#FFCDD2",
              alpha = 0.3,
              linewidth = 1.2) +
  
  # Ajouter equation de regression
  annotate("text",
           x = max(donnees$heures_etude_semaine, na.rm = TRUE) * 0.65,
           y = max(donnees$moyenne_s2, na.rm = TRUE) * 0.85,
           label = paste0("Equation: y = ", 
                          round(coef(lm_model)[1], 2), 
                          " + ", 
                          round(coef(lm_model)[2], 3), 
                          "x\n",
                          "R² = ", round(r_squared, 3),
                          " | p = ", ifelse(p_value < 0.001, "< 0.001", round(p_value, 4)),
                          "\n",
                          "Correlation: r = ", round(correlation, 3)),
           size = 4, 
           fontface = "bold",
           color = "#B71C1C",
           hjust = 0) +
  
  # Personnalisation
  scale_color_brewer(palette = "Set1", name = "Filiere") +
  scale_shape_manual(values = c(16, 17), name = "Genre") +
  labs(title = "RELATION ENTRE HEURES D'ETUDE ET PERFORMANCE ACADEMIQUE",
       subtitle = "Semestre 2 - Analyse de correlation et regression lineaire",
       x = "Heures d'etude par semaine",
       y = "Moyenne generale S2 (sur 20)",
       caption = paste("Regression lineaire avec intervalle de confiance 95%",
                       "\nPoints colores par filiere, formes par genre")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    plot.caption = element_text(size = 9, color = "gray40", hjust = 0)
  )

# Exporter avec la fonction personnalisee
chemin_g3 <- exporter_graphique(g3, "03_heures_etude_vs_performance.png", 
                                largeur = 13, hauteur = 8)
cat("-> Scatter plot exporte:", basename(chemin_g3), "\n")

# ============================================================================
# GRAPHIQUE 4 : Graphique d'evolution - S1 vs S2
# ============================================================================

cat("\n", rep("-", 60),  "\n")
cat("GRAPHIQUE 4 : EVOLUTION DES PERFORMANCES S1 → S2\n")
cat(rep("-", 60), "\n")

# Selectionner un echantillon representatif (pas aleatoire)
# Prendre les 15 etudiants avec la plus forte variation (positive et negative)
top_prog <- donnees[order(-donnees$variation_moyenne), ][1:8, ]
top_reg <- donnees[order(donnees$variation_moyenne), ][1:7, ]
donnees_echantillon <- rbind(top_prog, top_reg)

# Calculer les statistiques pour l'echantillon
moyenne_variation_ech <- mean(donnees_echantillon$variation_moyenne, na.rm = TRUE)
pourcentage_prog_ech <- mean(donnees_echantillon$variation_moyenne > 0, na.rm = TRUE) * 100

# Graphique d'evolution avec fleches
g4 <- ggplot() +
  # Fleches pour montrer l'evolution
  geom_segment(data = donnees_echantillon,
               aes(x = 1, xend = 2,
                   y = moyenne_s1, yend = moyenne_s2, 
                   color = variation_moyenne),
               arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
               linewidth = 1.2,
               alpha = 0.8) +
  
  # Points S1
  geom_point(data = donnees_echantillon,
             aes(x = 1, y = moyenne_s1, fill = "S1"),
             size = 5, shape = 21, color = "black", alpha = 0.9) +
  
  # Points S2
  geom_point(data = donnees_echantillon,
             aes(x = 2, y = moyenne_s2, fill = "S2"),
             size = 5, shape = 21, color = "black", alpha = 0.9) +
  
  # Ligne horizontale pour seuil
  geom_hline(yintercept = 10, linetype = "dashed", color = "red", linewidth = 1.2) +
  
  # Etiquettes pour les extremes
  geom_text(data = donnees_echantillon[which.max(donnees_echantillon$variation_moyenne), ],
            aes(x = 1.5, y = (moyenne_s1 + moyenne_s2)/2 + 0.3,
                label = paste0("+", round(variation_moyenne, 1))),
            size = 3.5, fontface = "bold", color = "#1B5E20") +
  
  geom_text(data = donnees_echantillon[which.min(donnees_echantillon$variation_moyenne), ],
            aes(x = 1.5, y = (moyenne_s1 + moyenne_s2)/2 - 0.3,
                label = paste0(round(variation_moyenne, 1))),
            size = 3.5, fontface = "bold", color = "#B71C1C") +
  
  # Personnalisation
  scale_x_continuous(breaks = c(1, 2), 
                     labels = c("Semestre 1", "Semestre 2"),
                     limits = c(0.8, 2.2)) +
  scale_color_gradient2(low = "#D32F2F", mid = "gray70", high = "#388E3C",
                        midpoint = 0, name = "Variation") +
  scale_fill_manual(values = c("S1" = "#4E79A7", "S2" = "#F28E2B"),
                    name = "Semestre") +
  labs(title = "EVOLUTION INDIVIDUELLE DES PERFORMANCES S1 → S2",
       subtitle = paste("Echantillon de 15 etudiants extremes",
                        "\nVariation moyenne: ", round(moyenne_variation_ech, 2), 
                        " points | ", round(pourcentage_prog_ech, 1), "% en progression", sep = ""),
       x = "Semestre academique",
       y = "Moyenne generale (sur 20)",
       caption = paste("Fleches: evolution individuelle (couleur = amplitude variation)",
                       "\nLigne rouge: seuil de reussite (10/20)",
                       "\nEtiquettes: variations extremes")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    plot.caption = element_text(size = 9, color = "gray40", hjust = 0)
  ) +
  ylim(min(c(donnees$moyenne_s1, donnees$moyenne_s2), na.rm = TRUE) - 1,
       max(c(donnees$moyenne_s1, donnees$moyenne_s2), na.rm = TRUE) + 1)

# Exporter avec la fonction personnalisee
chemin_g4 <- exporter_graphique(g4, "04_evolution_s1_s2.png", 
                                largeur = 12, hauteur = 9)
cat("-> Graphique d'evolution exporte:", basename(chemin_g4), "\n")

# ============================================================================
# GRAPHIQUE 5 : Graphique creatif - Matrice de correlation
# ============================================================================

cat("\n", rep("-", 60),  "\n")
cat("GRAPHIQUE 5 : MATRICE DE CORRELATION - FACTEURS CLES\n")
cat(rep("-", 60), "\n")

# Selectionner les variables pour la correlation
variables_corr <- donnees %>%
  select(moyenne_s1, moyenne_s2, heures_etude_semaine, 
         nb_absences_s1, nb_absences_s2, age)

# Renommer les variables pour plus de clarte
colnames(variables_corr) <- c("Moyenne_S1", "Moyenne_S2", "Heures_etude", 
                              "Absences_S1", "Absences_S2", "Age")

# Calculer la matrice de correlation
matrice_corr <- cor(variables_corr, use = "complete.obs")

# Convertir en format long pour ggplot
corr_melt <- melt(matrice_corr)

# Heatmap des correlations
g5 <- ggplot(corr_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = ifelse(abs(value) > 0.05, round(value, 2), "")), 
            color = "black", 
            size = 4.5, 
            fontface = "bold") +
  
  # Ajouter les valeurs de p si disponibles
  geom_text(aes(label = ifelse(abs(value) > 0.3 & value != 1, "*", "")),
            color = "darkred",
            size = 6,
            fontface = "bold",
            vjust = -1.5) +
  
  # Personnalisation des couleurs
  scale_fill_gradient2(low = "#D73027", 
                       mid = "white", 
                       high = "#1A9850",
                       midpoint = 0,
                       limits = c(-1, 1),
                       name = "Coefficient\nde correlation\n(r)") +
  
  labs(title = "MATRICE DE CORRELATION ENTRE VARIABLES CLES",
       subtitle = "Analyse des relations lineaires entre facteurs academiques",
       x = "",
       y = "",
       caption = paste("Echelle: rouge (correlation negative) -> blanc (pas de correlation) -> vert (correlation positive)",
                       "\n*: correlation notable (|r| > 0.3)",
                       "\nValeurs: coefficients de correlation de Pearson")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11, face = "bold"),
    axis.text.y = element_text(size = 11, face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10, hjust = 0.5),
    legend.text = element_text(size = 9),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.caption = element_text(size = 9, color = "gray40", hjust = 0)
  ) +
  coord_fixed()

# Exporter avec la fonction personnalisee
chemin_g5 <- exporter_graphique(g5, "05_matrice_correlation.png", 
                                largeur = 10, hauteur = 8)
cat("-> Matrice de correlation exportee:", basename(chemin_g5), "\n")

# ============================================================================
# GRAPHIQUE 6 : Taux de reussite par filiere et genre
# ============================================================================

cat("\n", rep("-", 60),  "\n")
cat("GRAPHIQUE 6 : TAUX DE REUSSITE PAR FILIERE ET GENRE\n")
cat(rep("-", 60), "\n")

# Calculer les taux de reussite par filiere et genre
taux_reussite <- donnees %>%
  group_by(filiere, genre) %>%
  summarise(
    n = n(),
    taux_s1 = calcul_taux_reussite(moyenne_s1, seuil = 10),
    taux_s2 = calcul_taux_reussite(moyenne_s2, seuil = 10),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = starts_with("taux_"),
               names_to = "semestre",
               values_to = "taux") %>%
  mutate(semestre = ifelse(semestre == "taux_s1", "Semestre 1", "Semestre 2"),
         taux_label = paste0(round(taux, 0), "%"))

# Graphique a barres groupees
g6 <- ggplot(taux_reussite, aes(x = filiere, y = taux, fill = genre)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), 
           alpha = 0.85, color = "black", linewidth = 0.3) +
  geom_hline(yintercept = 50, linetype = "dotted", color = "gray40", linewidth = 0.8) +
  geom_hline(yintercept = 75, linetype = "dashed", color = "green4", linewidth = 0.8, alpha = 0.5) +
  geom_hline(yintercept = 25, linetype = "dashed", color = "red3", linewidth = 0.8, alpha = 0.5) +
  
  # Ajouter les etiquettes de pourcentage
  geom_text(aes(label = taux_label, group = genre),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3.2, fontface = "bold") +
  
  facet_wrap(~ semestre, ncol = 2) +
  
  scale_fill_manual(values = c("Homme" = "#4E79A7", "Femme" = "#E15759"),
                    name = "Genre") +
  labs(title = "TAUX DE REUSSITE PAR FILIERE ET GENRE",
       subtitle = "Pourcentage d'etudiants avec moyenne >= 10/20",
       x = "Filiere universitaire",
       y = "Taux de reussite (%)",
       caption = paste("Ligne pointillee grise: 50% (seuil median)",
                       "\nLignes tiretees: vert=75% (bon niveau), rouge=25% (niveau critique)",
                       "\nEtiquettes: pourcentage de reussite")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 11, color = "white"),
    strip.background = element_rect(fill = "gray30", color = "black"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    plot.caption = element_text(size = 9, color = "gray40", hjust = 0)
  ) +
  ylim(0, 105)

# Exporter avec la fonction personnalisee
chemin_g6 <- exporter_graphique(g6, "06_taux_reussite_genre_filiere.png", 
                                largeur = 14, hauteur = 7)
cat("-> Graphique bonus exporte:", basename(chemin_g6), "\n")
