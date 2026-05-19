 
#Nepieciešamās pakotnes:

if(!require(readxl)) install.packages("readxl")
if(!require(readxl)) install.packages("vegan")
if(!require(readxl)) install.packages("writexl")
if(!require(readxl)) install.packages("ggplot2")
if(!require(readxl)) install.packages("ggrepel")
if(!require(DT)) install.packages("DT")


library(readxl)
library(vegan)

augu_raw <- read_excel("failsTAB.xlsx", sheet = "Augi")
erces_raw <- read_excel("failsTAB.xlsx", sheet = "Erces")


# Augu sugu matrica
augu_matrica <- as.data.frame(augu_raw[,-1])
rownames(augu_matrica) <- augu_raw$vieta

# Bruņērču sugu matrica
erces_matrica <- as.data.frame(erces_raw[,-1])
rownames(erces_matrica) <- erces_raw$vieta

grupas <- as.factor(substr(rownames(augu_matrica), 1, 1))

#Logaritmēšana
augu_trans <- log1p(augu_matrica)
erces_trans <- log1p(erces_matrica)

#NMS analīze
nmds_erces <- metaMDS(erces_trans, distance = "bray", k = 2, trymax = 100)

fit_augi <- envfit(nmds_erces, augu_trans, permutations = 999)



options(scipen = -10)

nmds_coords <- scores(nmds_erces, display = "sites")

rezultati_list <- list()
for (suga in colnames(augu_trans)) {
  test1 <- cor.test(augu_trans[[suga]], nmds_coords[,1], method = "kendall", exact = FALSE)
  test2 <- cor.test(augu_trans[[suga]], nmds_coords[,2], method = "kendall", exact = FALSE)
  
  rezultati_list[[suga]] <- data.frame(
    Augu_suga = suga,
    Ass1_tau = test1$estimate,
    Ass1_p   = test1$p.value,
    Ass1_R2  = (test1$estimate)^2,
    Ass2_tau = test2$estimate,
    Ass2_p   = test2$p.value,
    Ass2_R2  = (test2$estimate)^2
  )
}

stat_tabula <- do.call(rbind, rezultati_list)
stat_tabula$Kopejais_envfit_R2 <- fit_augi$vectors$r
stat_tabula$Kopejais_envfit_p  <- fit_augi$vectors$pvals

#Atlasītas tās augu sugas kuras ir ar r2 determinācijas koeficientu, kas lielāks par 0,4 un arī p-vērtība
#statistiski būtiska <0,05
stat_atlasiti <- subset(stat_tabula, Kopejais_envfit_p < 0.05 & Kopejais_envfit_R2 > 0.4)

stat_atlasiti <- stat_atlasiti[order(-stat_atlasiti$Kopejais_envfit_R2), ]

#Attēlo rezultātus atlasītajā augu sugām

library(DT)

datatable(
  stat_atlasiti, 
  rownames = FALSE,             
  class = "cell-border stripe", 
  options = list(
    dom = 't',                  
    paging = FALSE,             
    ordering = FALSE,           
    scrollX = TRUE,             
    
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().container()).css({'font-size': '16px'});",
      "}"
    )
  ),
  caption = "Augu sugu vērtību tabula"
) %>% 
  formatRound(columns = c("Ass1_tau", "Ass1_R2", "Ass2_tau", "Ass2_R2", "Kopejais_envfit_R2"), digits = 4) %>% 
  formatSignif(columns = c("Ass1_p", "Ass2_p", "Kopejais_envfit_p"), digits = 3)














#Tās augu sugas, kurām r^2 determinācijas koeficients ir vismaz 0,4 un ar statistiski būtisku p-vērtību

#Ste_sp.
#Tri_pra
#Ran_rep
#Vic_cra
#Ran_acr
#Ely_rep
#Agr_cap


options(scipen = 999)

manas_izveletas <- c("Ste_sp.", "Tri_pra", "Ran_rep", "Vic_cra", "Ran_acr", 
                     "Ely_rep", "Agr_cap")


fit_atlasiti <- fit_augi
fit_atlasiti$vectors$r <- fit_augi$vectors$r[manas_izveletas]
fit_atlasiti$vectors$pvals <- fit_augi$vectors$pvals[manas_izveletas]
fit_atlasiti$vectors$arrows <- fit_augi$vectors$arrows[manas_izveletas, , drop = FALSE]

site_coords <- as.data.frame(scores(nmds_erces, display = "sites"))
site_coords$Group <- as.character(grupas)
site_coords$Label <- rownames(erces_matrica)

species_coords <- as.data.frame(scores(nmds_erces, display = "species"))
species_coords$Species <- rownames(species_coords)

vec_coords <- as.data.frame(scores(fit_atlasiti, display = "vectors"))
vec_coords$Species <- rownames(vec_coords)

multiplier <- 1.2 
vec_coords[, c("NMDS1", "NMDS2")] <- vec_coords[, c("NMDS1", "NMDS2")] * multiplier

library(ggplot2)
library(ggrepel)

p <- ggplot() +
  geom_point(data = site_coords, aes(x = NMDS1, y = NMDS2, color = Group), 
             shape = 17, size = 4) + 
  
  geom_text_repel(data = site_coords, aes(x = NMDS1, y = NMDS2, label = Label, color = Group),
                  size = 4, box.padding = 0.3, fontface = "bold") +
  
  geom_text_repel(data = species_coords, aes(x = NMDS1, y = NMDS2, label = Species),
                  color = "black", fontface = "italic", size = 5,
                  force = 8.5, max.overlaps = Inf) +
  
  geom_segment(data = vec_coords, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.3, "cm")), 
               color = "darkgreen", lwd = 1) +
  
  geom_text_repel(data = vec_coords, aes(x = NMDS1, y = NMDS2, label = Species),
                  color = "darkgreen", fontface = "bold", size = 6,
                  point.padding = 0, 
                  box.padding = 0.5,
                  
                  nudge_x = vec_coords$NMDS1 * 0.15, 
                  nudge_y = vec_coords$NMDS2 * 0.15, 
                  direction = "both",               
                  
                  segment.color = "darkgreen",
                  segment.alpha = 0.6)+
  
  scale_color_manual(values = c("A" = "#C096F0", "B" = "#59DC78", "C" = "#F3BE78")) +
  
  theme_bw() + 
  
  theme(
    aspect.ratio = 1,
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), 
    legend.position = "right",
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.margin = margin(1, 1, 1, 1, "cm") 
  ) +
  
  coord_fixed() + 
  labs(x = "NMDS1", y = "NMDS2", color = "Audzētava")

print(p)

