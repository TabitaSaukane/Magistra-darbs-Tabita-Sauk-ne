
#Nepieciešamās pakotnes:

if(!require(readxl)) install.packages("readxl")
if(!require(readxl)) install.packages("ggplot2")
if(!require(readxl)) install.packages("ggtext")

# Bruņērču indivīdu skaita attēlošana audzētavu augsnes paraugos

library(readxl)

audz1<- read_excel("Augsne1.xlsx")

# Kategoriju mainīgo pārvēršana par faktoriem
audz1$fveids=factor(audz1$veids)
audz1$fvieta=factor(audz1$vieta)

audz1$fveids=factor(audz1$veids,ordered=TRUE,levels=c("Kontrole","Ziemeli","Zem_ekskrementiem"))
audz1$fvieta=factor(audz1$vieta,ordered=TRUE,levels=c("A", "B", "C"))


library(ggplot2)

# Attēlo kastveida un nogriežņa procentiļu diagrammas (boxplot) 
#Bruņērču indivīdu skaita attēlojums audzētavu augsnes paraugu veidos

ggplot(audz1, aes(x = fveids, y = Brunercu_ind, fill = fveids)) + 
  geom_boxplot() + 
  # Sadala pa audzētavām A, B, C
  facet_wrap(~fvieta) + 
  scale_fill_manual(values = c("grey80", "grey80", "grey80")) + 
  theme_bw() + 
  theme(
    # Pielāgo uzrakstu izkārtojumu
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(1.9), color = "black"),
    axis.text.y = element_text(size = rel(1.9), color = "black"),
    axis.title = element_text(size = rel(1.9), face = "bold"),
    strip.text = element_text(size = rel(1.9), face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) + 
  labs(x = "Parauga veids", y = "Bruņērču indivīdu skaits audzētavu paraugos")



# Bruņērču sugu skaita attēlošana audzētavu augsnes paraugos

# Attēlo kastveida un nogriežņa procentiļu diagrammas (boxplot) 

ggplot(audz1, aes(x = fveids, y = Brunercu_susk, fill = fveids)) + 
  geom_boxplot() + 
  # Sadala pa audzētavām A, B, C
  facet_wrap(~fvieta) + 
  scale_fill_manual(values = c("grey80", "grey80", "grey80")) + 
  scale_y_continuous(limits = c(0, 11)) + 
  theme_bw() + 
  theme(
    # Pielāgo uzrakstu izkārtojumu
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(1.9), color = "black"),
    axis.text.y = element_text(size = rel(1.9), color = "black"),
    axis.title = element_text(size = rel(1.9), face = "bold"),
    strip.text = element_text(size = rel(1.9), face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) + 
  labs(x = "Parauga veids", y = "Bruņērču sugu skaits")


###   ###   ###


library(readxl)
audz2<- read_excel("Eksstad.xlsx")

# Kategoriju mainīgo pārvēršana par faktoriem
audz2$fvieta <- factor(audz2$vieta, levels = c("A", "B", "C"))

audz2$fstadijas <- factor(audz2$E_stadijas, 
                          ordered = TRUE, 
                          levels = c("1", "2", "3"))

audz2$fveids <- factor(audz2$veids, levels = c("Ekskrementi"))

# Bruņērču indivīdu skaits katras audzētavas ekskrementu noārdīšanās stadijās. 

# Attēlo kastveida un nogriežņa procentiļu diagrammas (boxplot) 

ggplot(audz2, aes(x = fstadijas, y = Brunercu_ind, fill = fstadijas)) + 
  geom_boxplot() + 
  # Sadala pa audzētavām A, B, C
  facet_wrap(~fvieta) + 
  scale_fill_manual(values = c("grey80", "grey80", "grey80")) + 
  theme_bw() + 
  theme(
    axis.text = element_text(size = rel(1.9), color = "black"),
    axis.title = element_text(size = rel(1.9), face = "bold"),
    strip.text = element_text(size = rel(1.9), face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "none" 
  ) + 
  labs(x = "Ekskrementu stadijas", 
       y = "Bruņērču indivīdu skaits")



# Bruņērču sugu skaits katras audzētavas ekskrementu noārdīšanās stadijās. 

# Attēlo kastveida un nogriežņa procentiļu diagrammas (boxplot) 

ggplot(audz2, aes(x = fstadijas, y = Brunercu_susk, fill = fstadijas)) + 
  geom_boxplot() + 
  # Sadala pa audzētavām A, B, C
  facet_wrap(~fvieta) + 
  scale_fill_manual(values = c("grey80", "grey80", "grey80")) + 
  theme_bw() + 
  theme(
    axis.text = element_text(size = rel(1.9), color = "black"),
    axis.title = element_text(size = rel(1.9), face = "bold"),
    strip.text = element_text(size = rel(1.9), face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "none" 
  ) + 
  labs(x = "Ekskrementu stadijas", 
       y = "Bruņērču sugu skaits")


###   ###   ###

# bruņērču indivīdu skaits un sugu skaits dažādās ekskrementu noārdīšanās stadijās, audzētavu parauglaukumos 


library(readxl)
dati<- read_excel("Eksstad.xlsx")

library(ggplot2)
library(ggtext)

sugu_skaita_izmers_virs <- 6   
grafika_kompaktums <- 0.15      


# Datu sakārtošana
dati$E_stadijas <- as.factor(dati$E_stadijas)
dati$x_labels <- paste0(dati$vieta, dati$parauga_id, "\n", dati$E_stadijas)
dati$x_labels <- factor(dati$x_labels, levels = unique(dati$x_labels))

# Stabiņu diagrammu attēlošana

ggplot(dati, aes(x = x_labels, y = Brunercu_ind, fill = E_stadijas)) +
  geom_bar(stat = "identity", fill = "#4682B4", width = 0.75) +
  # Pievieno bruņērču sugu skaitu
  geom_text(aes(label = ifelse(Brunercu_susk > 0, Brunercu_susk, "")), 
            vjust = -0.5, 
            fontface = "bold", 
            size = sugu_skaita_izmers_virs) +
  # sadala katru y asi savā rindiņā
  facet_wrap(~ vieta, scales = "free", ncol = 1) +
  scale_y_continuous(
    limits = c(0, 110), 
    breaks = seq(0, 100, by = 20),
    expand = c(0, 0) 
  ) +
  theme_minimal() +
  labs(x = NULL, y = "Bruņērču ndivīdu skaits") +
  theme(
    # Noformē tekstu
    axis.text.x = element_markdown(size = 14, color = "black", lineheight = 1.2, margin = margin(t = 10)),
    axis.text.y = element_text(size = 20, face = "bold", color = "black"),
    axis.title.y = element_text(size = 10, face = "bold"),
    axis.line = element_line(color = "black", size = 1), 
    panel.grid.major = element_line(color = "lightgrey", size = 0.2), 
    strip.text = element_blank(),
    strip.background = element_blank(),
    panel.spacing = unit(1.5, "lines"),
    panel.grid.minor = element_blank(), 
    aspect.ratio = grafika_kompaktums,
    legend.position = "none"
  )





