
#Nepieciešamās pakotnes:

if(!require(readxl)) install.packages("readxl")
if(!require(readxl)) install.packages("dplyr")
if(!require(readxl)) install.packages("gllvm")
if(!require(readxl)) install.packages("MuMIn")
if(!require(readxl)) install.packages("lattice")
if(!require(readxl)) install.packages("sjPlot")


# Modeļi ar ekskrementu noārdīšanās pakāpes un relataīvā mitruma saistību ar bruņērču sugām

library(readxl)
sugass <- read_excel("e_sugas.xlsx")
faktorii <- read_excel("efak.xlsx")
library(dplyr)

# Savieno kopīgās failu daļas
colnames(sugass)[1:3] <- c("vieta", "veids", "parauga_id")

colnames(faktorii)[1:3] <- c("vieta", "veids", "parauga_id")

visi_dati <- left_join(sugass, faktorii, by = c("vieta", "veids", "parauga_id"))

# Bruņērču sugu matrica - atbildes mainīgais ir skaits, katrai bruņērču sugai 
Y <- as.matrix(visi_dati[, 4:13])

# Augsnes faktori - neatkarīgie mainīgie ir pH, organiskās vielas saturs un relatīvais mitrums. 
X <- scale(visi_dati[, 14:16])

visi_dati$unikals_id <- paste0(visi_dati$vieta, visi_dati$veids, visi_dati$parauga_id)


# Hierarhijas veidošana 
dr_matrica <- visi_dati[, c("vieta", "veids", "parauga_id")]

dr_matrica <- data.frame(
  audzetava = as.factor(visi_dati$vieta),
  laukums = as.factor(visi_dati$veids),
  id = as.factor(visi_dati$parauga_id)
)


library(gllvm)
library(MuMIn)


X_cat <- data.frame(
  Rel_mitrums = scale(visi_dati$Rel_mitrums),
  E_stadijas = as.factor(visi_dati$E_stadijas)
)


# Logaritmēta paraugu sausā masa 
off_val_masa <- log(visi_dati$sausa_masa)

#

model0 <- gllvm(y = Y, 
                  X = X_cat,
                  family = "tweedie",
                  optimizer = "nlminb",
                  num.lv = 0,
                  row.eff = ~(1 | audzetava/laukums/id),
                  studyDesign = dr_matrica,
                  formula = ~ E_stadijas + Rel_mitrums,
                  offset = off_val_masa,
                  seed = 123,       
                  n.init = 3)
summary(model0)
AICc(model0)
# nekonverģēja uz ticamu risinājumu ekskrmenetu stadiju faktora dēļ


# Noņem optimizer
model1 <- gllvm(y = Y, 
                  X = X_cat,
                  family = "tweedie",
                  num.lv = 0,
                  row.eff = ~(1 | audzetava/laukums/id),
                  studyDesign = dr_matrica,
                  formula = ~ E_stadijas + Rel_mitrums,
                  offset = off_val_masa,
                  seed = 123,       
                  n.init = 3)
summary(model1)
# Lielas standartkļūdas (aizdomas, ka nekonverģēja uz ticamu risinājumu ekskrementu stadiju faktora dēļ)

AICc(model1)

# AICc 543.1045



model2 <- gllvm(y = Y, 
                X = X_cat,
                family = "tweedie",
                num.lv = 0,
                row.eff = ~(1 | audzetava/laukums),
                studyDesign = dr_matrica,
                formula = ~ E_stadijas + Rel_mitrums,
                offset = off_val_masa,
                seed = 123,       
                n.init = 3)
summary(model2)
# Lielas standartkļūdas (aizdomas, ka nekonverģēja uz ticamu risinājumu ekskrementu stadiju faktora dēļ)

AICc(model2, model1, model0)

#        df     AICc
#model2 52 595.9145
#model1 53 543.1045
#model0 53 543.0875


# Veidoti modeļi tikai ekskrementu relatīvā mitruma saistību ar bruņērcēm.


library(readxl)
library(dplyr)
library(gllvm)
library(MuMIn)

# 1. Ielādē jauno failu "efak2.xlsx"
sugass <- read_excel("e_sugas.xlsx")
faktorii <- read_excel("efak2.xlsx") # <- ŠEIT NOMAINĪTS FAILS

# Savieno kopīgās failu daļas
colnames(sugass)[1:3] <- c("vieta", "veids", "parauga_id")
colnames(faktorii)[1:3] <- c("vieta", "veids", "parauga_id")

visi_dati <- left_join(sugass, faktorii, by = c("vieta", "veids", "parauga_id"))

# Bruņērču sugu matrica (sugas joprojām ir no 4 līdz 13 kolonnai)
Y <- as.matrix(visi_dati[, 4:13])

# 2. Augsnes faktoru matrica X
# UZMANĪBU: Tā kā "E_stadijas" ir izņemtas, pārbaudi, vai pH, Organika un Rel_mitrums 
# joprojām atrodas kolonnās 14:16. Drošāk ir atlasīt pēc nosaukumiem, nevis numuriem:
X <- scale(visi_dati[, c("Rel_mitrums")]) 

visi_dati$unikals_id <- paste0(visi_dati$vieta, visi_dati$veids, visi_dati$parauga_id)

# Hierarhijas veidošana 
dr_matrica <- data.frame(
  audzetava = as.factor(visi_dati$vieta),
  laukums = as.factor(visi_dati$veids),
  id = as.factor(visi_dati$parauga_id)
)

# 3. Vides mainīgo objekts modelim (bez E_stadijas)
# Tā kā Tev paliek tikai relatīvais mitrums, izveidojam tīru data.frame tikai ar to
X_cat <- data.frame(
  Rel_mitrums = scale(visi_dati$Rel_mitrums)
)

# Logaritmēta paraugu sausā masa 
off_val_masa <- log(visi_dati$sausa_masa)


m1 <- gllvm(y = Y, 
                X = X_cat,
                family = "tweedie",
                optimizer = "nlminb", 
                num.lv = 1,
                row.eff = ~(1 | audzetava/laukums/id),
                studyDesign = dr_matrica,
                formula = ~ Rel_mitrums,
                offset = off_val_masa,
                seed = 123,       
                n.init = 3)
summary(m1)
#Nekonverģē

AICc(m1)


m2 <- gllvm(y = Y, 
                X = X_cat,
                family = "tweedie",
                num.lv = 0,
                row.eff = ~(1 | audzetava/laukums/id),
                studyDesign = dr_matrica,
                formula = ~ Rel_mitrums,
                offset = off_val_masa,
                seed = 123,       
                n.init = 3)
summary(m2)
# Nav lielas standartnovirzes, kas ir labs rādītājs

AICc(m2)
# 515.7313



m3 <- gllvm(y = Y, 
            X = X_cat,
            family = "tweedie",
            optimizer = "nlminb", 
            num.lv = 0,
            row.eff = ~(1 | audzetava/laukums/id),
            studyDesign = dr_matrica,
            formula = ~ Rel_mitrums,
            offset = off_val_masa,
            seed = 123,       
            n.init = 3)
summary(m3)
#Nekonverģē

AICc(m3)


# ar negative.binomial saimi

m4 <- gllvm(y = Y, 
            X = X_cat,
            family = "negative.binomial",
            num.lv = 0,
            row.eff = ~(1 | audzetava/laukums/id),
            studyDesign = dr_matrica,
            formula = ~ Rel_mitrums,
            offset = off_val_masa,
            seed = 123,       
            n.init = 3)
summary(m4)
# Nav atbilstošs šādu datu analizēšanai, jāpaliek pie tweedie saimes
AICc(m4)
#557.5152


# Labākais modelis ir m2


m2 <- gllvm(y = Y, # bruņērču sugas
            X = X_cat, # ekskrementu faktori
            family = "tweedie", # varbūtību sadalījumu saime
            num.lv = 0, # nulle latento mainīgo
            row.eff = ~(1 | audzetava/laukums/id), # hierarhikski nejaušie efekti funkcijā
            studyDesign = dr_matrica, # pētījuma dizaina definēšana - paraugu ievākšana konkrētājā vietā
            formula = ~ Rel_mitrums, # ekskrementu faktori
            offset = off_val_masa, # sausā masa attiecīgi standartizēta
            seed = 123,       # nodrošina modeļa atkārtojamību
            n.init = 3) # 5 neatkarīgi risinājumi, lai modelis nonāktu pie gala risinājuma, kas ir precīzs
summary(m2)


summ <- summary(m2)


# p- vērtību un z - vērtību atlasīšana no modeļa rezultātiem un matricas izveide
z_vals <- m2$params$X / m2$sd$X
p_values_raw <- 2 * (1 - pnorm(abs(z_vals)))

dim(p_values_raw) 

#veido statistiski būtisko zvaigznīšu izveidi matricā
p_mat_transposed <- t(p_values_raw)


sig_stars <- apply(p_mat_transposed, c(1,2), function(p) {
  if(is.na(p)) return("")
  if(p <= 0.001) return("***")
  if(p <= 0.01) return("**")
  if(p <= 0.05) return("*")
  return("")
})



library(lattice)

# Veido siltumkarti (heatmap) labākajam modelim

env_coefs <- t(m2$params$X)
colort <- colorRampPalette(c("#D81B60", "white", "#004D40"))
a <- max(abs(env_coefs), na.rm = TRUE)

plot.m2 <- levelplot(as.matrix(env_coefs), 
                      main = list(label = "Ekskrementu relatīvā mitruma ietekme uz bruņērču sugām", cex = 1.8),
                      
                      xlab = list(label = " ", cex = 1.5), 
                      ylab = list(label = " ", cex = 1.5), 
                      
                      col.regions = colort(100), 
                      at = seq(-a, a, length = 100),
                      
                      scales = list(
                        x = list(rot = 45, cex = 1.9, font = 2), 
                        y = list(cex = 1.5, font = 3)            
                      ),
                      
                      colorkey = list(labels = list(cex = 1.2)),
                      
                      panel = function(x, y, z, ...) {
                        panel.levelplot(x, y, z, ...)
                        
                        # Statistiski būtisko p - vērtību zvaigznītes 
            
                        panel.text(x, y, labels = as.vector(sig_stars), 
                                   cex = 1.5, font = 2, col = "black")
                      })

print(plot.m2)


# Rezultātu attēlošana tabulā

# koeficienti un standartkļūdas
est <- m2$params$X
se <- m2$sd$X

z_vals <- est / se
p_vals <- 2 * (1 - pnorm(abs(z_vals)))
lci <- est - 1.96 * se
uci <- est + 1.96 * se

coef_table <- data.frame(
  Mainīgais = "Rel_mitrums",
  Suga = rownames(est),
  Estimate = as.numeric(est),
  Std_Error = as.numeric(se),
  z_value = as.numeric(z_vals),
  p_value = as.numeric(p_vals), 
  LCI_95 = as.numeric(lci),
  UCI_95 = as.numeric(uci),
  stringsAsFactors = FALSE
)

coef_table$p_value_text <- format.pval(coef_table$p_value, eps = 0.001, digits = 3)

cols_to_round <- c("Estimate", "Std_Error", "z_value", "LCI_95", "UCI_95")
coef_table[cols_to_round] <- round(coef_table[cols_to_round], 4)

coef_table$Signif <- cut(coef_table$p_value, 
                         breaks=c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), 
                         labels=c("***", "**", "*", ".", " "))


is_significant <- coef_table$p_value <= 0.05

coef_table$p_value_text[is_significant] <- paste0("<b>", coef_table$p_value_text[is_significant], "</b>")

coef_table$p_value <- coef_table$p_value_text
coef_table$p_value_text <- NULL 

# Vizualizācija
library(sjPlot)
tab_df(coef_table, 
       title = "GLLVM modeļa koeficientu tabula (Relatīvais mitrums)",
       show.rownames = FALSE,
       alternate.row.colors = TRUE)
