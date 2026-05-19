
#Nepieciešamās pakotnes:

if(!require(readxl)) install.packages("readxl")
if(!require(readxl)) install.packages("dplyr")
if(!require(readxl)) install.packages("gllvm")
if(!require(readxl)) install.packages("MuMIn")
if(!require(readxl)) install.packages("lattice")
if(!require(readxl)) install.packages("sjPlot")
if(!require(readxl)) install.packages("corrplot")
if(!require(readxl)) install.packages("tidyr")
if(!require(readxl)) install.packages("flextable")
if(!require(readxl)) install.packages("RColorBrewer")



# Modeļu veidošana, AICc vērtību atlasīšana, līdz labākā modeļa izvēlei


library(readxl)
sugas1 <- read_excel("iznemtas.xlsx") # iekļauj sugas, kas kopā visos paraugos reģistrētas vismaz divas reizes 
faktori1 <- read_excel("augsn_faktori.xlsx")

library(dplyr)
# Savieno kopīgās failu daļas
colnames(sugas1)[1:3] <- c("vieta", "veids", "parauga_id")

colnames(faktori1)[1:3] <- c("vieta", "veids", "parauga_id")

visi_dati <- left_join(sugas1, faktori1, by = c("vieta", "veids", "parauga_id"))

# Bruņērču sugu matrica - atbildes mainīgais ir skaits, katrai bruņērču sugai 
Y <- as.matrix(visi_dati[, 4:23])

# Augsnes faktori - neatkarīgie mainīgie ir pH, organiskās vielas saturs un relatīvais mitrums. 
X <- scale(visi_dati[, 24:27])

# Hierarhijas veidošana
visi_dati$unikals_id <- paste0(visi_dati$vieta, visi_dati$veids, visi_dati$parauga_id)


# Matrica izveido unikālo paraugu ID
dr_matrica <- data.frame(
  audzetava = as.factor(visi_dati$vieta),
  laukums = as.factor(visi_dati$veids),
  id = as.factor(visi_dati$unikals_id) 
)


library(gllvm)

vides_faktori <- visi_dati[, c("pH", "organika", "Rel_mitrums")]
X_scaled <- scale(vides_faktori)

# Logaritmēta paraugu sausā masa 
off_val_masa <- log(visi_dati$sausa_masa)



# Veido modeļus ar "negative.binomial" saimi.

modelis1 <- gllvm(y = Y, 
                      X = X_scaled,
                      family = "negative.binomial",
                      num.lv = 1,
                      row.eff = ~(1 | audzetava/laukums/id),
                      studyDesign = dr_matrica,
                      formula = ~ pH + organika + Rel_mitrums, 
                      offset = off_val_masa,
                      seed = 123,       
                      n.init = 5)       

summary(modelis1)

library(MuMIn)
# Modeļa AICc vērtības
AICc(modelis1)
# AICc 2772.677




modelis2 <- gllvm(y = Y, 
                  X = X_scaled,
                  family = "negative.binomial",
                  optimizer = "nlminb", #pievieno optimizer labākā risinājuma meklēšanai
                  num.lv = 2, #palielināja neizmērīto faktoru ietekmi
                  row.eff = ~(1 | audzetava/laukums/id),
                  studyDesign = dr_matrica,
                  formula = ~ pH + organika + Rel_mitrums, 
                  offset = off_val_masa,
                  seed = 123,       
                  n.init = 5) 

summary(modelis2)


AICc(modelis2, modelis1)
#          df   AICc
#modelis2 142 2814.720
#modelis1 123 2772.677

modelis3 <- gllvm(y = Y, 
                  X = X_scaled,
                  family = "negative.binomial",
                  optimizer = "nlminb",
                  num.lv = 1, # samazina 
                  row.eff = ~(1 | audzetava), # atstāj tikai audzētavu
                  studyDesign = dr_matrica,
                  formula = ~ pH + organika + Rel_mitrums, 
                  offset = off_val_masa,
                  seed = 123,       
                  n.init = 5) 

summary(modelis3)

AICc(modelis3, modelis2, modelis1)


#         df     AICc
#modelis3 121 2779.324
#modelis2 142 2814.720
#modelis1 123 2772.677


modelis4 <- gllvm(y = Y, 
                  X = X_scaled,
                  family = "negative.binomial",
                  optimizer = "nlminb",
                  num.lv = 1, # samazina 
                  row.eff = ~(1 | audzetava/ laukums), # atstāj tikai audzētavu un parauglaukumu
                  studyDesign = dr_matrica,
                  formula = ~ pH + organika + Rel_mitrums, 
                  offset = off_val_masa,
                  seed = 123,       
                  n.init = 5) 

summary(modelis4)
# Saime "negative.binomial" nav piemērota šiem datiem, jāizvēlas tweedie

AICc(modelis3, modelis2, modelis1, modelis4)

#        df     AICc
#modelis3 121 2779.324
#modelis2 142 2814.720
#modelis1 123 2772.677
#modelis4 122 2770.483

# Saime "negative.binomial" nav piemērota šiem datiem, nespēj tikt galā ar nuļļu piesātinājumu, jāizvēlas tweedie



# Nomaina uz varbūtību sadalījumu saime "tweedie"


modelis <- gllvm(y = Y, 
                 X = X_scaled,
                 family = "tweedie",
                 optimizer = "nlminb", 
                 num.lv = 1, 
                 row.eff = ~(1 | audzetava/laukums/id), 
                 studyDesign = dr_matrica,
                 formula = ~ pH + organika + Rel_mitrums,
                 offset = off_val_masa,
                 seed = 123,       
                 n.init = 5)     

summary(modelis)
AICc(modelis)



modelis01 <- gllvm(y = Y, 
                   X = X_scaled,
                   family = "tweedie",
                   optimizer = "nlminb", 
                   num.lv = 2, # lai kontrolētu neizmērīto faktoru ietekmi
                   row.eff = ~(1 | audzetava/laukums/id), # hierarhikski nejaušie efekti funkcijā
                   studyDesign = dr_matrica,
                   formula = ~ pH + organika + Rel_mitrums,
                   offset = off_val_masa,
                   seed = 123,       
                   n.init = 5)     

#modeļa rezultātu apskate
summary(modelis01)


# Modeļa AICc vērtības
AICc(modelis01)
# 2719.389


AICc(modelis, modelis01)

#         df     AICc
#modelis   123 2740.162
#modelis01 142 2719.389 labākais līdz šim




modelis02 <- gllvm(y = Y, 
                   X = X_scaled,
                   family = "tweedie",
                   optimizer = "nlminb", 
                   num.lv = 3, # palielina
                   row.eff = ~(1 | audzetava/laukums/id), 
                   studyDesign = dr_matrica,
                   formula = ~ pH + organika + Rel_mitrums,
                   offset = off_val_masa,
                   seed = 123,       
                   n.init = 5)     

#Nespēj konverģēt, 
summary(modelis02)
#nekonverģē


AICc(modelis, modelis01, modelis02)

#2740.162 konverģē, AICc labs, bet ne zemākais starp modeļiem
#2719.389  konverģē, ir labākais AICc
#2707.114 nekonverģē



#Kā labāko modeli izvēlas modeli -> modelis01



#ar ekskrementu stadijām pameģina kā x matricas pazīmi

library(readxl)
sugas1 <- read_excel("visi.xlsx")
faktori1 <- read_excel("visssi.xlsx")

# Doma bija salikt kopā failus un pateikt, ka  vieta, veids un id ir viens un tas pats abos failos
colnames(sugas1)[1:3] <- c("vieta", "veids", "parauga_id")

colnames(faktori1)[1:3] <- c("vieta", "veids", "parauga_id")

library(dplyr)
visi_dati <- left_join(sugas1, faktori1, by = c("vieta", "veids", "parauga_id"))

#sugas
Y <- as.matrix(visi_dati[, 4:20])

#faktori
X <- scale(visi_dati[, 21:25])

# Hierarhija 
dr_matrica <- visi_dati[, c("vieta", "veids", "parauga_id")]

dr_matrica <- data.frame(
  audzetava = as.factor(visi_dati$vieta),
  laukums = as.factor(visi_dati$veids),
  id = as.factor(visi_dati$parauga_id)
)

library(gllvm)
library(MuMIn)

# Pievieno vai definē stadiju kā faktoru
visi_dati$E_stadijas <- as.factor(visi_dati$E_stadijas) 

# Atjauno vides faktoru matricu, iekļaujot stadiju
vides_faktori <- visi_dati[, c("pH", "organika", "Rel_mitrums", "E_stadijas", "veids")]
X_scaled <- data.frame(scale(vides_faktori[, 1:3]), 
                       E_stadijas = vides_faktori$E_stadijas,
                       veids = as.factor(vides_faktori$veids))


#vides_faktori <- visi_dati[, c("pH", "organika", "Rel_mitrums")]
#X_scaled <- scale(vides_faktori)

off_val_masa <- log(visi_dati$sausa_masa)


mod1 <- gllvm(y = Y, 
                       X = X_scaled, 
                       family = "tweedie",
                       optimizer = "nlminb",
                       num.lv = 1, 
                       row.eff = ~(1 | audzetava), # Vienkāršots, bez smalkākas hierarhijas
                       studyDesign = dr_matrica,
                       formula = ~ pH + organika + Rel_mitrums + E_stadijas, # Bez mijiedarbības 
                       offset = off_val_masa,
                       seed = 123,       
                       n.init = 3) 
summary(mod1)
# Kopumā vērojamas ielas standartkļūdas, aizdoma, ka modelis nav stabils, rodas kļūdas sugu sastopamības skaidrošanā
AICc(mod1)
#3247.048 



mod2 <- gllvm(y = Y, 
                       X = X_scaled, 
                       family = "tweedie",
                       optimizer = "nlminb",
                       num.lv = 0, #nulle
                       row.eff = ~(1 | audzetava), 
                       studyDesign = dr_matrica,
                       formula = ~ pH + organika + Rel_mitrums + E_stadijas, 
                       offset = off_val_masa,
                       seed = 123,       
                       n.init = 3) 
summary(mod2)

AICc(mod2, mod1)


#      df     AICc
#mod2 137 3546.580 lielāks
#mod1 154 3247.048 

mod3 <- gllvm(y = Y, 
              X = X_scaled, 
              family = "tweedie",
              optimizer = "nlminb",
              num.lv = 0, 
              row.eff = ~(1 | audzetava/ laukums), #hierarhija 
              studyDesign = dr_matrica,
              formula = ~ pH + organika + Rel_mitrums + E_stadijas, 
              offset = off_val_masa,
              seed = 123,       
              n.init = 3) 

summary(mod3)

# Nekonverģē, ja pievieno smalkāku hierarhiju

mod4 <- gllvm(y = Y, 
              X = X_scaled, 
              family = "tweedie",
              optimizer = "nlminb",
              num.lv = 2, 
              row.eff = ~(1 | audzetava), # Vienkāršots, bez smalkākas hierarhijas
              studyDesign = dr_matrica,
              formula = ~ pH + organika + Rel_mitrums + E_stadijas,
              offset = off_val_masa,
              seed = 123,       
              n.init = 3) 
summary(mod4)

AICc(mod4)

#3196.267 




mod5 <- gllvm(y = Y, 
              X = X_scaled, 
              family = "tweedie",
              optimizer = "nlminb",
              num.lv = 3, 
              row.eff = ~(1 | audzetava), 
              studyDesign = dr_matrica,
              formula = ~ pH + organika + Rel_mitrums + E_stadijas,
              offset = off_val_masa,
              seed = 123,       
              n.init = 3) 
summary(mod5)
# Modelis nespēj tikt galā ar mazo sastopamību datos
AICc(mod5)
#3173.658


mod6 <- gllvm(y = Y, 
              X = X_scaled, 
              family = "tweedie",
              optimizer = "nlminb",
              num.lv = 2, 
              row.eff = ~(1 | audzetava/ laukums/ id), 
              studyDesign = dr_matrica,
              formula = ~ pH + organika + Rel_mitrums + E_stadijas,
              offset = off_val_masa,
              seed = 123,       
              n.init = 3) 
summary(mod6)
# Lielas tandartkļūdas, mazs paraugu apjoms, kas ir izaicinājums modelim
AICc(mod6)
#3205.908

# Kopumā neviens no šiem, ar E_stadijas iekļauto faktoru, modeļiem neuzrāda bez
#kļūdām (lielas standartnovirzes) rezultātus summary()
# Tāpēc modeļi nav tik uzticami


# Izvēlās modeli, kas ir labākais modelis01 (skat. augstāk tā atlasi), bez E_stadijas kā faktora, atkal ielasa datus, ja nepieciešams



library(readxl)
sugas1 <- read_excel("iznemtas.xlsx") # iekļauj sugas, kas kopā visos paraugos reģistrētas vismaz divas reizes 
faktori1 <- read_excel("augsn_faktori.xlsx")

library(dplyr)
# Savieno kopīgās failu daļas
colnames(sugas1)[1:3] <- c("vieta", "veids", "parauga_id")

colnames(faktori1)[1:3] <- c("vieta", "veids", "parauga_id")

visi_dati <- left_join(sugas1, faktori1, by = c("vieta", "veids", "parauga_id"))

# Bruņērču sugu matrica - atbildes mainīgais ir skaits, katrai bruņērču sugai 
Y <- as.matrix(visi_dati[, 4:23])

# Augsnes faktori - neatkarīgie mainīgie ir pH, organiskās vielas saturs un relatīvais mitrums. 
X <- scale(visi_dati[, 24:27])

# Hierarhijas veidošana
visi_dati$unikals_id <- paste0(visi_dati$vieta, visi_dati$veids, visi_dati$parauga_id)


# Matrica izveido unikālo paraugu ID
dr_matrica <- data.frame(
  audzetava = as.factor(visi_dati$vieta),
  laukums = as.factor(visi_dati$veids),
  id = as.factor(visi_dati$unikals_id) 
)


library(gllvm)

vides_faktori <- visi_dati[, c("pH", "organika", "Rel_mitrums")]
X_scaled <- scale(vides_faktori)

# Logaritmēta paraugu sausā masa 
off_val_masa <- log(visi_dati$sausa_masa)


modelis01 <- gllvm(y = Y, # bruņērču sugu matrica
                   X = X_scaled, # augsnes faktoru matrica
                   family = "tweedie", # varbūtību sadalījumu saime 
                   optimizer = "nlminb", # modeļa konverģēšanas (modeļa risinājuma atrašanas) ātrākai darbībai 
                   num.lv = 2, # latentie mainīgie, lai kontrolētu neizmērīto faktoru ietekmi
                   row.eff = ~(1 | audzetava/laukums/id), # hierarhikski nejaušie efekti funkcijā
                   studyDesign = dr_matrica, # pētījuma dizaina definēšana - paraugu ievākšana konkrētājā vietā
                   formula = ~ pH + organika + Rel_mitrums, # augsnes faktori
                   offset = off_val_masa, # sausā masa attiecīgi standartizēta
                   seed = 123,       # nodrošina modeļa atkārtojamību
                   n.init = 5)  # 5 neatkarīgi risinājumi, lai modelis nonāktu pie gala risinājuma, kas ir precīzs


#modeļa rezultātu apskate
summary(modelis01)

# Vizuāla modeļa diagnostika pēc nepieciešamības
#par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
#plot(modelis01, var.colors = 1)


# Modeļa AICc vērtības
AICc(modelis01)
# 2719.389


summ <- summary(modelis01)

# p- vērtību un z - vērtību atlasīšana no modeļa rezultātiem un matricas izveide
z_vertiba <- modelis01$params$X / modelis01$sd$X
p_vertiba <- 2 * (1 - pnorm(abs(z_vertiba)))

dim(p_vertiba) 

#veido statistiski būtisko zvaigznīšu izveidi matricā
p_mat <- t(p_vertiba)

sig_stars <- apply(p_mat, c(1,2), function(p) {
  if(is.na(p)) return("")
  if(p <= 0.001) return("***")
  if(p <= 0.01) return("**")
  if(p <= 0.05) return("*")
  return("")
})


library(lattice)

# Veido siltumkarti (heatmap) labākajam modelim

env_coefs <- t(modelis01$params$X)
colort <- colorRampPalette(c("#D81B60", "white", "#004D40"))
a <- max(abs(env_coefs), na.rm = TRUE)

plot.env <- levelplot(as.matrix(env_coefs), 
                      main = list(label = "Augsnes faktoru ietekme uz bruņērču sugām", cex = 1.8),
                      
                      xlab = list(label = "Faktori augsnē", cex = 1.5), 
                      ylab = list(label = "Bruņērču sugas", cex = 1.5), 
                      
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

print(plot.env)



# Rezultātu attēlošana tabulā


library(dplyr)
library(tidyr)
library(flextable)

est <- modelis01$params$X
se <- modelis01$sd$X
z_vals <- est / se
p_vals <- 2 * (1 - pnorm(abs(z_vals)))
sugu_nosaukumi <- rownames(est)

long_data <- data.frame(
  Suga = rep(sugu_nosaukumi, 3),
  Faktors = rep(c("pH", "Organika", "Rel_mitrums"), each = nrow(est)),
  Est = as.vector(est),
  SE = as.vector(se),
  z = as.vector(z_vals),
  p = as.vector(p_vals)
)

long_data <- long_data %>%
  mutate(
    LCI = round(Est - 1.96 * SE, 2),
    UCI = round(Est + 1.96 * SE, 2),
    Est_round = round(Est, 3),
    SE_round = round(SE, 3),
    z_round = round(z, 2),
    p_txt = format.pval(p, eps = 0.001, digits = 3),
    
    Viss_Kopa_Txt = paste0(Est_round, " [", LCI, "; ", UCI, "]; SE=", SE_round, "; z=", z_round, "; p=", p_txt)
  )

wide_data <- long_data %>%
  select(Suga, Faktors, Viss_Kopa_Txt, p) %>%
  pivot_wider(
    names_from = Faktors, 
    values_from = c(Viss_Kopa_Txt, p),
    names_glue = "{Faktors}_{.value}"
  )

wide_data <- wide_data %>%
  select(Suga, pH_Viss_Kopa_Txt, pH_p, Organika_Viss_Kopa_Txt, Organika_p, Rel_mitrums_Viss_Kopa_Txt, Rel_mitrums_p)

ft <- flextable(wide_data, col_keys = c("Suga", "pH_Viss_Kopa_Txt", "Organika_Viss_Kopa_Txt", "Rel_mitrums_Viss_Kopa_Txt")) %>%
  
  set_header_labels(
    Suga = "Suga",
    pH_Viss_Kopa_Txt = "pH\n(Est [95% TI]; SE; z; p)",
    Organika_Viss_Kopa_Txt = "Organisko vielu saturs\n(Est [95% TI]; SE; z; p)",
    Rel_mitrums_Viss_Kopa_Txt = "Relatīvais mitrums\n(Est [95% TI]; SE; z; p)"
  ) %>%
  
  bold(i = ~ pH_p <= 0.05, j = "pH_Viss_Kopa_Txt", part = "body") %>%
  bold(i = ~ Organika_p <= 0.05, j = "Organika_Viss_Kopa_Txt", part = "body") %>%
  bold(i = ~ Rel_mitrums_p <= 0.05, j = "Rel_mitrums_Viss_Kopa_Txt", part = "body") %>%
  
  fontsize(size = 12, part = "all") %>% 

theme_booktabs() %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>% 
  italic(j = 1, part = "body") %>%               
  
  padding(padding.top = 4, padding.bottom = 4, padding.left = 5, padding.right = 5, part = "all") %>%
  
  width(j = 1, width = 2.4) %>%  
  width(j = 2, width = 3.8) %>%  
  width(j = 3, width = 3.8) %>%  
  width(j = 4, width = 3.8)      

ft


###   ###   ###

# Sugu savstarpējās sastopamības siltumkarte (heatmap) labākajam modelim modelis01

# Atlikumvērtību korelāciju matrica

library(corrplot)
library(RColorBrewer) 

cr <- getResidualCor(modelis01)
ord <- corrMatOrder(cr, order = "hclust") 
cr_ordered <- cr[ord, ord]

rownames(cr_ordered) <- gsub("_", " ", rownames(cr_ordered))
colnames(cr_ordered) <- gsub("_", " ", colnames(cr_ordered))

palete <- colorRampPalette(brewer.pal(11, "RdYlBu"))(100)


corrplot(cr_ordered, 
         diag = FALSE, 
         type = "lower", 
         method = "color",        
         addCoef.col = "black",   
         number.cex = 0.55,       
         col = palete,            
         cl.pos = "b",            
         cl.cex = 0.6,            
         addgrid.col = "white",   
         tl.cex = 0.6,            
         tl.col = "black",        
         tl.srt = 45,             
         title = "Bruņērču sugu atlikumu korelācijas (gllvm)",
         mar = c(3, 1, 3, 1))     







library(tidyverse)

# Izveidojam garo tabulu
gara_tabula <- cr_ordered %>%
  as.data.frame() %>%
  rownames_to_column(var = "Suga_1") %>%
  pivot_longer(cols = -Suga_1, names_to = "Suga_2", values_to = "Korelacija") %>%
  filter(Suga_1 < Suga_2) %>% 
  mutate(Korelacija = round(Korelacija, 3)) %>%
  arrange(desc(abs(Korelacija)))

# LAI UZRĀDĪTOS:
# Izvēle A: Atvērt kā smuku tabulu atsevišķā logā:
View(gara_tabula)

# Izvēle B: Parādīt pirmās 20 rindas turpat Console logā apakšā:
print(gara_tabula)
