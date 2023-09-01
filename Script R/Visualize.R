library(readr)
library(tidyr)
library(dplyr)
library(tibble)
library(ggplot2)
library(scales)
library(ggrepel)

# - PARAMETRI -
# rangeYears intervallo di anni in cui scarica i dati dei SET, parte sempre dall'anno scorso e va indietro di rangeYears
# Esempio 10 Anno corrente 2023 parte dal 2022 e torna indietro fino al 2013 (compreso)
rangeYears = 10
currentDate = Sys.Date()
currentYear = as.numeric(format(currentDate,'%Y'))
firstYear = currentYear - rangeYears - 1;
#Numero di utenti registrati su Brickset
userOnBrickset = 306659
#Soglia percentuale degli utenti che hanno manifestato la preferenza
# 5: 5 utenti su 100 hanno manifestato il desiderio di averlo o lo hanno già acquistato
threshold_perc = 5

#Importazione CSV con tutti i set
tb_sets_PP = as_tibble(read.csv("tot_tibble_sets.csv"),header = TRUE,sep = ";",dec = ".",stringsAsFactors = TRUE)

# Tabella riassuntiva raggruppata per tema, anno con le seguenti medie:
# n: numero di scatole commercializzate
# avg_pp: rapporto tra il prezzo ed il n. di pezzi
# avg_pieces: numero pezzi

tb_sets_meanThemeYear = tb_sets_PP %>%
  group_by(theme,year) %>% 
  summarise(n = n(), avg_pp = mean(as.double(pp)), avg_pieces = mean(pieces),.groups = 'drop') %>% 
  arrange(theme,year)

tb_sets_meanThemeYear

#Elenco delle linee di prodotto rincorrenti negli ultmi 10 anni

tb_recurrent_theme_sets = tb_sets_meanThemeYear %>%
  group_by(theme) %>%
  summarise(n=n()) %>%
  filter(n == rangeYears)

#DOMANDA 1 - Andamento della media del rapporto tra prezzo e numero di pezzi negli ultimi 10 anni (rangeYears)

tb_sets_statsbyYear = tb_sets_PP %>%
  group_by(year) %>% 
  summarise(n=n(),avg_pp = mean(as.double(pp)), avg_pieces = mean(pieces), avg_price=mean(retailPrice),.groups = 'drop') %>% 
  arrange(year)

ggplot(data = tb_sets_statsbyYear, mapping = aes(x = as.integer(year), y = avg_pp)) +
  geom_point() + 
  geom_smooth() +
  labs(
    title = "Grafico sulla media del rapporto prezzo/n.pezzi (pp) della scatola per anno (year)",
    y = "Media del rapporto prezzo/n.pezzi (€)",
    x = "Anno"
  ) +
  scale_x_continuous(labels = label_number(accuracy = 1), breaks = seq(firstYear, currentYear, 1)) 

#DOMANDA 1 - Andamento della media del numero di pezzi negli ultimi 10 anni (rangeYears)

ggplot(data = tb_sets_statsbyYear, mapping = aes(x = as.integer(year), y = avg_pieces)) +
  geom_point() + 
  geom_smooth() +
  labs(
    title = "Grafico sulla media del numero di pezzi (pieces) della scatola per anno (year)",
    y = "Media del n. dei pezzi in una scatola",
    x = "Anno"
  ) +
  scale_x_continuous(labels = label_number(accuracy = 1), breaks = seq(firstYear, currentYear, 1)) 

#DOMANDA 1 - Andamento della media del prezzo della scatole negli ultimi 10 anni (rangeYears)

ggplot(data = tb_sets_statsbyYear, mapping = aes(x = as.integer(year), y = avg_price)) +
  geom_point() + 
  geom_smooth()+
  labs(
    title = "Grafico sulla media del prezzo (retailPrice) della scatola per anno (year)",
    y = "Media del prezzo di una scatola (€)",
    x = "Anno"
  ) +
  scale_x_continuous(labels = label_number(accuracy = 1), breaks = seq(firstYear, currentYear, 1)) 

#DOMANDA 1 - Andamento della media del numero di scatole commercializzate negli ultimi 10 anni (rangeYears)

ggplot(data = tb_sets_statsbyYear, mapping = aes(x = as.integer(year), y = n)) +
  geom_point() + 
  geom_smooth()+
  labs(
    title = "Grafico sulla media del numero di scatole commercializzate per anno (year)",
    y = "Media numero di scatole",
    x = "Anno"
  ) +
  scale_x_continuous(labels = label_number(accuracy = 1), breaks = seq(firstYear, currentYear, 1)) 

#DOMANDA 2 - Analizziamo come il prezzo, il numero di pezzi, ed il rapporto prezzo numero di pezzi si distribuisce sulle linee di prodotto

ggplot(data = tb_sets_PP) +
  geom_boxplot(mapping = aes(x = reorder(theme, retailPrice, FUN = median), y=retailPrice))+
  geom_boxplot(data=tb_sets_PP[tb_sets_PP$theme %in% tb_recurrent_theme_sets$theme,],
               aes(x = reorder(theme, retailPrice, FUN = median), y = retailPrice),fill="#18a7f5")+
  labs(
    title = "Grafico ordinato sulla linea di prodotto in base alla media del prezzo di vendita nell'area euro",
    subtitle = "Attenzione! l'asse y del prezzo di vendita è in scala logaritmica",
    y = "Prezzo di vendita",
    x = "Linea di prodotto",
    caption = paste("Sono evidenziate le linee di prodotto rincorrenti negli ultimi",rangeYears," anni")
  ) +
  scale_y_continuous(labels = dollar_format(suffix = "€", prefix = ""), trans = "log10",breaks = c(1,2.5,5,7.5,10,15,20,30,40,60,80,120,160,240,320,480,640,960,1280))+ 
  theme(
    axis.text.x = element_text(size=10, angle=90,hjust=0.95,vjust=0.2),
    plot.subtitle=element_text(face="italic", color="blue"),
    plot.caption=element_text(size=11,face="bold", color="#18a7f5"))

ggplot(data = tb_sets_PP) +
  geom_boxplot(mapping = aes(x = reorder(theme, pieces, FUN = median),y = pieces)) + 
  geom_boxplot(data=tb_sets_PP[tb_sets_PP$theme %in% tb_recurrent_theme_sets$theme,],
               aes(x = reorder(theme, pieces, FUN = median), y = pieces),fill="#18a7f5")+
  labs(
    title = "Grafico ordinato sulla linea di prodotto in base alla media del numero di pezzi",
    subtitle = "Attenzione! l'asse y del numero di pezzi è in scala logaritmica",
    y = "Numero di pezzi",
    x = "Linea di prodotto",
    caption = paste("Sono evidenziate le linee di prodotto rincorrenti negli ultimi",rangeYears," anni")
  ) +
  scale_y_continuous(trans = "log10",breaks = c(1,10,20,40,80,160,320,640,1280,2560,5120,10240))+
  theme(
    axis.text.x = element_text(size=10, angle=90,hjust=0.95,vjust=0.2),
    plot.subtitle=element_text(face="italic", color="blue"),
    plot.caption=element_text(size=11,face="bold", color="#18a7f5"))

ggplot(data = tb_sets_PP) +
  geom_boxplot(mapping = aes(x = reorder(theme, pp, FUN = median),y = pp)) + 
  geom_boxplot(data=tb_sets_PP[tb_sets_PP$theme %in% tb_recurrent_theme_sets$theme,],
               aes(x = reorder(theme, pp, FUN = median), y = pp),fill="#18a7f5")+
  labs(
    title = "Grafico ordinato sulla linea di prodotto in base al rapporto tra prezzo e il numero di pezzi",
    subtitle = "Attenzione! l'asse y prezzo/n.pezzi è in scala logaritmica",
    y = "Rapporto tra il prezzo ed il n. di pezzi",
    x = "Linea di prodotto",
    caption = paste("Sono evidenziate le linee di prodotto rincorrenti negli ultimi",rangeYears," anni")
  ) +
  scale_y_continuous(labels = dollar_format(suffix = "€", prefix = ""), trans = "log10",breaks = c(0.1,0.2,0.4,0.8,2,5,10,20,40,80,160,320))+ 
  theme(
    axis.text.x = element_text(size=10, angle=90,hjust=0.95,vjust=0.2),
    plot.subtitle=element_text(face="italic", color="blue"),
    plot.caption=element_text(size=11,face="bold", color="#18a7f5"))

#DOMANDA 3 - Valutazione delle linee di prodotto migliore secondo il rapporto tra il prezzo ed il n. pezzi 

ggplot(data = tb_sets_PP %>% filter (! theme %in% c('Powered Up','Mindstorms','Power Functions','Duplo','Education'))) +
  geom_boxplot(mapping = aes(x = reorder(theme, pp, FUN = median),y = pp)) +
  geom_boxplot(data=tb_sets_PP[tb_sets_PP$theme %in% tb_recurrent_theme_sets$theme,]%>% filter (! theme %in% c('Powered Up','Mindstorms','Power Functions','Duplo','Education')),
               aes(x = reorder(theme, pp, FUN = median), y = pp),fill="#18a7f5")+
  labs(
    title = "Grafico ordinato sulla linea di prodotto in base al rapporto tra prezzo e il numero di pezzi.\n Al grafico è stato applicato uno zoom sull'asse y: 0.01€ - 0.50€\n Escluso le linee di prodotto: Powered Up,Mindstorms,Power Functions,Duplo,Education",
    subtitle = "Attenzione! l'asse y prezzo/n.pezzi è in scala logaritmica",
    y = "Rapporto tra il prezzo ed il n. di pezzi",
    x = "Linea di prodotto",
    caption = paste("Sono evidenziate le linee di prodotto rincorrenti negli ultimi",rangeYears," anni")
  ) +
  scale_y_continuous(labels = dollar_format(suffix = "€", prefix = ""), trans = "log10",breaks = c(0.1,0.2,0.4,0.8,2,5,10,20,40,80,160,320))+ 
  coord_cartesian(ylim = c(0.01, 0.50))+
  theme(
    axis.text.x = element_text(size=10, angle=90,hjust=0.95,vjust=0.2),
    plot.subtitle=element_text(face="italic", color="blue"),
    plot.caption=element_text(size=11,face="bold", color="#18a7f5"))



tb_MediaPPYear_reccurent_theme_sets = tb_sets_PP %>%
  filter (theme %in% tb_recurrent_theme_sets$theme ) %>%
  group_by(theme,year) %>% 
  summarise(n = n(), avg_pp = mean(as.double(pp)), .groups = 'drop')

ggplot(data = tb_MediaPPYear_reccurent_theme_sets, mapping = aes(x = year, y = avg_pp)) +
  geom_point(mapping = aes(color = theme)) + 
  geom_line(mapping = aes(color = theme),linewidth=1.2)+
  geom_label_repel(aes(label = paste(theme,":",format(round(avg_pp, 3), nsmall = 3),"€")),
                   data = tb_MediaPPYear_reccurent_theme_sets %>% filter(
                     (year %in% c(firstYear +1,firstYear +(rangeYears / 2),currentYear-1) & avg_pp > 0.18) |
                    (year %in% c(firstYear +1,firstYear +(rangeYears / 2),currentYear-1) & avg_pp < 0.10)),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  geom_smooth() +
  labs(
    title = "Grafico sulla media del rapporto tra il prezzo ed il n.pezzi suddiviso per anno, solo delle linee di prodotto ricorrenti.\n Le etichette <linea di prodotto> : <rapporto in €> sono specifiche degli anni 2013, 2017, 2022 con la soglia > 0.18€ o < 0.10€.",
    subtitle = "Attenzione! l'asse y il rapporto tra il prezzo ed il n.pezzi è in scala logaritmica",
    y = "Rapporto tra il prezzo ed il n. di pezzi",
    x = "Anno"
  ) +
  scale_x_continuous(labels = label_number(accuracy = 1), breaks = seq(firstYear, currentYear, 1)) +
  scale_y_continuous(labels = dollar_format(suffix = "€", prefix = ""), trans = "log10",breaks = c(0.05,0.1,0.2,0.4,0.8,2)) +
  theme(
    legend.position="bottom",
    plot.subtitle=element_text(face="italic", color="blue"))

#DOMANDA 4 - Analizziamo le mie linee di prodotto preferite
#Technic
data_label = tb_sets_PP %>%
  filter(theme=='Technic') %>%
  group_by(year) %>%
  filter(row_number(desc(pieces)) == 1)

ggplot(data=tb_sets_PP %>% filter(theme=='Technic'), mapping=aes(x=retailPrice, y=pieces, color = as.character(year))) +
  geom_point() +
  geom_smooth(mapping = aes(colour=as.character(year)), se = FALSE)+
  geom_label_repel(aes(label = paste(year,":",pieces,"(",number,")")),
                   data = data_label,
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(
    title = "Grafico sul andamento del prezzo ed il n.pezzi per la linea di prodotto Technic",
    subtitle = "Le etichette descrivono la scatola che in quell'anno ha il maggior numero di pezzi.\n <anno> : <numero di pezzi> (<numero della scatola)",
    y = "N.pezzi",
    x = "Prezzo",
    colour = "Raggruppamento\n per Anno (year)"
  ) +
  theme(legend.position="bottom")

ggplot(data=tb_sets_PP %>% filter(theme=='Technic' & retailPrice > 50 & retailPrice < 250), mapping=aes(x=retailPrice, y=pieces, color = as.character(year))) +
  geom_point() +
  geom_label_repel(aes(label = paste(year)),
                   box.padding   = 0.2, 
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   max.overlaps = Inf )+
  labs(
    title = "Grafico sul andamento del prezzo ed il n.pezzi per la linea di prodotto Technic",
    subtitle = "Filtro sul prezzo tra i 50€ ed i 250€",
    y = "N.pezzi",
    x = "Prezzo",
    colour = "Raggruppamento\n per Anno (year)"
  )+
  theme(legend.position="bottom")

#Creator

data_label = tb_sets_PP %>%
  filter(theme=='Creator') %>%
  group_by(year) %>%
  filter(row_number(desc(pieces)) == 1)

ggplot(data=tb_sets_PP %>% filter(theme=='Creator'), mapping=aes(x=retailPrice, y=pieces, color = as.character(year))) +
  geom_point() +
  geom_smooth(mapping = aes(colour=as.character(year)), se = FALSE)+
  geom_label_repel(aes(label = paste(year,":",pieces,"(",number,")")),
                   data = data_label,
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(
    title = "Grafico sul andamento del prezzo ed il n. di pezzi per la linea di prodotto Creator",
    subtitle = "Le etichette descrivono la scatola che in quell'anno ha il maggior numero di pezzi.\n <anno> : <numero di pezzi> (<numero della scatola)",
    y = "N.pezzi",
    x = "Prezzo",
    colour = "Raggruppamento\n per Anno (year)"
  )+
  theme(legend.position="bottom")


#DOMANDA 5 - Modello di classificazione sulla preferenza data dal utente su Brickset

tb_sets_classification = tb_sets_PP %>%
  mutate(preferences = (ifelse(is.na(collections.wantedBy),0,collections.wantedBy) + ifelse(is.na(collections.ownedBy),0,collections.ownedBy))) %>%
  mutate(prefered = (ifelse((preferences > 20000),TRUE,FALSE)))

model_pp = lm(formula= preferences ~ pp, data = tb_sets_classification)
summary(model_pp)
model_retailPrice = lm(formula= preferences ~ retailPrice, data = tb_sets_classification)
summary(model_retailPrice)
model_pieces = lm(formula= preferences ~ retailPrice, data = tb_sets_classification)
summary(model_pieces)

ggplot(data=tb_sets_classification) +
  geom_histogram(mapping=aes(x=preferences), binwidth = 10000)

# Plot class distribution
class.dist = table(tb_sets_classification$prefered)
barplot(class.dist)
print("Absolute frequencies")
print(class.dist)
print("Relative frequencies")
print(class.dist/sum(class.dist))

df2 = subset(as.data.frame(tb_sets_classification), select = -c(X, number, name, subtheme, category,packagingType, ageRange, collections.wantedBy, collections.ownedBy, preferences))

df2$theme <- as.factor(df2$theme)
df2$themeGroup <- as.factor(df2$themeGroup)
df2$availability <- as.factor(df2$availability)

library(caTools)

#make this example reproducible
set.seed(17)

#use 70% of dataset as training set and 30% as test set
split <- sample.split(df2, SplitRatio = 0.7)
train_cl <- subset(df2, split == "TRUE")
test_cl <- subset(df2, split == "FALSE")

library(rpart)
library(cluster)
library(rpart.plot)

# creation of an object 'model' using rpart function
decision.tree = rpart(prefered ~ ., data = train_cl, method = 'class')

split.fun <- function(x, labs, digits, varlen, faclen)
{
  # replace commas with spaces (needed for strwrap)
  labs <- gsub(",", ", ", labs)
  for(i in 1:length(labs)) {
    # split labs[i] into multiple lines
    labs[i] <- paste(strwrap(labs[i], width=80), collapse="\n")
  }
  labs
}

prp(decision.tree, faclen = 0, clip.facs = TRUE, split.fun=split.fun)


library(lattice)
library(caret)

dt_pred <- predict(decision.tree, test_cl, type = "class")

dt_pred

# Confusion Matrix
dt_cm <- table(test_cl$prefered, dt_pred)
dt_cm

# Model Evaluation
confusionMatrix(dt_cm, positive = "TRUE")

# Naive Bayes
library(e1071)

nb <- naiveBayes(prefered ~ ., data = train_cl)
nb

# Predicting on test data
nb_pred <- predict(nb, newdata = test_cl)

# Confusion Matrix
nb_cm <- table(test_cl$prefered, nb_pred)
nb_cm

# Model Evaluation
confusionMatrix(nb_cm, positive = "TRUE")












