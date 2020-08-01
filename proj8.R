setwd("C:/Users/katyg/Desktop/OPENCLASSROOMS/PROJET_8/")  

##rm(list=ls(all=TRUE))
library(tidyverse)
library(Hmisc)
library(lubridate)
library(stringr)
library(readxl)

#### RFM ANALYSE DES CLIENTS #####
library(data.table)
library(dplyr)
library(ggplot2)
#library(stringr)
#library(DT)
library(tidyr)
library(knitr)
library(rmarkdown)
Data_vente=read.csv2("Vente2011UK.csv")
str(Data_vente)

df_data <- Data_vente %>% 
  mutate(InvoiceNo=as.factor(InvoiceNo), StockCode=as.factor(StockCode), 
         CustomerID=as.factor(CustomerID), 
         Country=as.factor(Country),
         UnitPrice=as.integer(UnitPrice))
df_data <- df_data %>% 
  mutate(total_Amount = Quantity*UnitPrice)

glimpse(df_data)

df_RFM <- df_data %>% 
  group_by(CustomerID) %>% 
  summarise(recency=as.numeric(as.Date("2011-11-30")-max(InvoiceDate)),
            frequency=n_distinct(InvoiceNo), monitery= sum(total_Amount)/n_distinct(InvoiceNo)) 

summary(df_RFM)

kable(head(df_RFM))
hist(df_RFM$recency)

df_RFM2 <- df_RFM
row.names(df_RFM2) <- df_RFM2$CustomerID
df_RFM2$CustomerID <- NULL

df_RFM2 <- scale(df_RFM2)
summary(df_RFM2)
d <- dist(df_RFM2)
c <- hclust(d, method = 'ward.D2')
#visualisation du dendogram
plot(c)
# division en 8 group
members <- cutree(c,k = 8)

members[1:5]
table(members)
aggregate(df_RFM[,2:4], by=list(members), mean)

# autre result du RFM score
library(rfm)
analysis_date <- lubridate::as_date('2011-11-30')
rfm_result <- rfm_table_customer(df_RFM, CustomerID, frequency,
                                 recency, monitery, analysis_date)
rfm_result

#(La carte de chaleur montre la valeur monétaire moyenne pour différentes catégories de scores
#de récence et de fréquence. Des scores plus élevés de fréquence et de récence sont caractérisés 
#par une valeur monétaire moyenne plus élevée, comme indiqué par les zones plus sombres de la carte thermique.)
rfm_heatmap(rfm_result)
#(Permet rfm_bar_chart()de générer la distribution des scores monétaires pour les différentes combinaisons 
#de scores de fréquence et de récence)
rfm_bar_chart(rfm_result)

#(Utilisez rfm_histograms()pour examiner la distribution relative de
#valeur monétaire (revenu total généré par chaque client)
#jours de récence (jours depuis la dernière visite pour chaque client)
#fréquence (nombre de transactions pour chaque client))
rfm_histograms(rfm_result)
#Visualisez la répartition des clients entre les commandes.
rfm_order_dist(rfm_result)

## les achats proches ont généré plus de chiffre et les chiffres les + importants ont été effectués dans un délai median
rfm_rm_plot(rfm_result)

## la fréquence n'est pas générateur de chiffre, les clients dont le CA est le + elevé effectue un achat ,
#les clients dont les fréquences sont élévées , n'engendre pas de montant élévé
# la majorité des clients se situe sur une échelle de fréquence inférieur à 25 et montant inférieur à 2500
rfm_fm_plot(rfm_result)

# frequence et recence: les plus anciens effectues le moins d achats; les acheteurs actuels effectuent le plus d achats (fidèles)
rfm_rf_plot(rfm_result)

str(rfm_result)

# chargement des données "results de RFM" du fichier csv modifié 
RFM_Score = read.csv2("rfm_score.csv")
str(RFM_Score)
summary(RFM_Score) 

# classification par segments (de 1 à 8 selon le scoreRFM)
#segments %>%
#  count(segment) %>%
#  arrange(desc(n)) %>%
#  rename(Segment = segment, Count = n)

Score<-RFM_Score%>%
  select(rfm_score)%>%
  count(rfm_score)%>%
  arrange(desc(rfm_score))
## a faire : creer des segments correspondant a tres bon client....client perdu
rfm_plot_median_recency(segments)
rfm_plot_median_frequency(segments)
rfm_plot_median_monetary(segments)


