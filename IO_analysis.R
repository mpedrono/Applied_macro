setwd("C:/Users/maily/ENSAE/Applied_macro/Applied_macro")

rm(list = ls())

library(tidyr)
library(dplyr)
library(ioanalysis)
library(readxl)
library(reshape2)
library(ggplot2) 

####################################
# Importation des bases de données #
####################################

# WIOD
load("WIOTS_in_R/WIOT2014_October16_ROW.RData")

wiot = data.frame(wiot)

# Indexation des lignes avec le code pays et le numéro de secteur
wiot <- wiot %>% mutate(Code = paste(Country, RNr, sep=""))

# Compte environmental
co2_sheets <- excel_sheets("CO2 emissions.xlsx")
co2_sheets <- co2_sheets[co2_sheets != "Notes" & co2_sheets != "Sector" ]

co2_list <- lapply(setNames(co2_sheets, co2_sheets) , function(x)
  read_excel("CO2 emissions.xlsx", sheet = x)
)

#########################################
# Manpiluation du compte environmenntal #
#########################################

# Sélection de des données de 2014 pour faire le lien avec la base WIOD
co2_list <- lapply(co2_list, function(x)
  select(x,c(1,"2014"))
)

# On renomme certaines colonnes
co2_list <- lapply(co2_list, function(x)
  rename(x, "IndustryCode" = "...1", "Emission" = "2014")
)


code_table <- wiot %>% filter(Country=="AUT") %>% select(RNr,IndustryCode)

co2_list <- lapply(co2_list, function(x)
  left_join(x, code_table, by = "IndustryCode")
)

# Création d'une base avec les données par pays empilées
co2 <- bind_rows(co2_list, .id="Country")

# Séparation des émissions directes et indirectes

co2_prod <- co2 %>% filter(IndustryCode != "FC_HH") #émissions indirects
co2_conso <- co2 %>% filter(IndustryCode == "FC_HH") #émissions directes

# Indexation des lignes avec le code pays et le numéro de secteur
co2_prod <- co2_prod %>% mutate(Code = paste(Country, RNr, sep=""))

###########################################
# Definition des vecteurs et des matrices #
###########################################

# Nombre de pays
list_country <- unique(wiot %>% filter(Country!="TOT") %>% select(Country))
K <- length(list_country$Country)

# Nombre de secteurs
list_sec <- unique(wiot %>% filter(Country!="TOT") %>% select(IndustryCode,RNr,IndustryDescription))
N <- length(list_sec$IndustryCode)

# Matrice des consommations intermédiaires
Z_df  <- wiot %>% filter(Country!="TOT") %>% select(Code, 6:2469)
Z <- as.matrix(Z_df %>% select(-Code))

# Vecteur de production totale
x <- wiot %>% filter(Country!="TOT") %>% select(c(Code,TOT)) 

# Consommation des ménages
hh_colnames <- sapply(list_country, function(x) paste0(x,"57"))
hh_df <-  wiot %>%  filter(Country!="TOT") %>% select(Code, Country, IndustryCode, FRA57)

y <- as.matrix(hh_df$FRA57)

# Vecteur de la demande domestique
y_dom <- as.matrix(hh_df %>% mutate(FRA57 = ifelse(Country=="FRA",FRA57,0)) %>% select(FRA57))

# Vecteur des importations
y_imp <- as.matrix(hh_df %>% mutate(FRA57 = ifelse(Country!="FRA",FRA57,0)) %>% select(FRA57))

# Vecteur des émissions totales par pays et par secteur
# Vecteur des émissions ordonné comme le vecteur de production
co2_prod <- co2_prod[order(match(co2_prod$Code,x$Code)),]

E <- t(as.matrix(co2_prod %>% select (Emission)))
E <- type.convert(E,dec=".",as.is=T)

######################
# Calculs matriciels #
######################

# Matrice des coefficients techniques (A)

x_hat <- matrix(0, K*N, K*N)
diag(x_hat) <- x$TOT

x_hat_inv <- matrix(0, K*N, K*N)
diag(x_hat_inv) <- 1/x$TOT
x_hat_inv[is.infinite(x_hat_inv)] <- 0 #remove infinite values

A <- Z %*% x_hat_inv

# Matrice de Leontief

I <- matrix(0, K*N, K*N)
diag(I) <- rep(1,K*N)
L = solve(I-A)

# Matrice des emissions par unité

S <- E %*% x_hat_inv  

# Matrice finale de passage entre demande finale et émissions

M <- S %*% as.matrix(L) 

#########################################################
# Emissions totales liées à la consommation des ménages #
#########################################################

# Emissions indirectes

y_hat <- matrix(0, K*N, K*N)
diag(y_hat) <- y

co2_prod_mat <- M %*% y_hat
co2_prod_mat

co2_prod_fr <- M %*% y
co2_prod_fr

write.csv(t(co2_prod_mat), "conso_ménages_Maïlys.csv")

# Emissions indirectes domestiques

co2_dom_fr <- M %*% y_dom
co2_dom_fr

# Emissions indirectes importées

co2_imp_fr <- M %*% y_imp
co2_imp_fr

# Emissions directes

co2_conso_fr <- co2_conso %>% filter(Country == "FRA") %>% select(Emission)
co2_conso_fr

##############################
# Décomposition par secteurs #
##############################

#plot_sector(list_sec,total=T)

co2_sec <- data.frame(list_sec$IndustryCode,list_sec$IndustryDescription,rep(0,length(list_sec$IndustryCode)),rep(0,length(list_sec$IndustryCode)))
colnames(co2_sec) <- c("IndustryCode", "IndustryDescription", "e_dom", "e_imp")

co2_fr_sec <- data.frame(list_sec$IndustryCode,list_sec$IndustryDescription,rep(0,N),rep(0,N))
colnames(co2_fr_sec) <- c("IndustryCode", "IndustryDescription", "e_dom", "e_imp")

for (ind in list_sec$IndustryCode){
  y_dom_sec <- as.matrix(hh_df %>% mutate(FRA57 = ifelse(Country=="FRA",FRA57,0)) %>% 
  mutate(FRA57 = ifelse(IndustryCode==ind,FRA57,0)) %>% 
  select(FRA57))
  
  co2_fr_sec$e_dom[co2_fr_sec$IndustryCode == ind] <- M %*% y_dom_sec
  
  y_imp_sec <- as.matrix(hh_df %>% mutate(FRA57 = ifelse(Country!="FRA",FRA57,0)) %>% 
                          mutate(FRA57 = ifelse(IndustryCode==ind,FRA57,0)) %>% 
                          select(FRA57))
  
  co2_fr_sec$e_imp[co2_fr_sec$IndustryCode == ind] <- M %*% y_imp_sec
}

co2_fr_sec <- co2_fr_sec %>%
  mutate(e_tot = e_dom + e_imp)

co2_fr_sec_long <- co2_fr_sec %>% select(IndustryCode, IndustryDescription, e_dom, e_imp) %>%
  gather(Origine, emission, e_dom:e_imp, factor_key=TRUE)

plot_sec <- ggplot(co2_fr_sec_long, aes(x=IndustryCode,y=emission, fill=Origine)) + 
  geom_bar(stat="identity") +
  labs(title="Emissions de CO2 par secteur", x="CO2 (en kt)", y = "Secteur") +
  scale_fill_manual(values = c("indianred1", "indianred4"), labels=c('Domestique', 'Importation'))

plot_sec

# Top 10 sectors

sec_10 <- co2_fr_sec[order(co2_fr_sec$e_tot, decreasing = T),] %>% slice(1:10) %>% select(IndustryCode,IndustryDescription,e_dom,e_imp)

co2_sec_10_long <- sec_10 %>%
  gather(Origine, emission, e_dom:e_imp, factor_key=TRUE)

plot_dom_top_sec <- ggplot(co2_sec_10_long, aes(x=factor(IndustryDescription, levels = sec_10$IndustryDescription),y=emission, fill=Origine)) + 
  geom_bar(stat="identity") +
  labs(title="Contenu en CO2 de la consommation des ménages pour les 10 secteurs les plus émetteurs", x="CO2 (en kt)", y = "Secteur") +
  scale_fill_manual(values = c("indianred1", "indianred4"), labels=c('Domestique', 'Importation')) +
  theme(axis.text.x = element_text(angle=65, vjust=1, hjust=1))

plot_dom_top_sec

#Aggregation

co2_sec_agr <- co2_fr_sec %>% mutate(IndustryAgr = substr(IndustryCode,1,1)) %>%
  group_by(IndustryAgr) %>%
  summarise(e_dom = sum(e_dom), e_imp = sum(e_imp))

co2_sec_agr_long <- co2_sec_agr %>% select(IndustryAgr, e_dom, e_imp) %>%
  gather(Origine, emission, e_dom:e_imp, factor_key=TRUE)

plot_sec_agr <- ggplot(co2_sec_agr_long, aes(x=IndustryAgr,y=emission, fill=Origine)) + 
  geom_bar(stat="identity")+ 
  labs(title="Contenu en CO2 de la consommation des ménages par secteur", x="CO2 (en kt)", y = "Secteur")+
  scale_fill_manual(values = c("indianred1", "indianred4"), labels=c('Domestique', 'Importation'))
plot_sec_agr


##########################
# Décomposition par pays #
##########################

co2_fr_country <- data.frame(list_country$Country[list_country$Country!="FRA"],rep(0,K))
colnames(co2_fr_country) <- c("Country", "e")

for (country in list_country$Country){
  y_country <- as.matrix(hh_df %>% mutate(FRA57 = ifelse(Country!="FRA",FRA57,0)) %>% 
                           mutate(FRA57 = ifelse(Country==country,FRA57,0)) %>% 
                           select(FRA57))
  
  co2_fr_country$e[co2_fr_country$Country==country] <- M %*% y_country
}

co2_top_country <- co2_fr_country[order(co2_fr_country$e, decreasing=T),] %>% slice(1:10)

plot_country <- ggplot(co2_fr_country, aes(x=Country,y=e)) + 
  geom_bar(stat="identity", fill="indianred1")+ 
  labs(title="Contenu en CO2 de la consommation des ménages importé par pays", x="CO2 (en kt)", y = "Pays")
plot_country

plot_top_country <- ggplot(co2_top_country, aes(x=Country,y=e)) + geom_bar(stat="identity", fill="indianred1")+ 
  labs(title="Contenu en CO2 de la consommation des ménages par pays", x="CO2 (en kt)", y = "Secteur")
plot_top_country


#UE
list_ue <- c("AUT", "BEL",	"BGR",	"HRV",	"CYP",	"CZE",	"DNK",	"EST",	
             "FIN", "DEU",	"GRC",	"HUN",	"IRL",	"ITA",	"LVA",	"LTU",
             "LUX", "MLT",	"NLD",	"POL",	"PRT",	"ROU",	"SVK",	"SVN",	
             "ESP",	"SWE")

co2_fr_ue <- data.frame(list_ue,rep(0,length(list_ue)))
colnames(co2_fr_ue) <- c("Country", "e")


for (country in list_ue){
  y_ue <- as.matrix(hh_df %>% mutate(FRA57 = ifelse(Country!="FRA",FRA57,0)) %>% 
                           mutate(FRA57 = ifelse(Country==country,FRA57,0)) %>% 
                           select(FRA57))
  
  co2_fr_ue$e[co2_fr_ue$Country==country] <- M %*% y_ue
}

plot_ue <- ggplot(co2_fr_ue, aes(x=Country,y=e)) + 
  geom_bar(stat="identity", fill="indianred1")+ 
  labs(title="Contenu en CO2 de la consommation des ménages importé par pays", x="CO2 (en kt)", y = "Pays")
plot_ue

####################################################################
# Emissions associées à une hausse de 1% de la demande des ménages #
####################################################################

#By sector

co2_fr_sec_marg <- data.frame(list_sec$IndustryCode,list_sec$IndustryDescription,rep(0,N),rep(0,N))
colnames(co2_fr_sec_marg) <- c("IndustryCode", "IndustryDescription", "e_dom", "e_imp")

for (ind in list_sec$IndustryCode){
  y_dom_sec_marg <- as.matrix(hh_df %>% mutate(FRA57 = ifelse(IndustryCode==ind,1,0)) %>%
                                mutate(FRA57 = ifelse(Country=="FRA",FRA57,0)) %>% 
                                select(FRA57))
  
  co2_fr_sec_marg$e_dom[co2_fr_sec$IndustryCode == ind] <- M %*% y_dom_sec_marg
  
  y_imp_sec_marg <- as.matrix(hh_df %>% mutate(FRA57 = ifelse(IndustryCode==ind,1,0)) %>%
                                mutate(FRA57 = ifelse(Country!="FRA",FRA57,0)) %>% 
                                select(FRA57))
  
  co2_fr_sec_marg$e_imp[co2_fr_sec$IndustryCode == ind] <- M %*% y_imp_sec_marg
}

co2_fr_sec_marg <- co2_fr_sec_marg %>%
  mutate(e_tot = e_dom + e_imp)

co2_fr_sec_long_marg <- co2_fr_sec_marg %>% select(IndustryCode, IndustryDescription, e_dom, e_imp) %>%
  gather(Origine, emission, e_dom:e_imp, factor_key=TRUE)

plot_sec_marg <- ggplot(co2_fr_sec_marg, aes(x=IndustryCode,y=e_tot)) + 
  geom_bar(stat="identity", fill="indianred1") +
  labs(title="Emissions de CO2 par secteur", x="CO2 (en kt)", y = "Secteur")

plot_sec_marg

# Top 10 sectors

sec_10_marg <- co2_fr_sec_marg[order(co2_fr_sec_marg$e_tot, decreasing = T),] %>% slice(1:10) %>% select(IndustryCode,IndustryDescription,e_tot)

plot_top_marg <- ggplot(sec_10_marg, aes(x=IndustryCode,y=e_tot)) + 
  geom_bar(stat="identity", fill="indianred1") +
  labs(title="Emissions de CO2 par secteur", x="CO2 (en kt)", y = "Secteur")

plot_top_marg

