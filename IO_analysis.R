setwd("C:/Users/maily/OneDrive/Documents/Cours 3A ENSAE/Applied Macro")

#code = https://gist.github.com/jasonrwang/fed65b0766b442ae4dee8ed3efddf6a9

library(tidyr)
library(dplyr)
library(ioanalysis)
library(readxl)
library(reshape2)
library(ggplot2) 

###################
# Import datasets #
###################

load("WIOTS_in_R/WIOT2014_October16_ROW.RData")

wiot = data.frame(wiot)
# Re-index the row names in the same way as columns
wiot <- wiot %>% mutate(Code = paste(Country, RNr, sep=""))

co2_sheets <- excel_sheets("CO2 emissions.xlsx")
co2_sheets <- co2_sheets[co2_sheets != "Notes" & co2_sheets != "Sector" ]

co2_list <- lapply(setNames(co2_sheets, co2_sheets) , function(x)
  read_excel("CO2 emissions.xlsx", sheet = x)
)

###################################
# Environmental satellite account #
###################################

co2_list <- lapply(co2_list, function(x)
  select(x,c(1,"2014"))
)

co2_list <- lapply(co2_list, function(x)
  rename(x, "IndustryCode" = "...1")
)

co2_list <- lapply(co2_list, function(x)
  rename(x, "Emission" = "2014")
)

code_table <- wiot %>% filter(Country=="AUT") %>% select(RNr,IndustryCode)

co2_list <- lapply(co2_list, function(x)
  left_join(x, code_table, by = "IndustryCode")
)

co2 <- bind_rows(co2_list, .id="Country")

#Check nb pays et nb secteurs = NLD manquant
list_country <- unique(wiot %>% filter(Country!="TOT") %>% select(Country))
list_country_co2 <- unique(co2 %>% select(Country))
list_country_co2$Country <- toupper(list_country_co2$Country)
list_country$Country[!(list_country$Country %in% list_country_co2$Country)] 
# il manque NLD dans les emissions CO2 => pq ?

list_sec <- unique(wiot %>% filter(Country!="TOT") %>% select(IndustryCode,RNr,IndustryDescription))
list_sec_co2 <- unique(co2 %>% select(IndustryCode))
list_sec_co2$IndustryCode[!(list_sec_co2$IndustryCode %in% list_sec$IndustryCode)] 
# FC_HH = Final consumption by HH => can be removed

co2_df <- co2 %>% filter(IndustryCode != "FC_HH")
#co2_conso <- co2 %>% filter(IndustryCode == "FC_HH")

# en attendant = creer un sous-tab vide pour NLD ? SOLUTION A TROUVER !!
# Peut être mettre les facteurs d'emissions moyens / national account ?

nld_creat <- cbind(rep("NLD",56), list_sec$IndustryCode,rep(0,56), list_sec$RNr)
colnames(nld_creat) <- colnames(co2_df)
co2_df <- rbind(co2_df, nld_creat)

co2_df <- co2_df %>% mutate(Code = paste(Country, RNr, sep=""))

# FAIRE ATTENTION = VERIFIER SI ORDRE EST BON ??

###################
# Define matrices #
###################

# Column 1-5 are row descriptions, the countries' data end at col 2469, and 2690 is the total output
# Row 2470 is value added
# Therefore, let's subset the data into our matrices.

#nb countries
K <- n_distinct(wiot %>% filter(Country!="TOT") %>% select(Country))  #remove TOT (not a country)

#nb sectors 56
N <- n_distinct(wiot %>% filter(Country!="TOT") %>% select(IndustryCode))

unwanted_cols <- c("IndustryCode", "IndustryDescription", "Country", "RNr", "Year", "TOT")

# Inter-industry matrix
Z_df  <- wiot %>% filter(Country!="TOT") %>% select(Code, 6:2469)
Z <- as.matrix(Z_df %>% select(-Code))

# Total Output
x_df <- wiot %>% filter(Country!="TOT") %>% select(c(Code,TOT)) 

# Order emission vector

co2_df <- co2_df[order(match(co2_df$Code,x_df$Code)),]

# Value-add
#v_df <- wiot %>% filter(IndustryCode=="VA") %>% select(Code, 6:2469)

# HH consumption
hh_colnames <- sapply(list_country, function(x) paste0(x,"57"))
hh_df <-  wiot %>%  filter(Country!="TOT") %>% select(Code, Country, IndustryCode, FRA57)

y <- as.matrix(hh_df$FRA57)

y_dom <- as.matrix(hh_df %>% mutate(FRA57 = ifelse(Country=="FRA",FRA57,0)) %>% select(FRA57))

y_imp <- as.matrix(hh_df %>% mutate(FRA57 = ifelse(Country!="FRA",FRA57,0)) %>% select(FRA57))

# Emission by sector and country

E <- t(as.matrix(co2_df %>% select (Emission)))
E <- type.convert(E,dec=".",as.is=T)

#Computation of the technical coefficients

x_hat <- matrix(0, K*N, K*N)
diag(x_hat) <- x$TOT

x_hat_inv <- matrix(0, K*N, K*N)
diag(x_hat_inv) <- 1/x$TOT
x_hat_inv[is.infinite(x_hat_inv)] <- 0 #remove infinite values

A <- Z %*% x_hat_inv

#Leontief matrix

I <- matrix(0, K*N, K*N)
diag(I) <- rep(1,K*N)
L = solve(I-A)

# Factors of emission

y_hat <- matrix(0, K*N, K*N)
diag(x_hat) <- x$TOT

S <- E %*% x_hat_inv  

M <- S %*% as.matrix(L) 

#Repasser en df pour faciliter ?

#L = as.data.frame(L)
#colnames(L) <- x$Code

E = as.data.frame(E)
colnames(E) <- x$Code

###################
# Total emissions #
###################

## Total CO2 emissions due to final demand by households

y <- as.matrix(hh_df$FRA57)

y_dom <- as.matrix(hh_df %>% mutate(FRA57 = ifelse(Country=="FRA",FRA57,0)) %>% select(FRA57))

y_imp <- as.matrix(hh_df %>% mutate(FRA57 = ifelse(Country!="FRA",FRA57,0)) %>% select(FRA57))

#Production

co2_prod_fr <- M %*% y
co2_prod_fr

#Domestic production

co2_dom_fr <- M %*% y_dom
co2_dom_fr

#Imported production

co2_imp_fr <- M %*% y_imp
co2_imp_fr

#HH final consumption

co2_conso_fr <- co2_conso %>% filter(Country == "FRA") %>% select(Emission)
co2_conso_fr

##Par secteur

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

# Top 10 domestic sectors

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


## Par pays

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
  geom_bar(stat="identity", fill="lightblue")+ 
  labs(title="Contenu en CO2 de la consommation des ménages importé par pays", x="CO2 (en kt)", y = "Pays")
plot_country

plot_top_country <- ggplot(co2_top_country, aes(x=Country,y=e)) + geom_bar(stat="identity", fill="indianred1")+ 
  labs(title="Contenu en CO2 de la consommation des ménages par pays", x="CO2 (en kt)", y = "Secteur")
plot_top_country

#resultats chelous NLD?

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


