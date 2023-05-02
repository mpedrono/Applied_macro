rm(list = ls())

library(tidyr)
library(dplyr)
library(ioanalysis)
library(readxl)
library(reshape2)
library(ggplot2)
library(knitr)
library(kableExtra)

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
co2_sheets <- co2_sheets[co2_sheets != "Notes" & co2_sheets != "Sector" & co2_sheets!="SectorAgr" ]

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

# Nombre de pays (44)
list_country <- unique(wiot %>% filter(Country!="TOT") %>% select(Country))
K <- length(list_country$Country)

# Nombre de secteurs (56)
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

# Autres types de consommation

npo_colnames <- sapply(list_country, function(x) paste0(x,"58")) # Organisations a but non lucratif
gov_colnames <- sapply(list_country, function(x) paste0(x,"59")) # Depenses publiques
npo_df <-  wiot %>%  filter(Country!="TOT") %>% select(Code, Country, IndustryCode, FRA58)
gov_df <-  wiot %>%  filter(Country!="TOT") %>% select(Code, Country, IndustryCode, FRA59)

y_tot <- as.matrix(hh_df$FRA57) + as.matrix(npo_df$FRA58) + as.matrix(gov_df$FRA59)

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

co2_prod_fr <- M %*% y
co2_prod_fr

# Emissions indirectes domestiques

co2_dom_fr <- M %*% y_dom
co2_dom_fr

# Emissions indirectes importées

co2_imp_fr <- M %*% y_imp
co2_imp_fr

# Part dans les emissions de production

co2_tot_fr <- M %*% y_tot
co2_prod_fr / co2_tot_fr

# Emissions directes

co2_conso_fr <- co2_conso %>% filter(Country == "FRA") %>% select(Emission)
co2_conso_fr

# Graphique par type d'emissions

co2_fr_type <- data.frame(co2_conso_fr, co2_imp_fr, co2_dom_fr)
co2_fr_type[2,] <- c("Emissions directes","Emissions importées","Emissions domestiques")
co2_fr_type = as.data.frame(t(co2_fr_type))
colnames(co2_fr_type) = c("conso_fr","type")
rownames(co2_fr_type) = NULL
co2_fr_type$conso_fr = as.numeric(co2_fr_type$conso_fr)

plot_type <- ggplot(data = co2_fr_type, aes(x="",y=conso_fr,fill=as.factor(type)))+
  geom_col(color = "black")+
  geom_text(aes(label = scales::percent(conso_fr/sum(conso_fr),accuracy = 0.01)),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())+
  labs(x="", y="", title = ~atop("Répartition des émissions de CO2 liées","à la consommation française"))+
  guides(fill = guide_legend(title = "Emissions"))+
  scale_fill_discrete(labels = c("Directes", "Importées","Domestiques"))+
  scale_fill_manual(values = c("indianred1","indianred3","indianred4"))+
  theme_void()

plot_type
ggsave("plot_type.jpg", plot = plot_type)

##############################
# Décomposition par secteurs #
##############################
table_sector = read_excel("CO2 Emissions.xlsx", sheet = "Sector")

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

co2_fr_sec %>% select(IndustryDescription, e_dom, e_imp, e_tot) %>% mutate(e_dom = round(e_dom),e_imp = round(e_imp), e_tot = round(e_tot)) %>%
  kable("latex")

# Top 10 des secteurs les plus emetteurs

sec_10 <- co2_fr_sec[order(co2_fr_sec$e_tot, decreasing = T),] %>% slice(1:10) %>% select(IndustryCode,IndustryDescription,e_dom,e_imp)

co2_sec_10_long <- sec_10 %>%
  gather(Origine, emission, e_dom:e_imp, factor_key=TRUE)

co2_sec_10_long$IndustryDescription_fr = table_sector$Description_fr_short[match(co2_sec_10_long$IndustryCode, table_sector$Code)]
sec_10$IndustryDescription_fr = table_sector$Description_fr_short[match(sec_10$IndustryCode, table_sector$Code)]

# Graphique des emissions indirectes des 10 secteurs les plus emetteurs

plot_top_sec <- ggplot(co2_sec_10_long%>%filter(Origine!="e_tot"), aes(x=factor(IndustryDescription_fr, levels = sec_10$IndustryDescription_fr),y=emission, fill=Origine)) + 
  geom_bar(stat="identity") +
  labs(title=~atop("Contenu en CO2 de la consommation des ménages","pour les 10 secteurs les plus émetteurs"), y="CO2 (en kt)", x = "Secteur") +
  scale_fill_manual(values = c("indianred1", "indianred4"), labels=c('Domestique', 'Importation')) +
  theme(axis.text.x = element_text(angle=65, vjust=1, hjust=1))+
  coord_flip()+
  theme_classic()

plot_top_sec
ggsave("plot_top_sec.jpg",plot_top_sec)

# Agregation des secteurs au niveau 1

co2_sec_agr <- co2_fr_sec %>% mutate(IndustryAgr = substr(IndustryCode,1,1)) %>%
  group_by(IndustryAgr) %>%
  summarise(e_dom = sum(e_dom), e_imp = sum(e_imp), e_tot = sum(e_tot))

table_sector_agr = read_excel("CO2 emissions.xlsx", sheet = "SectorAgr")

sec_agr_rank <- co2_sec_agr[order(co2_sec_agr$e_tot, decreasing = T),] %>% select(IndustryAgr,e_dom,e_imp,e_tot)
sec_agr_rank[21,c("e_dom","e_imp","e_tot")] <- sum(sec_agr_rank[6:20,c("e_dom","e_imp","e_tot")])
sec_agr_rank[21,"IndustryAgr"] = "Autres"
sec_agr_rank = sec_agr_rank[c(1:5,21),]

sec_agr_rank$IndustryDescriptionAgr = table_sector_agr$IndustryDescriptionAgr_short[match(sec_agr_rank$IndustryAgr, table_sector_agr$IndustryAgr)]
sec_agr_rank[6,"IndustryDescriptionAgr"] = "Autres"

# Graphique de la répartition des emissions indirectes selon les secteurs agreges

piechart_sec = ggplot(sec_agr_rank, aes(x="",y=e_tot, fill = IndustryDescriptionAgr))+
  geom_col(color="black")+
  coord_polar(theta = "y")+
  geom_text(data=subset(sec_agr_rank, e_tot/sum(e_tot)*100>6),aes(x=1.7,label = scales::percent(e_tot/sum(e_tot),accuracy = 1)),
            position = position_stack(vjust = 0.5))+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())+
  labs(x="", y="", title = ~atop("Répartition des émissions indirectes de CO2","liées à la consommation française"))+
  guides(fill = guide_legend(title = "Secteurs"))+
  scale_fill_brewer(palette = "Reds")+
  theme_void()

piechart_sec
ggsave("piechart_sec.jpg", piechart_sec)


##########################
# Décomposition par pays #
##########################

co2_fr_country <- data.frame(list_country$Country[list_country$Country!="FRA"],rep(0,(K-1)))
colnames(co2_fr_country) <- c("Country", "e")

for (country in list_country$Country){
  y_country <- as.matrix(hh_df %>% mutate(FRA57 = ifelse(Country!="FRA",FRA57,0)) %>% 
                           mutate(FRA57 = ifelse(Country==country,FRA57,0)) %>% 
                           select(FRA57))
  
  co2_fr_country$e[co2_fr_country$Country==country] <- M %*% y_country
}

# Agregation des pays par region

list_am <- c("BRA","CAN","MEX")
list_asia_pacific <- c("AUS","CYP","IDN","IND","TWN","JPN","KOR")
list_ue <- c("AUT", "BEL",	"BGR",	"HRV",	"CZE",	"DNK",	"EST",
             "FIN", "DEU",	"GRC",	"HUN",	"IRL",	"ITA",	"LVA",	"LTU",
             "LUX", "MLT",	"NLD",	"POL",	"PRT",	"ROU",	"SVK",	"SVN",	
             "ESP",	"SWE")
list_europe <- c("CHE","GBR","NOR","TUR")

co2_fr_country$region = ifelse(co2_fr_country$Country %in% list_ue, "UE", 
                               ifelse(co2_fr_country$Country %in% list_am, "Amérique du Nord (hors Etats-Unis)", 
                                      ifelse(co2_fr_country$Country=="USA","Etats-Unis",
                                             ifelse(co2_fr_country$Country=="RUS", "Russie", 
                                                    ifelse(co2_fr_country$Country=="CHN","Chine",
                                                           ifelse(co2_fr_country$Country %in% list_asia_pacific, "Asie et Pacifique",
                                                                  ifelse(co2_fr_country$Country %in% list_europe,"Europe","Reste du monde")))))))


# Graphique en niveau des emissions importees par region

plot_region = ggplot(co2_fr_country, aes(x=region,y=e)) +
  geom_bar(stat = "identity",fill="indianred2")+
  labs(x="Région",y="Co2 (en kt)")+
  coord_flip()+
  theme_classic()
plot_region

# Graphique de la repartition des emissions importees par region

co2_fr_region = co2_fr_country %>% group_by(region)%>% summarise(e_imp = sum(e))

piechart_region = ggplot(co2_fr_region, aes(x="", y=e_imp, fill=region))+
  geom_col(color = "black")+
  coord_polar(theta = "y")+
  geom_text(aes(x=1.7,label = scales::percent(e_imp/sum(e_imp), accuracy = 0.1)),position = position_stack(vjust = 0.5))+
  labs(x="",y="")+
  guides(fill = guide_legend(title = "Région"))+
  scale_fill_brewer(palette = "Reds")+
  theme_void()
piechart_region

# Graphique multiple par region

plots_region = plot_grid(plot_region, piechart_region, nrow = 1, ncol = 2, labels = "Répartition des émissions de CO2 importées par région", vjust = 1,scale = c(0.8,1))
plots_region
ggsave("plots_region.jpg", plots_region, units = "cm",
       height = 10, width = 25)


#########################################################################
# Emissions associées à une hausse de 1 unité de la demande des ménages #
#########################################################################

# Par secteur

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


# Top 10 des secteurs les plus emetteurs

sec_10_marg <- co2_fr_sec_marg[order(co2_fr_sec_marg$e_tot, decreasing = T),] %>% slice(1:10) %>% select(IndustryCode,IndustryDescription,e_tot)
sec_10_marg$IndustryDescription_fr = table_sector$Description_fr[match(sec_10_marg$IndustryCode, table_sector$Code)]

plot_top_marg <- ggplot(sec_10_marg, aes(x=factor(IndustryDescription_fr, levels = IndustryDescription_fr),y=e_tot)) + 
  geom_bar(stat="identity", fill="indianred1") +
  labs(title=~atop("Contenu en CO2 de la consommation des ménages","pour les 10 secteurs les plus émetteurs"), y="CO2 (en kt)", x = "Secteur") +
  theme(axis.text.x = element_text(angle=65, vjust=1, hjust=1))+
  coord_flip()+
  theme_classic()

plot_top_marg

ggsave("plot_top_marg.jpg",plot_top_sec)

