#code = https://gist.github.com/jasonrwang/fed65b0766b442ae4dee8ed3efddf6a9

library(tidyr)
library(dplyr)
library(ioanalysis)
library(readxl)
library(reshape2)
library(ggplot2)
library(cowplot)
library(knitr)

###################
# Import datasets #
###################

load("WIOT2014_October16_ROW.RData")

wiot = data.frame(wiot)
# Re-index the row names in the same way as columns
wiot <- wiot %>% mutate(Code = paste(Country, RNr, sep=""))

co2_sheets <- excel_sheets("CO2 emissions.xlsx")
co2_sheets <- co2_sheets[co2_sheets != "Notes" & co2_sheets != "Sector" & co2_sheets != "SectorAgr" ]

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
#list_country_co2 <- unique(co2 %>% select(Country))
#list_country_co2$Country <- toupper(list_country_co2$Country)
#list_country$Country[!(list_country$Country %in% list_country_co2$Country)] 
# il manque NLD dans les emissions CO2 => pq ?

list_sec <- unique(wiot %>% filter(Country!="TOT") %>% select(IndustryCode,RNr,IndustryDescription))
list_sec_co2 <- unique(co2 %>% select(IndustryCode))
list_sec_co2$IndustryCode[!(list_sec_co2$IndustryCode %in% list_sec$IndustryCode)] 
# FC_HH = Final consumption by HH => can be removed

co2_df <- co2 %>% filter(IndustryCode != "FC_HH")
#co2_conso <- co2 %>% filter(IndustryCode == "FC_HH")

# en attendant = creer un sous-tab vide pour NLD ? SOLUTION A TROUVER !!
# Peut être mettre les facteurs d'emissions moyens / national account ?

#nld_creat <- cbind(rep("NLD",56), list_sec$IndustryCode,rep(0,56), list_sec$RNr)
#colnames(nld_creat) <- colnames(co2_df)
#co2_df <- rbind(co2_df, nld_creat)

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
diag(x_hat) <- x_df$TOT

x_hat_inv <- matrix(0, K*N, K*N)
diag(x_hat_inv) <- 1/x_df$TOT
x_hat_inv[is.infinite(x_hat_inv)] <- 0 #remove infinite values

A <- Z %*% x_hat_inv

#Leontief matrix

I <- matrix(0, K*N, K*N)
diag(I) <- rep(1,K*N)
L = solve(I-A)

# Factors of emission

y_hat <- matrix(0, K*N, K*N)
diag(x_hat) <- x_df$TOT

S <- E %*% x_hat_inv  

M <- S %*% as.matrix(L) 

#Repasser en df pour faciliter ?

#L = as.data.frame(L)
#colnames(L) <- x$Code

E = as.data.frame(E)
colnames(E) <- x_df$Code

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

#HH final consumption : à vérifier avec Maïlys

co2_conso_fr <- co2 %>% filter(Country == "FRA") %>% select(IndustryCode,Emission)
co2_fr_ind <- sum(co2_conso_fr[-57,]$Emission) #somme des émissions indirectes
co2_conso_dir <- sum(co2_conso_fr[57,]$Emission) 

#somme des émissions totales : directes + indirectes
co2_conso_dir + co2_fr_ind #345586.8

co2_fr_type <- data.frame(co2_conso_dir, co2_imp_fr, co2_dom_fr)
co2_fr_type[2,] <- c("émissions directes","émissions importées","émissions domestiques")
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
  theme_void()

plot_type
ggsave("plot_type.jpg", plot = plot_type)


co2_fr_imp = data.frame(co2_dom_fr, co2_imp_fr)
co2_fr_imp = as.data.frame(t(co2_fr_imp))
co2_fr_imp$type = c("Domestique","Importées")
rownames(co2_fr_imp) = NULL

plot_imp <- ggplot(data = co2_fr_imp, aes(x="",y=Emission,fill=as.factor(type)))+
  geom_col(color = "black")+
  geom_text(aes(label = scales::percent(Emission/sum(Emission),accuracy = 0.01)),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())+
  labs(x="", y="", title = ~atop("Répartition des émissions indirectes de CO2","liées à la consommation française"))+
  guides(fill = guide_legend(title = "Emissions"))+
  scale_fill_discrete(labels = c("Domestiques", "Importées"))+
  theme_void()

plot_imp
ggsave("plot_imp.jpg", plot = plot_imp)


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

# Top 10 sectors

sec_10 <- co2_fr_sec[order(co2_fr_sec$e_tot, decreasing = T),] %>% slice(1:10) %>% select(IndustryCode,IndustryDescription,e_dom,e_imp,e_tot)

co2_sec_10_long <- sec_10 %>%
  gather(Origine, emission, e_dom:e_tot, factor_key=TRUE)

table_sector = read_excel("CO2 Emissions.xlsx", sheet = "Sector")

co2_sec_10_long$IndustryDescription_fr = table_sector$Description_fr_short[match(co2_sec_10_long$IndustryCode, table_sector$Code)]
sec_10$IndustryDescription_fr = table_sector$Description_fr_short[match(sec_10$IndustryCode, table_sector$Code)]

plot_top_sec <- ggplot(co2_sec_10_long%>%filter(Origine!="e_tot"), aes(x=factor(IndustryDescription_fr, levels = sec_10$IndustryDescription_fr),y=emission, fill=Origine)) + 
  geom_bar(stat="identity") +
  labs(title=~atop("Contenu en CO2 de la consommation des ménages","pour les 10 secteurs les plus émetteurs"), y="CO2 (en kt)", x = "Secteur") +
  scale_fill_manual(values = c("indianred1", "indianred4"), labels=c('Domestique', 'Importation')) +
  theme(axis.text.x = element_text(angle=65, vjust=1, hjust=1))+
  coord_flip()+
  theme_classic()

plot_top_sec
ggsave("plot_top_sec.jpg",plot_top_sec)

sec_10_rank <- co2_fr_sec[order(co2_fr_sec$e_tot, decreasing = T),] %>% select(IndustryCode,IndustryDescription,e_dom,e_imp,e_tot)
sec_10_rank[57,c("e_dom","e_imp","e_tot")] = sum(sec_10_rank[11:56,c("e_dom","e_imp","e_tot")])
sec_10_rank = sec_10_rank[c(1:10,57),]
sec_10_rank$IndustryDescription_fr = table_sector$Description_fr_short[match(sec_10_rank$IndustryCode, table_sector$Code)]
sec_10_rank[11,c("IndustryCode","IndustryDescription","IndustryDescription_fr")] = c("Autres","Others","Autres")
sec_10_rank$e_percent = sec_10_rank$e_tot/sum(sec_10_rank$e_tot)*100

table_sec = sec_10_rank[,c("IndustryCode","IndustryDescription_fr","e_dom","e_imp","e_tot","e_percent")]
kable(table_sec, digits=2, format = "latex")

piechart_10_sec= ggplot(sec_10_rank[1:10,], aes(x="",y=e_tot,fill=IndustryDescription_fr))+
  geom_col(color="black")+
  coord_polar(theta = "y")+
  geom_text(aes(x=1.65,label=scales::percent(e_tot/sum(e_tot), accuracy = 0.1)), position = position_stack(vjust = 0.5))+
  labs(x="",y="",title = "Répartition du contenu en CO2 des 10 plus gros secteurs émetteurs")+
  guides(fill = guide_legend(title = "Secteurs"))+
  theme_void()
piechart_10_sec
ggsave("piechart_10_sec.jpg", piechart_10_sec)

#Pourcentage des émissions émises par le top 10 des secteurs
top_10_sec = sum(sec_10_rank[1:10,"e_tot"])
autres_sec = sec_10_rank[11,"e_tot"]
top_10_sec/(top_10_sec+autres_sec)*100 #46.8%

#Aggregation

co2_sec_agr <- co2_fr_sec %>% mutate(IndustryAgr = substr(IndustryCode,1,1)) %>%
  group_by(IndustryAgr) %>%
  summarise(e_dom = sum(e_dom), e_imp = sum(e_imp), e_tot=sum(e_tot))

co2_sec_agr_long <- co2_sec_agr %>% select(IndustryAgr, e_dom, e_imp) %>%
  gather(Origine, emission, e_dom:e_imp, factor_key=TRUE)

sec_agr_10 <- co2_sec_agr[order(co2_sec_agr$e_tot, decreasing = T),] %>% slice(1:10) %>% select(IndustryAgr,e_dom,e_imp,e_tot)

sec_agr_10_long <- sec_agr_10 %>% select(IndustryAgr, e_dom,e_imp)%>%
  gather(Origine,emission, e_dom:e_imp, factor_key = TRUE)

table_sector_agr = read_excel("CO2 emissions.xlsx", sheet = "SectorAgr")

sec_agr_10_long$IndustryDescriptionAgr = table_sector_agr$IndustryDescriptionAgr_short[match(sec_agr_10_long$IndustryAgr, table_sector_agr$IndustryAgr)]

plot_sec_agr <- ggplot(sec_agr_10_long, aes(x=reorder(IndustryDescriptionAgr,-emission),y=emission, fill=Origine)) + 
  geom_bar(stat="identity")+ 
  labs(title="Contenu en CO2 de la consommation des ménages par secteur", y="CO2 (en kt)", x = "Secteur")+
  scale_fill_manual(values = c("indianred1", "indianred4"), labels=c('Domestique', 'Importation'))+
  theme(axis.text.x = element_text(angle=65, vjust=1, hjust=1))
plot_sec_agr
ggsave("plot_10_sec_agr.jpg",plot_sec_agr)

sec_agr_rank <- co2_sec_agr[order(co2_sec_agr$e_tot, decreasing = T),] %>% select(IndustryAgr,e_dom,e_imp,e_tot)
sec_agr_rank[21,c("e_dom","e_imp","e_tot")] <- sum(sec_agr_rank[6:20,c("e_dom","e_imp","e_tot")])
sec_agr_rank[21,"IndustryAgr"] = "Autres"
sec_agr_rank = sec_agr_rank[c(1:5,21),]

sec_agr_rank$IndustryDescriptionAgr = table_sector_agr$IndustryDescriptionAgr_short[match(sec_agr_rank$IndustryAgr, table_sector_agr$IndustryAgr)]
sec_agr_rank[6,"IndustryDescriptionAgr"] = "Autres"

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
  theme_void()
piechart_sec
ggsave("piechart_sec.jpg", piechart_sec)

## Par pays

co2_fr_country <- data.frame(list_country$Country[list_country$Country!="FRA"],rep(0,K-1))
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
  labs(title="Contenu en CO2 de la consommation des ménages par pays", x="CO2 (en kt)", y = "Secteur")+
  theme_classic()
plot_top_country

#Par region

list_am_without_us <- c("BRA","CAN","MEX")
list_asia_without_china <- c("AUS","CYP","IDN","IND","TWN","JPN","KOR")
list_ue <- c("AUT", "BEL",	"BGR",	"HRV",	"CZE",	"DNK",	"EST",	
             "FIN", "DEU",	"GRC",	"HUN",	"IRL",	"ITA",	"LVA",	"LTU",
             "LUX", "MLT",	"NLD",	"POL",	"PRT",	"ROU",	"SVK",	"SVN",	
             "ESP",	"SWE")
list_europe <- c("CHE","GBR","NOR","TUR")

co2_fr_country$region = ifelse(co2_fr_country$Country %in% list_ue, "UE", 
                               ifelse(co2_fr_country$Country %in% list_am_without_us, "Amérique du Nord (hors Etats-Unis)", 
                                      ifelse(co2_fr_country$Country=="USA","Etats-Unis",
                                             ifelse(co2_fr_country$Country=="RUS", "Russie", 
                                                    ifelse(co2_fr_country$Country=="CHN","Chine",
                                                           ifelse(co2_fr_country$Country %in% list_asia_without_china, "Asie et Pacifique",
                                                                  ifelse(co2_fr_country$Country %in% list_europe,"Europe","Reste du monde")))))))


plot_region = ggplot(co2_fr_country, aes(x=region,y=e)) +
  geom_bar(stat = "identity",fill="indianred1")+
  labs(x="Région",y="Co2 (en kt)")+
  coord_flip()+
  theme_classic()
plot_region

co2_fr_region = co2_fr_country %>% group_by(region)%>% summarise(e_imp = sum(e))

piechart_region = ggplot(co2_fr_region, aes(x="", y=e_imp, fill=region))+
  geom_col(color = "black")+
  coord_polar(theta = "y")+
  geom_text(aes(x=1.7,label = scales::percent(e_imp/sum(e_imp), accuracy = 0.1)),position = position_stack(vjust = 0.5))+
  labs(x="",y="")+
  guides(fill = guide_legend(title = "Région"))+
  theme_void()
piechart_region
ggsave("piechart_region.jpg", piechart_region)

plots_region = plot_grid(plot_region, piechart_region, nrow = 1, ncol = 2, labels = "Répartition des émissions de CO2 importées par région", vjust = 1,scale = c(0.8,1))
plots_region
ggsave("plots_region.jpg", plots_region, units = "cm",
       height = 10, width = 25)

#UE


co2_fr_ue <- data.frame(list_ue,rep(0,length(list_ue)))
colnames(co2_fr_ue) <- c("Country", "e")


for (country in list_ue){
  y_ue <- as.matrix(hh_df %>% mutate(FRA57 = ifelse(Country!="FRA",FRA57,0)) %>% 
                      mutate(FRA57 = ifelse(Country==country,FRA57,0)) %>% 
                      select(FRA57))
  
  co2_fr_ue$e[co2_fr_ue$Country==country] <- M %*% y_ue
}

co2_fr_ue = co2_fr_ue[order(co2_fr_ue$e, decreasing = T),]

deu_percent = co2_fr_ue[1,"e"]/sum(co2_fr_ue$e)
esp_percent = co2_fr_ue[2,"e"]/sum(co2_fr_ue$e)
bel_percent = co2_fr_ue[3,"e"]/sum(co2_fr_ue$e)

plot_ue <- ggplot(co2_fr_ue, aes(x="",fill=Country,y=e)) + 
  geom_col(color = "black")+ 
  coord_polar(theta = "y")+
  geom_text(data = subset(co2_fr_ue, e/sum(e)>0.08), aes(x=1.6, label = scales::percent(e/sum(e), accuracy=0.1)), position = position_stack(vjust = 0.5))+
  labs(title="Contenu en CO2 de la consommation des ménages importé par pays", x="",y="")+
  theme_void()
plot_ue
