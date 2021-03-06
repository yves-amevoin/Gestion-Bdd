library(stringr)

# Les expressions regulières

# THIS PART IS THE BEGINNING
# Automa theory
# - Controler au maximum des textes
# - Methodes pour s'assurer du controle et la gestion du texte.
# - Trouver un langage pour traiter que du texte.
# - Langage Regex (administration)
# - A maitriser (important)!!!!
# - Chopper ce qu'on ne veut pas en plus de ce qu'on veut
# - Chopper ce qu'on ne veut pas du tout sans ce qu'on veut
# - Awal vivre jusqu'à 99 ans.
# - 0.2789
# - 0.576[]


#COMMENT ON L'UTILISE EN PRATIQUE

character_test <- "# THIS PART IS THE BEGINNING
# Automa theory
# - Controler au maximum des textes
# - Methodes pour s'assurer du controle et la gestion du texte.
# - Trouver un langage pour traiter que du texte.
# - Langage Regex (administration)
# - A maitriser (important)!!!!
# - Chopper ce qu'on ne veut pas en plus de ce qu'on veut
# - Chopper ce qu'on ne veut pas du tout sans ce qu'on veut
# - Awal vivre jusqu'à 99 ans.
# - 0.2789
# - 0.576[]"


char_test <- gsub("#","",character_test)
char_test <- gsub("(^\\s+)|(\\s+)$", "", char_test)

#gerer, enlever les espaces
char_test <- iconv(char_test,
                        from = "latin1",
                        to= "ASCII//TRANSLIT")

char_test <- str_split(char_test, "\\n")

char_test <- unlist(char_test)

#meme chose que gsub
char_test <- str_replace_all(char_test, "[[:punct:]]","")

char_test <- str_trim(char_test, side = "both") #ou trimws sans le package

char_test <- gsub("\\d","",char_test)

char_test <- str_squish(char_test)

phrase_1 <- char_test[1]
word(phrase_1, start = 1, end = -1)

#Extraire chaque mot
unlist(str_split(phrase_1, "\\s"))




# -------------- Tidyverse ------------
#ensemble de packages
library(tidyverse)
library(scales)

#voir la liste des fichiers d'un repertoire: list.files("./data")
#Voir la liste des repertoires: list.dir("./", recursive=F)
#Le recursive permet de savoir si oui ou non au noeud terminal.

fichiers <- list.files("./data", full.names = TRUE)
file.rename(fichiers[2],"./data/whatsapp_issa.txt")

cas_tidy <- readxl::read_excel("./data/cas_pratique.xlsx")

#The number of positive
#Without tidyverse
round(100*prop.table(table(cas_tidy$resultat_test)),4)

#With Tidyverse
cas_tidy %>% 
  count(resultat_test) %>% 
  mutate(perc=percent(n/sum(n)))

#Without the tidyverse
table(subset(cas_tidy, sexe=="Feminin")$resultat_test)

#With the tidyverse
cas_tidy %>% 
  dplyr::filter(sexe=="Feminin") %>% 
  count(resultat_test)

#Pyramide des ages
base_pyramid <- cas_tidy %>% 
  dplyr::select(sexe,age)

base_pyramid <- base_pyramid %>% 
  mutate(age_coupee= cut(age,breaks = seq(0, 100, by=5),
                         right = FALSE)) %>% 
  group_by(sexe, age_coupee) %>% 
  summarize(nb=n()) %>% 
  mutate(nb=ifelse(sexe=="Feminin",-nb,nb))

# Visualization
coupures_axes <- seq(-100, 120, by=5)
ggplot(base_pyramid, aes(x=age_coupee, y=nb, fill=sexe))+
  geom_bar(stat = "identity",width = 1, colour="black")+
  coord_flip()+
  labs(x="Groupe d'age", y="Effectif")+
  scale_fill_manual(values = c("pink", "blue"))+
  scale_y_continuous(breaks = coupures_axes,
                     labels = abs(coupures_axes))+
  theme_bw()



#Charger une base de donnee sqllite
#MySQL
#Oracle/SQL
#SQLite
#SQL Server
#ProgeSQL

library(dbplyr)
library(dplyr)
library(tidyverse)
connection <- DBI::dbConnect(RSQLite::SQLite(),
                             "./data/portal_mammals.sqlite")
src_dbi(connection)
surveys <- tbl(connection,"surveys")
plots <- tbl(connection,"plots")
species <- tbl(connection,"species")

surveys %>% 
  filter(plot_id==1) %>% 
  collect() %>% 
  count(species_id)

ab_species <- species %>% 
  filter(species_id=="AB") %>% 
  collect()

ab_species <- left_join(ab_species, collect(surveys), 
                        by="species_id")


ab_species <- left_join(ab_species, collect(plots),
                        by="plot_id") %>% 
  select(plot_type)

ggplot(ab_species, aes(x=plot_type))+
  geom_bar()


# Rvest et webmining
library(rvest)
library(stringr)

lecture <- function(i){
  page_web <- read_html(
    paste("https://www.expat-dakar.com/dernieres-annonces?txt=apple&page=",
                        i,sep=""))

texte <- page_web %>% 
  html_nodes(".ad-item-price span , .ad-item-subtitle , .mb-md-3 a") %>% 
  html_text()

texte <- str_squish(texte)
vec_nom <- seq(1, length(texte), by=3)
vec_car <- seq(2, length(texte), by=3)
vec_prix <- seq(3, length(texte), by=3)

base_apple <- data.frame(nom=texte[vec_nom],
                         carc=texte[vec_car],
                         prix=texte[vec_prix])

base_apple %>% 
  mutate(prix=as.numeric(gsub("\\D","",prix)))
}

grosse_base <- purrr::map_dfr(1:10, lecture, .id="Page")




