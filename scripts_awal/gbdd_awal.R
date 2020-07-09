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
ggplot(base_pyramid, aes(x=age_coupee, y=nb, fill=sexe))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(x="Groupe d'age", y="Effectif")
