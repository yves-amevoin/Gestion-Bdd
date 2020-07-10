# Les expressions régulières

# Petite partie pour expliquer.
#
# automa theory:
# - Controler au maximum des textes
# - Methodes pour s'assurer et de la gestion du texte.
# - Trouver. Un langage pour traiter uniquement que du texte.
# - Langage Regex (administration)
# - A maîtriser!!!!
# - Chopper ce qu'on ne veut pas en plus de ce qu'on veut
# - Chopper ce qu'on ne veut pas du tout sans ce qu'on veut.(.9
# - Awal a 99 an5s.
# - 0.2789
# - 0.576[]
# 
#Comment on l'utilise en pratique.

library(stringr)


charactere_test <- "# Petite partie pour expliquer.
# automa theory:
# - Controler au maximum des textes
# - Methodes pour s'assurer et de la gestion du texte.
# - Trouver. Un langage pour traiter uniquement que du texte.
# - Langage Regex (administration)
# - A maîtriser!!!!
# - Chopper ce qu'on ne veut pas en plus de ce qu'on veut
# - Chopper ce qu'on ne veut pas du tout sans ce qu'on veut.(.9
# - Awal a 99 an5s.
# - 0.2789
# - 0.576[]"

char_test <- gsub("#", "", charactere_test)

#Gerer, enlever les espaces
char_test <- gsub("(^\\s+)|(\\s+)$", "", char_test)
char_test <- iconv(char_test,
                   from = "latin1",
                   to = "ASCII//TRANSLIT")

char_test <- str_split(char_test, "\\n")
char_test <- unlist(char_test)
#Meme chose que gsub
char_test <- str_replace_all(char_test, "[[:punct:]]", "")
char_test <- str_trim(char_test, side = "both") # ou trimws sans package
char_test <- gsub("\\d", "", char_test)
char_test <- str_squish(char_test)
phrase_1 <- char_test[1]
unlist(str_split(phrase_1, "\\s"))


#----------------- Tidyverse -----------------------
library(tidyverse)
library(scales)

#Voir la liste les fichiers d'un repertoire:
#list.files("./data")
# voir la liste des repertoires:
# list.dir("./", recursive = F)
# le recursive permet de savoir si oui ou non on descend au noeud terminal.

#fichiers <- list.files("./data", full.names = TRUE)

#renommer un fichier
#file.rename(fichiers[2], "./data/whatsapp_issa.txt")

readxl::read_excel("./data/cas_pratique.xlsx") -> cas_tidy

# CTRL + SHIFT + M %>% 

# without the tidyverse
round(100*prop.table(table(cas_tidy$resultat_test)), 2)

# with the tidyverse
cas_tidy %>% 
  count(resultat_test) %>% 
  mutate(perc = percent(n / sum(n)))

# without the tidyverse
table(subset(cas_tidy, sexe == "Feminin")$resultat_test)

# with the tidyverse
cas_tidy %>% 
  dplyr::filter(sexe == "Feminin") %>% 
  count(resultat_test)

# pyramide des ages

base_pyramid <- cas_tidy %>% 
  dplyr::select(sexe, age)  

base_pyramid <- base_pyramid %>% 
  mutate(age_coupee  = cut(age, breaks = seq(0, 100, by = 5),
                           right = FALSE)) %>% 
  group_by(sexe, age_coupee) %>% 
  summarize(nb = n()) %>% 
  mutate(nb = ifelse(sexe == "Feminin", -nb, nb))

# Visualisation:

coupures_axe <- seq(-100, 120, by = 5)

# Pyramide des ages;

ggplot(base_pyramid, aes(x = age_coupee, y = nb, fill = sexe)) +
  geom_bar(stat = "identity", width = 1, colour = "black") +
  coord_flip() +
  labs(x = "Groupes d'âge", y = "Effectif") +  
  scale_fill_manual(values = c("pink", "blue")) +
  scale_y_continuous(breaks = coupures_axe, 
                     labels = abs(coupures_axe)) +
  theme_bw()

# chargement de base de données sqllite.
# MySQL
# Oracle / SQL
# SQL Server
# PotgreSQL
# SQLite
# Hadoop / Mapreduce et consort...
library(dbplyr)
library(dplyr)

connection <- DBI::dbConnect(RSQLite::SQLite(), 
                             "./data/portal_mammals.sqlite")
src_dbi(connection)

surveys <- tbl(connection, "surveys")
plots <- tbl(connection, "plots")
species <- tbl(connection, "species")

surveys %>% 
  filter(plot_id == 1) %>% 
  collect() %>% 
  count(species_id)

ab_species <- species %>% 
  filter(species_id == "AB") %>% 
  collect()

ab_species <- left_join(ab_species, collect(surveys), 
                        by = "species_id")
ab_species <- left_join(ab_species, collect(plots), 
                        by = "plot_id") %>% 
  select(plot_type) 

ggplot(ab_species, aes(x = plot_type)) +
  geom_bar()

# Rvest et webmining.
library(rvest)
library(stringr)


lecture <- function(i){
page_web <- read_html(
  paste("https://www.expat-dakar.com/dernieres-annonces?txt=apple&page="           , i, sep ="")
  )

texte <- page_web %>% 
  html_nodes(".ad-item-price span , .ad-item-subtitle , .mb-md-3 a") %>%
  html_text()

texte <- str_squish(texte)
vect_nom <- seq(1, length(texte), by = 3)
vect_car <- seq(2, length(texte), by = 3)
vect_prix <- seq(3, length(texte), by = 3)

base_apple <- data.frame(nom = texte[vect_nom], 
                         carc = texte[vect_car],
                         prix = texte[vect_prix])

base_apple %>% 
  mutate(prix = as.numeric(gsub("\\D", "", prix)))
}

grosse_base <- purrr::map_dfr(1:10, lecture, .id = "Page")
