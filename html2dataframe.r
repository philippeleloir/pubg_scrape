## scrape1anal.r par leloir
## Script pour lire une page extraite par phantomjs
## 2017-09-16

####SETUP####
library(rvest)
library(stringi)
page_html <- read_html("page1.html") ## une page lue par phantom_scrape.js


####LIRE DATA
## tous les titres associés à des données
labels <- html_nodes(page_html, 'div.label')  %>% html_text()
values <- html_nodes(page_html, 'div.value')  %>% html_text()

#nature des resultats annoncés {Win, X Matches, Top 10} #
res <- html_nodes(page_html, 'h3') %>% html_text()

### Fonction permettant de filtrer les entrées à match multiples.
i <- 1 
data <- data.frame(matrix(list(), ncol=28, nrow=0))
colnames(data) <- labels[1:28]
for(r in res){
 if(nchar(r) < 8){ # on ne prend pas le premier résultat puisqu'il sert à initialiser le df ci-haut
  #i <- i + 27
  df.tmp <- data.frame(matrix(unlist(values[(i):(i+27)]), ncol=28, nrow=1))
  colnames(df.tmp) <- labels[1:28]
  data <- rbind(data, df.tmp)
  i <- i + 28
 } else {
  i <- i + 29
 }
  ii <- 1
}

###ajouter une colonne Resultat
df3.tmp <- data.frame(matrix(list(), ncol=1, nrow=0))
ii <- 1
colnames(df3.tmp)<- list("Resultat")
for(r in res){
  if(nchar(r) < 8){
    df <- data.frame(matrix(list(r), ncol=1, nrow=1))
    df3.tmp <- rbind(df3.tmp, df)
    ii <- ii + 1
  }
}
data$Resultat <- as.character(df3.tmp[,1])


##extraire le Playmode
df.tmp <- data.frame(matrix(list(), ncol=1, nrow=0))
colnames(df.tmp) <- "Playmode"
parties <- html_nodes(page_html, 'span.player-count-icons')

for(mode in parties){
  df.tmp <- data.frame(matrix(list(), ncol=1, nrow=0))
  colnames(df.tmp) <- "Playmode"
  parties <- html_nodes(page_html, 'span.player-count-icons')
  i <- 1
  for(mode in parties){
    if(nchar(res)[i] < 8){
      if(length(html_nodes(mode, 'i')) == 1){ # img selon nb de joueurs
        x <- data.frame(matrix(list("Solo"), ncol=1, nrow=1)) 
        colnames(x) <- "Playmode"
        df.tmp <- rbind(df.tmp, x)
      } else if (length(html_nodes(mode, 'i')) == 2){
        x <- data.frame(matrix(list("Duo"), ncol=1, nrow=1))
        colnames(x) <- "Playmode"
        df.tmp <- rbind(df.tmp, x)
      } else if (length(html_nodes(mode, 'i')) == 4){
        x <- data.frame(matrix(list("Squad"), ncol=1, nrow=1))
        colnames(x) <- "Playmode"
        df.tmp <- rbind(df.tmp, x)
      }
    }
    i <- i + 1
    #  print(playmode)
  }

}
playmode <- df.tmp  
data$Playmode <- as.character(playmode[,1])


#### DATA FORMAT
##Probleme 1: dedoublement des donnes. Il faut retirer les colonnes [1:4] et 6
data <- data[ -c(1:4, 12) ]

## définir les données numériques et nettoyer leur enregistrement
data$Kills <- as.numeric(data$Kills)
data$RatingT <- as.numeric(stri_join(substr(data[,"Rating"],1,1),substr(data[,"Rating"],3,5)))
data$Assists <- as.numeric(data$Assists)
data$Damage <- as.numeric(gsub(",","",data$Damage))
data$Travelled <- as.numeric(substr(as.character(data$Travelled),1,5))
data$`Time Alive (min)` <- as.numeric(substr(stri_trim_both(data$'Time Alive', "\\P{Wspace}"),1,2)) ## ATTENTION non teste sur une duree < 10 par manque de donnees
data$DBNOs <- as.numeric(as.character(data$'DBNOs'))
data$'Road Kills' <- as.numeric(as.character(data$'Road Kills'))
data$WalkDist <- as.numeric(substr(data[,'Walk Distance'],1,5))
data$VehicleDist <- as.numeric(substr(data[,'Vehicle Distance'],1,5))
data$'Weapons Acquired'<- as.numeric(as.character(data$'Weapons Acquired'))
data$'Heals' <- as.numeric(as.character(data$'Heals'))
data$'Boosts' <- as.numeric(as.character(data$'Boosts'))
data$'Revives' <- as.numeric(as.character(data$'Revives'))
data$'Team Kills' <- as.numeric(as.character(data$'Team Kills'))
data$'Suicide' <- as.character(data$'Suicide')

##Combiner une table adaptée à l'analyse
dt2 <- data.frame(matrix(as.character(data$Resultat), ncol=1, nrow=length(data$Resultat)))
colnames(dt2) <- c("Resultat")
dt2$Playmode <- data$Playmode
dt2$Kills <- data$Kills
dt2$Assists <- data$Assists
dt2$Damage <- data$Damage
dt2$DBNOs <- data$DBNOs
dt2$'Time Alive (min)' <- data$'Time Alive (min)'
dt2$WalkDist <- data$WalkDist
dt2$VehicleDist <- data$VehicleDist
dt2$Heals <- data$Heals
dt2$Boosts <- data$Boosts
dt2$Revives <- data$Revives



#### ANALYSES

# filter(data, Resultat=="Win")
# 
# ### Analyse de base: Moyenne d'un paramètre selon de type de victoire
# mean((filter(data, Resultat=="Win"))["Damage"][,1])
# 
# ## Les parties en Squad terminant dans le Top 10 (sans les victoires)
# filter(dt2, Resultat=="Top 10", Playmode=="Squad")
# 
# ## Consommation de meds nulle dans les parties gagnées
# filter(dt2, Resultat=="Win", Boosts==0, Heals==0)[c("Heals","Boosts", "Playmode")]
# 
# ####DUMP
# ### WIP - pas en fonction
# data$RatingAdd <- NULL##
# for(nb in sapply(str_extract_all((substr(data[,"Rating"],6,16)),"[0-9]*"), as.numeric)){
#   print(nb[4])
# }
# 
# 
# ####EXPLORATION####
# #pour explorer la page
# divs <- html_nodes(page_html, 'div')
# 
# ## Localisation du serveur & type
# divs[519]
# #compte le nombre de ptits bonhommes pour ce span
# length(html_nodes(divs[519], 'i'))
# 
# html_nodes(divs[518], 'div')
# #<span class="badge badge-region badge-region-na">na</span>
# 
# #Trouver les entrées qui impliquent >1 matches
# lapply(res, nchar)
# # nchar(res) > 8 implique "X matches"
