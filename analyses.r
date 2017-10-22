## quelques analyses des donnees recuperees

#### ANALYSES

filter(data, Resultat=="Win")

### Analyse de base: Moyenne d'un paramètre selon de type de victoire
mean((filter(data, Resultat=="Win"))["Damage"][,1])

## Les parties en Squad terminant dans le Top 10 (sans les victoires)
filter(dt2, Resultat=="Top 10", Playmode=="Squad")

## Consommation de meds nulle dans les parties gagnées
filter(dt2, Resultat=="Win", Boosts==0, Heals==0)[c("Heals","Boosts", "Playmode")]

####EXPLORATION####
#pour explorer la page
divs <- html_nodes(page_html, 'div')

## Localisation du serveur & type
divs[519]
#compte le nombre de ptits bonhommes pour ce span
length(html_nodes(divs[519], 'i'))

html_nodes(divs[518], 'div')
#<span class="badge badge-region badge-region-na">na</span>

#Trouver les entrées qui impliquent >1 matches
lapply(res, nchar)
# nchar(res) > 8 implique "X matches"
