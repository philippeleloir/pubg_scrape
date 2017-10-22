// scrape2.js pour utiliser phantomjs@pubg.me/player/VissGames/matches
var page = new WebPage();
var fs = require('fs');
var urlv = []

//generation de la liste url
// definir j selon le nombre de pages a telecharger
for (var j=1; j<6; j++) { 
  var s = "https://pubg.me/player/VissGames/matches?page=" + j;
  urlv.push(s)
}

var urls = urlv

function handle_page(url){
    page.open(url, function(){
        fs.write(""+url.substring(41,49)+".html", page.content, 'w');
        next_page();
    });
}

function next_page(){
    var url = urls.shift();
    if(!url){
        phantom.exit(0);
    }
    setTimeout(function() {
        handle_page(url)
    }, 1113);
}

next_page();
