double.escape<-function(textvector) {
  text.out<-gsub('"','\"',textvector)
  text.out<-gsub("'","\\\\'",text.out)
  text.out<-paste("\'",text.out,"\'",sep="")
  return(text.out)
}

#' r.leaflet
#' @export
r.leaflet<-function(
  mark.lat=NULL, mark.lng=NULL, mark.label=NULL,
  circle.lat=NULL, circle.lng=NULL,
  circle.label=NULL,
  circle.size=NULL,
  circle.stroke=FALSE, circle.color=NULL,
  circle.fillcolor=NULL, circle.opacity=NULL,
  map.height=480,
  map.lat=41.383531,
  map.long=2.178389,
  map.zoom=8,
  filename="mymap.html",
  openmap=FALSE,
  leafletcss="http://cdn.leafletjs.com/leaflet-0.5/leaflet.css",
  leafletjs="http://cdn.leafletjs.com/leaflet-0.5/leaflet.js",
  lgooglejs="https://raw.github.com/shramov/leaflet-plugins/master/layer/tile/Google.js",
  mgooglejs="https://maps.googleapis.com/maps/api/js?v=3.exp&sensor=false"
) {
  head.open<-"<head>"
  head.css<-paste("<link rel=\"stylesheet\" href=\"",leafletcss,"\" />",sep="")
  head.js1<-paste("<script src=\"",leafletjs,"\"></script>",sep="")
  head.js2<-paste("<script src=\"",lgooglejs,"\"></script>",sep="")
  head.close<-"</head>"
  
  body.open<-"<body>"
  div.open<-paste("<div id=\"map\" style=\"height:",
                  map.height,
                  "px;\"><script type=\"text/javascript\">", sep="")
  div.view<-paste("var map = L.map(\'map\').setView([",
                  map.lat,",",map.long,"],",map.zoom,");", sep="")  
  div.tile<-"L.tileLayer(\'http://{s}.tile.osm.org/{z}/{x}/{y}.png\', { attribution: \'&copy; <a href=\"http://osm.org/copyright\">OpenStreetMap</a> contributors\' }).addTo(map);"
  if (!missing(mark.lat) && !is.null(mark.lat)
      && !missing(mark.lng) && !is.null(mark.lng)) {
    if (!missing(mark.label) && !is.null(mark.label)) {
      bindpopup<-paste(".bindPopup(",double.escape(mark.label),")",sep="")
    } else {
      bindpopup<-""
    }
    div.mark<-paste("L.marker([",
                    mark.lat,
                    ",",
                    mark.lng,
                    "]).addTo(map)",
                    bindpopup,
                    ";", sep="")
  } else {
    div.mark=""
  }
  if (!missing(circle.lat) && !is.null(circle.lat)
      && !missing(circle.lng) && !is.null(circle.lng)) {
    if (!missing(circle.label) && !is.null(circle.label)) {
      bindpopup<-paste(".bindPopup(",double.escape(circle.label),")",sep="")
    } else {
      bindpopup<-""
    }  
    if (circle.stroke) {
      strStroke = paste0("color: \'",
                      circle.color,"\'",
                      ",")
    } else {
      strStroke = "stroke:false,"      
    }
    div.circle<-paste0("L.circle([",
                      circle.lat,
                      ",",
                      circle.lng,
                      "], ",  
                      circle.size,
                      ", {",
                      strStroke,
                      "fillColor: \'",
                      circle.fillcolor,"\'",
                      ",",
                      "fillOpacity: ",
                      circle.opacity,
                      "}).addTo(map)",
                      bindpopup,
                      ";")
  } else {
    div.circle = ""
  }
  div.end<-"</script></div>"
  body.close<-"</body>"
  
  leaflet.text<-c(
    head.open,
    head.css,
    head.js1,
    head.js2,
    head.close,
    body.open,
    div.open,
    div.view,
    div.tile,
    div.mark,
    div.circle,
    div.end,
    body.close)
  conn<-file(filename)
  writeLines(leaflet.text,conn)  
  close(conn)
  if (openmap) {
    browseURL(filename)
  }
}
