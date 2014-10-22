double.escape<-function(textvector) {
  text.out<-gsub('"','\"',textvector)
  text.out<-gsub("'","\\\\'",text.out)
  text.out<-paste("\'",text.out,"\'",sep="")
  return(text.out)
}

getLabel <- function (array, strLabel) {
  if (is.null(array)) return (NULL)
  else return (strLabel)
}

getAttributeValue <- function (label, array=NULL, constant=NULL) {
  if (!is.null(array)) return (paste0("feature.properties.",label))
  else if (!is.null(constant)) {
    if (is.logical(constant)) {
      if (constant) return ("true")
      else return ("false")
    } else return (constant)
  } else return (NULL)
}

addAttribute <- function (strAttributesList, label, array=NULL, constant=NULL) {
  strNewAttributeValue = getAttributeValue(label, array, constant)
  
  if (is.null(strNewAttributeValue)) return (strAttributesList)
  else if (is.null(strAttributesList)) paste0(label,":",strNewAttributeValue)
  else paste0(strAttributesList,", ",label,":",strNewAttributeValue)
}

#' r.addByWord
#' @export
r.addByWord <- function(listText, strWord, ind=NULL, ignore.case=TRUE) {
  if (is.null(ind) || length(ind)==0) {
    if (length(strWord)==0) return (NULL)
    if (length(strWord)==1) return (which(grepl(strWord, listText, ignore.case=ignore.case)))
    else  return (union(which(grepl(strWord[1], listText, ignore.case=ignore.case)),r.addByWord(listText, strWord[2:length(strWord)])))
  } else {
    if (length(strWord)==0) return (ind)
    if (length(strWord)==1) return (union(ind,which(grepl(strWord, listText, ignore.case=ignore.case))))
    else  return (union(ind,union(which(grepl(strWord[1], listText, ignore.case=ignore.case)),r.addByWord(listText, strWord[2:length(strWord)]))))
  }
}

#' r.map.getProviderList
#' @export
r.map.getProviderList <- function(type=NULL) {
  if (is.null(type)) return (c("OpenStreetMap.Mapnik","OpenStreetMap.BlackAndWhite","OpenStreetMap.DE","OpenStreetMap.HOT","Thunderforest.OpenCycleMap","Thunderforest.Transport","Thunderforest.Landscape","Thunderforest.Outdoors","OpenMapSurfer.Roads","OpenMapSurfer.Grayscale","Hydda.Full","Hydda.Base","MapQuestOpen.OSM","MapQuestOpen.Aerial","Stamen.Toner","Stamen.TonerBackground","Stamen.TonerLite","Stamen.Terrain","Stamen.TerrainBackground","Stamen.Watercolor","Esri.WorldStreetMap","Esri.DeLorme","Esri.WorldTopoMap","Esri.WorldImagery","Esri.WorldTerrain","Esri.WorldShadedRelief","Esri.WorldPhysical","Esri.OceanBasemap","Esri.NatGeoWorldMap","Esri.WorldGrayCanvas","HERE.normalDay","HERE.normalDayCustom","HERE.normalDayGrey","HERE.normalDayMobile","HERE.normalDayGreyMobile","HERE.normalDayTransit","HERE.normalDayTransitMobile","HERE.normalNight","HERE.normalNightMobile","HERE.normalNightGrey","HERE.normalNightGreyMobile","HERE.carnavDayGrey","HERE.hybridDay","HERE.hybridDayMobile","HERE.pedestrianDay","HERE.pedestrianNight","HERE.satelliteDay","HERE.terrainDay","HERE.terrainDayMobile","Acetate.basemap","Acetate.terrain","Acetate.all","Acetate.hillshading","FreeMapSK","MtbMap","OpenMapSurfer.AdminBounds","Hydda.RoadsAndLabels","Stamen.TonerHybrid","Stamen.TonerLines","Stamen.TonerLabels","OpenWeatherMap.Clouds","OpenWeatherMap.CloudsClassic","OpenWeatherMap.Precipitation","OpenWeatherMap.PrecipitationClassic","OpenWeatherMap.Rain","OpenWeatherMap.RainClassic","OpenWeatherMap.Pressure","OpenWeatherMap.PressureContour","OpenWeatherMap.Wind","OpenWeatherMap.Temperature","OpenWeatherMap.Snow","Acetate.foreground","Acetate.roads","Acetate.labels"))
#   else if (tolower(type)=="favorite") return (c("OpenStreetMap.BlackAndWhite","OpenStreetMap.DE","Hydda.Full","Hydda.Base","Stamen.Toner","Stamen.Watercolor","Esri.WorldStreetMap","Esri.DeLorme","Esri.WorldTopoMap","Stamen.TonerHybrid","Stamen.TonerLines","HERE.terrainDay","HERE.normalDayGrey","HERE.normalNightGrey","HERE.pedestrianNight","HERE.satelliteDay","Acetate.terrain","Acetate.all","MtbMap"))
  else if (tolower(type)=="favorite") return (c("OpenStreetMap.BlackAndWhite","OpenStreetMap.DE","Stamen.Toner","Stamen.Watercolor","Esri.WorldStreetMap","Esri.DeLorme","Esri.WorldTopoMap","Stamen.TonerHybrid","Stamen.TonerLines","Acetate.terrain","Acetate.all"))
  return (c("OpenStreetMap.Mapnik","OpenStreetMap.BlackAndWhite","OpenStreetMap.DE","OpenStreetMap.HOT","Thunderforest.OpenCycleMap","Thunderforest.Transport","Thunderforest.Landscape","Thunderforest.Outdoors","OpenMapSurfer.Roads","OpenMapSurfer.Grayscale","Hydda.Full","Hydda.Base","MapQuestOpen.OSM","MapQuestOpen.Aerial","Stamen.Toner","Stamen.TonerBackground","Stamen.TonerLite","Stamen.Terrain","Stamen.TerrainBackground","Stamen.Watercolor","Esri.WorldStreetMap","Esri.DeLorme","Esri.WorldTopoMap","Esri.WorldImagery","Esri.WorldTerrain","Esri.WorldShadedRelief","Esri.WorldPhysical","Esri.OceanBasemap","Esri.NatGeoWorldMap","Esri.WorldGrayCanvas","HERE.normalDay","HERE.normalDayCustom","HERE.normalDayGrey","HERE.normalDayMobile","HERE.normalDayGreyMobile","HERE.normalDayTransit","HERE.normalDayTransitMobile","HERE.normalNight","HERE.normalNightMobile","HERE.normalNightGrey","HERE.normalNightGreyMobile","HERE.carnavDayGrey","HERE.hybridDay","HERE.hybridDayMobile","HERE.pedestrianDay","HERE.pedestrianNight","HERE.satelliteDay","HERE.terrainDay","HERE.terrainDayMobile","Acetate.basemap","Acetate.terrain","Acetate.all","Acetate.hillshading","FreeMapSK","MtbMap","OpenMapSurfer.AdminBounds","Hydda.RoadsAndLabels","Stamen.TonerHybrid","Stamen.TonerLines","Stamen.TonerLabels","OpenWeatherMap.Clouds","OpenWeatherMap.CloudsClassic","OpenWeatherMap.Precipitation","OpenWeatherMap.PrecipitationClassic","OpenWeatherMap.Rain","OpenWeatherMap.RainClassic","OpenWeatherMap.Pressure","OpenWeatherMap.PressureContour","OpenWeatherMap.Wind","OpenWeatherMap.Temperature","OpenWeatherMap.Snow","Acetate.foreground","Acetate.roads","Acetate.labels"))
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

#' r.map.new
#' @export
r.map.new <- function(
  mapCenter = c(41.39,  2.16), 
  mapZoom = 13,
  mapTiles = "MapQuestOpen.OSM",
  enablePopover = FALSE,
  fullScreen = TRUE) {
  m <- Leaflet$new()
  m$setView(mapCenter, mapZoom)
  m$tileLayer(provider = mapTiles)
  m$enablePopover(enablePopover)
  m$fullScreen(fullScreen)  
  return (m)
}

#' r.map.addHeatMap
#' @export
r.map.addHeatMap <- function (m, lat, lon) {
  posJSON = toJSONArray2(cbind(lat, lon), json = F, names = F)
  m$addAssets(jshead = c("http://leaflet.github.io/Leaflet.heat/dist/leaflet-heat.js"))
  m$setTemplate(afterScript = sprintf(
    "
    <script>
    var addressPoints = %s;
    var heat = L.heatLayer(addressPoints).addTo(map);
    </script>
    ", rjson::toJSON(posJSON)
  ))
  return (m)
}

#' r.map.addGeoJSON
#' @export
r.map.addGeoJSON <- function (m, 
                              latitude, 
                              longitude, 
                              popup = NULL,
                              radius = NULL,
                              fillColor = NULL,
                              fillOpacity = NULL,
                              color = NULL,
                              opacity = NULL,
                              weight = NULL,
                              constantpopup = NULL,
                              constantradius = NULL,
                              constantfillColor = NULL,
                              constantfillOpacity = NULL,
                              constantcolor = NULL,
                              constantopacity = NULL,
                              constantweight = NULL
) {
  dataGEO = data.frame(cbind(
    latitude, 
    longitude, 
    popup,
    radius,
    fillColor,
    fillOpacity,
    color,
    opacity,
    weight
  ), stringsAsFactors=FALSE)
  names(dataGEO) = c(getLabel(latitude,"latitude"), 
                     getLabel(longitude,"longitude"),
                     getLabel(popup,"popup"),
                     getLabel(radius,"radius"),
                     getLabel(fillColor,"fillColor"),
                     getLabel(fillOpacity,"fillOpacity"),
                     getLabel(color,"color"),
                     getLabel(opacity,"opacity"),
                     getLabel(weight,"weight")
  )
  dataGEO = lapply(split(dataGEO, rownames(dataGEO)), as.list)
  
  if (is.null(popup) && is.null(constantpopup)) clickable = FALSE
  else clickable = TRUE
  
  if ((is.null(weight) || max(weight)==0) && (is.null(constantweight) || constantweight==0)) stroke = FALSE
  else stroke = TRUE
  
  strOnEachFeature = "#! function(feature, layer){layer.bindPopup(feature.properties.popup)} !#"
  
  strAtributes = NULL
  strAtributes = addAttribute(strAtributes, "radius", radius, constantradius)
  strAtributes = addAttribute(strAtributes, "fillColor", fillColor, constantfillColor)
  strAtributes = addAttribute(strAtributes, "fillOpacity", fillOpacity, constantfillOpacity)
  strAtributes = addAttribute(strAtributes, "color", color, constantcolor)
  strAtributes = addAttribute(strAtributes, "opacity", opacity, constantopacity)
  strAtributes = addAttribute(strAtributes, "weight", weight, constantweight)
  if (!clickable) strAtributes = addAttribute(strAtributes, "clickable", NULL, clickable)
  if (!stroke) strAtributes = addAttribute(strAtributes, "stroke", NULL, stroke)
  #   print(strAtributes)
  
  strPointToLayer = paste0(
    "#! function(feature, latlng){"
    ,"          return L.circleMarker(latlng, {"
    ,strAtributes
    ,"})"
    ,"  } !#")
  
  if (!is.null(popup) && clickable)
    m$geoJson(toGeoJSON(dataGEO), 
              onEachFeature = strOnEachFeature,
              pointToLayer = strPointToLayer)  
  else
    m$geoJson(toGeoJSON(dataGEO), 
              pointToLayer = strPointToLayer)  
  
  return (m)
}

#' r.map.addLegend
#' @export
r.map.addLegend <- function (m, labels, colors, position = 'bottomright') {
  m$legend(position = position, 
           colors   =  colors, 
           labels   =  labels)
  return (m)
}