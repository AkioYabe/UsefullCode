ggJapanPrefecturesMap <- function(val) {
  require(ggplot2)
  shp <- sf::st_read(system.file("shapes/jpn.shp", package = "NipponMap")[1],quiet=T)
  #沖縄を上に持っていく
  shp$geometry[[47]][[1]][,1] <- shp$geometry[[47]][[1]][,1]+7
  shp$geometry[[47]][[1]][,2] <- shp$geometry[[47]][[1]][,2]+14
  
  g <- ggplot(shp)+
    geom_sf(aes(fill=val))+
    #沖縄のための線
    geom_line(data = data.frame(long = c(132, 135, 137, 137)
                                , lat = c(38, 38, 40, 43))
              ,mapping = aes(x = long, y = lat)
              ,colour = "black"
              ,size = 0.2)
  
  return(g)
}
