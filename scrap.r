leaflet(l2) %>% 
  setView(-96, 37.8, 4) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(color=~pal(l2$DEP_DELAY_NEW),popup = paste("Average Delay is",round(l2$DEP_DELAY_NEW)))
.

figure() %>% ly_points(perDelay,Flow,data=g2a, hover=Description)
figure() %>% ly_points(perDelay,Flow,data=g2a50, hover=list(Description,Flow,perDelay))

finished=left_join(Halfflow,l2,by=c("ORIGIN","Code"))

content <- paste(sep = "<br/>",
                 "<b><a>l2$DISPLAY_AIRPORT_NAME</a></b>","
                 l2$DEP_DELAY_NEW","
                 l2$AIRPORT_ID")
leaflet(finished) %>% 
  setView(-96, 37.8, 4) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(color=~lalala,popup = popuplabels,radius = ~sqrt(finished$Flow))

lalala=colorBin("YlOrRd",finished$DEP_DELAY_NEW,n=5)

leaflet(finished) %>% 
  setView(-96, 37.8, 4) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(color=~pal(finished$DEP_DELAY_NEW),popup = popuplabels,radius = ~sqrt(finished$Flow)*0.01,opacity = 1,fillColor = ~pal(finished$DEP_DELAY_NEW),fillOpacity = 1)

leaflet(finished) %>% 
  setView(-96, 37.8, 4) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(color=~pal(finished$DEP_DELAY_NEW),popup = popuplabels,radius = ~sqrt(finished$Flow)*0.01)


May17flight %>%
  group_by(ORIGIN) %>%
  summarise(perDelay =   
              length(DEP_DELAY_NEW[DEP_DELAY_NEW<=15])/length(DEP_DELAY_NEW))



linedf=May17flight %>%
       group_by(ORIGIN,DEST) %>% 
       summarise(dep_delay_ = mean(DEP_DELAY_NEW,na.rm = T),distance = median(DISTANCE,na.rm = T)) %>% 
       left_join(AirportData_nd,by = c("ORIGIN" = "Code")) %>% 
       left_join(AirportData_nd,by = c("DEST" = "Code"))

linedf %>% 
  filter(ORIGIN=="JFK") %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(lat = "LATITUDE.y", lng = "LONGITUDE.y")

JFK1=linedf %>% 
  filter(ORIGIN=="JFK")
JFK=AirportData_nd %>% 
  filter(Code == "JFK")




col.1 <- adjustcolor("orange red", alpha=0.4)
col.2 <- adjustcolor("blue", alpha=0.9)
edge.pal <- colorRampPalette(c(col.1, col.2), alpha = TRUE)
edge.col <- edge.pal(50)

map("world", regions=c("usa"), fill=T, col="grey8", bg="grey15", ylim=c(21.0,50.0), xlim=c(-130.0,-65.0))

for (i in (1:dim(JFK1)[1])) { 
  inter=gcIntermediate(c(jfk$lon[1], jfk$lat[1]), c(JFK1$LONGITUDE.y[i], JFK1$LATITUDE.y[i]), n=100)
  edge.ind <-(JFK1[i,]$dep_delay_)
  range(edge.ind)
  lines(inter, lwd=edge.ind/30, col=edge.col[edge.ind])   
}










map("world", regions=c("usa"), fill=T, col="grey8", bg="grey15", ylim=c(21.0,50.0), xlim=c(-130.0,-65.0))

for (i in (1:dim(JFK1)[1])) { 
  inter=gcIntermediate(c(jfk$lon[1], jfk$lat[1]), c(JFK1$LONGITUDE.y[i], JFK1$LATITUDE.y[i]), n=100)
  edge.ind <- round(JFK1[i,]$dep_delay_) 
  lines(inter, lwd=0.1, col="turquoise2")   
}

edge.ind <- round(JFK1[i,]$dep_delay_) 



for (i in (1:dim(usairports)[1])) { 
  inter <- gcIntermediate(c(jfk$lon[1], jfk$lat[1]), c(usairports$lon[i], usairports$lat[i]), n=200)
  lines(inter, lwd=0.1, col="turquoise2")    
}

library(nycflights13)

mydf <- data.frame(Observation = c("A", "B"),
                   InitialLat = c(42.94111,33.94250),
                   InitialLong = c(-78.73639, -118.40806),
                   NewLat = c(40.63861, 40.63861),
                   NewLong = c(-73.77694, -73.77694),
                   stringsAsFactors = FALSE)

mydf2 <- data.frame(group = c("A", "B"),
                    lat = c(mydf$InitialLat, mydf$NewLat),
                    long = c(mydf$InitialLong, mydf$NewLong))
leaflet()%>%
  addTiles() %>%
  addPolylines(data = mydf2, lng = ~long, lat = ~lat, group = ~group)

  
leaflet(JFK1) %>% 
       addTiles() %>% 
       addMarkers(lat = JFK1$LATITUDE.y, lng = JFK1$LONGITUDE.y) %>% 
       addPolylines(lat = c(JFK1$LATITUDE.x,JFK1$LONGITUDE.x),lng = c(JFK1$LONGITUDE.x,JFK1$LONGITUDE.y))

library(geosphere)
library(leaflet)

lat_ny <- 40.73
lng_ny <- -73.9
lat_del <- 28.63
lng_del <- 77.21
lng_ca <- -121.6406
lat_ca <- 39.16414

inter1 <- gcIntermediate(c(lng_ny, lat_ny), c(lng_del, lat_del), n=10, addStartEnd=TRUE, sp = TRUE, breakAtDateLine = TRUE)
lines(inter1)

inter2 <- gcIntermediate(c(lng_ca, lat_ca), c(lng_del, lat_del), n=10, addStartEnd=TRUE, sp = TRUE, breakAtDateLine = TRUE)
lines(inter2)

inters <- c(inter1,inter2)

ll0 <- lapply( inters , function(x) `@`(x , "lines") )
ll1 <- lapply( unlist( ll0 ) , function(y) `@`(y,"Lines") )
Sl <- SpatialLines( list( Lines( unlist( ll1 ) , ID = 1 ) ) )

leaflet(Sl) %>% addTiles() %>% addPolylines()

library(leaflet)
library(geosphere)
gcIntermediate(c(5,52), c(-120,37),
               n=100, 
               addStartEnd=TRUE,
               sp=TRUE) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolylines()

