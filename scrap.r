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