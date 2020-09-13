# GeoCodeTSP
Geocoding some addresses and then solving the Traveling Salesman Problem
---
title: "Geocoding & Traveling salesman"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Problem statement

A friend of mine needed to deliver care packages to a bunch of families. She had a group of volunteers, but was having a hard time sorting the addresses, because all she had was street address with zip code - of course, that's a mail delivery identifier, not an actual geo-identifier. While discussing her task at happy hour one night, I cried "Traveling Salesman Problem!" and said I could solve her problems and get to nerd out at the same time. 

I've substituted the home addresses with locations of Starbucks around Washington DC.

Limitation: distances are based on "as the crow flies" and not driving directions, so there might be some backtracking when on an actual road. 

## Set up

Loading so many libraries. Originally I tried to use the positionstack.com API tool, and eventually got frustrated and gave up.

```{r Startup, include=FALSE}
library(tidyverse)
library(httr)
library(RJSONIO)
library(jsonlite)
library(RCurl)
library(curl)
library(rvest)
library(lazyeval)
library(gsubfn)
library(sjlabelled)
library(tmaptools)
library(TSP)
library(tspmeta)
library(leaflet)
library(maps)
library(ggmap)

```

Reading in my data - in this case a very simple list of addresses from Starbucks.

```{r read some data}
local.dir <- "C:/Users/anoel1/Desktop/"

sbux <-read.csv(file=file.path(local.dir, "starbucks.csv"), stringsAsFactors = F, header=F)

#Make vector
list.bux<-sbux$V1
```


Used tmap:geocode_OSM (open streets map) to geocode the addresses.  

```{r TMap - Sbux}

#Geocode
sbux_tmaptools <- geocode_OSM(list.bux, details = TRUE, as.data.frame = TRUE)


# extracting from the result only coordinates and address, give it a faux store number

sbux_tmaptools <- sbux_tmaptools[, c("query","lat", "lon", "display_name")]
sbux_tmaptools <- sbux_tmaptools %>%
  mutate(store_number = rownames(sbux_tmaptools))

```
24 out of 25 were successful here, which was good enough for the illustration.


At this point, it was really enough for my friend's purpose - she could see the families on a map, draw a circle around them, and hand it out to her volunteers. 
```{r mapping the basic geocode, echo = FALSE}
leaflet() %>% 
   addTiles() %>% 
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  addCircleMarkers(data = sbux_tmaptools, lng = ~lon, lat = ~lat, popup = ~query)

```


But why stop there if you can be extra?

## Traveling Salesman Problem

First, we're going to strip the data down to just Latitude & Longitude, and make a matrix, with the distance between all the points.
```{r Matrix for TSP}
coords.df <- data.frame(long=sbux_tmaptools$lon, lat=sbux_tmaptools$lat)
coords.mx <- as.matrix(coords.df)
 
# Compute distance matrix
dist.mx <- dist(coords.mx)
```

Nerding out about combinatorics: At the beginning, Traveling Salesman Problem (TSP)  is variant on "the handshake problem" (that is, if you have N people in a room, how many handshakes occur for everyone to shake hands with every other person). In TSP, that's from any location, how many trips to go from every city to every other city? Except we won't go to every city, we'll go to the nearest city. 

With 24 locations, that's 
$$
\frac{n * (n-1)}{2} = \frac{24*23}{2}=\frac{552}{2} = 276
$$

If we jumped to all 81 Starbii in DC, it becomes
$$
\frac{81*80}{2}=\frac{6480}{2} = 3240
$$
Might take some time to compute. 

Fill in the matrix for our 24 Starbii.
```{r TSP Construction}
# Construct a TSP object
tsp.ins <- tsp_instance(coords.mx, dist.mx )
```

We get an error because there's two Starbii with the same address -- it happens to be two stores inside of a single shopping mall. 


And finally, solve
```{r Solve the TSP}
tour <- run_solver(tsp.ins, method="2-opt")
```
Used 2-opt (https://en.wikipedia.org/wiki/2-opt) because that's what the sample code online had used. Worth trying some other methods if it were a bigger "real" problem. 


You can plot these results, but I didn't find them user friendly:
```{r plot the solve}
autoplot(tsp.ins, tour) 
```


So, now we're going to clean up the "Tour" data, and merge it back onto store information.
```{r Cleaning up the output}
tour_order <- as.integer(tour)
df.tour<- as.data.frame(tour_order)

id <- rownames(df.tour)
df.tour <- cbind(id=as.numeric(id), df.tour)

df.tour<- df.tour %>%
  rename(stop_number=id,
         store_number = tour_order)

sbux.order <- merge(sbux_tmaptools, df.tour, by=c("store_number"))

```

And now, output the map, with a popup of addresses and a label showing the order in which they should be visited. 
```{r map with leaflet}
leaflet() %>% 
  addTiles() %>% 
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  addCircleMarkers(data = sbux.order, lng = ~lon, lat = ~lat, popup = ~query) %>%
  addMarkers(data = sbux.order, lng = ~lon, lat = ~lat, label = ~stop_number, labelOptions = labelOptions(noHide = T))

```


And finally, output the list, with the stop_number added in. Going back to my friend's purpose, from here she chunked off the list of addresses based number per volunteer, essentially sending volunteers to different neighborhoods for delivery. 

```{r Output the lat/long}

write.csv(sbux.order, file=file.path(local.dir, "sbux geocodes.csv"), row.names = F)
```


## Multiple Salespeople (mTSP)

So, technically my friend has a multiple TSP problem. If she has 5 volunteers, and she wants to make sure each one has to travel approximately equal distance, then the Volunteer heading to Starbucks at 5335 Wisconsin Ave Nw (far NW) or 2845 Alabama Ave SE (far SE) probably won't have too many other stops. 

Further, everyone in my friend's case starts at position 1 (supply depot) and fans out. Her volunteers are good natured, so she didn't have hard constraints you might in other situations (no more than given miles, no more than given stops.)

