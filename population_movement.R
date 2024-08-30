###### A model for a population moving around
###### 25/07/2024 AIMILIOS PASCHALIS KOUNIS

##### Dependencies ----
if(!"sp" %in% installed.packages()){
  install.packages("sp")}
library(sp)
if(!"magick" %in% installed.packages()){
  install.packages("magick")}
library(magick)
if(!"rgdal" %in% installed.packages()){
  install.packages("rgdal")}
library(rgdal)
if(!"raster" %in% installed.packages()){
  install.packages("raster")}
library(raster)
if(!"dismo" %in% installed.packages()){
  install.packages("dismo")}
library(dismo)
##### Functions ----

## Make a circle given x and y of a point and radius
circle.range <- function(x, y, radius){
  circle <- data.frame(
                       x = x + cos(1:360 * pi / 180) * radius,
                       y = y + sin(1:360 * pi / 180) * radius
                       )
  
  return(circle)
}

## Function for moving , looking for prefered value while avoiding avoided value

## a : dataset with starting position, MUST HAVE A COLUMN NAMED "time"
## x,y : character strings with names of coordinates in "a"
## distance : distance population can move per tick
## direction : number 1-8 (1 right, 3 up, 5 left, 7 down) showing cardinal directions
## time : number of ticks
## preferance : value in raster map that population is looking to move to
## avoid: value in raster map that population cant move to
## detection : how far around the population can detect the map (visibility)
## commitment : how long movement should be committed to a direction if pref not found in range

moving <- function(a, x, y, map, preferance, avoid, detection, distance, time, commitment){
  
  # Errors for wrong arguments
  if (is.data.frame(a) == F){
    stop("'a' must be a dataframe")
  }
  if (any(is.na(a$time)) == T | any(is.numeric(a$time) == F)){
    stop("'a' must have a 'time' column with numbers and no 'NA's")
  }
  if (is.character(x) == F | is.character(y) == F){
    stop("'x' and 'y' must be the name of variables contained in 'a' and must be in characters")
  }
  # I dont know how to make this work
  #  if (is.raster(map) == F){
  #    stop("'map' must be raster")
  # }
  if (is.numeric(preferance) == F){
    stop("'preferance' must be numeric")
  }
  if (is.numeric(avoid) == F){
    stop("'preferance' must be numeric")
  }
  if (is.numeric(detection) == F){
    stop("'detection' must be numeric")
  }
  if (is.numeric(distance) == F){
    stop("'distance' must be numeric")
  }
  if (is.numeric(time) == F){
    stop("'time' must be numeric")
  }
  if (is.numeric(commitment) == F){
    stop("'commitment' must be numeric")
  }
  
  # Map of preference
    map_pref <- map # Temp
    map_pref[map_pref[] != preferance] <- NA # Remove anything in map that is not pref
  # Set commitment timer to 0 (see random move)
    commitment_timer <- 0
  # Dataframe with circle angles
    compass <- data.frame(NE = 0:90, NW = 90:180, SW = 180:270, SE = 270:360)
    
  # Loop every tick
  for (i in 1:time) {
    real.time <- a[nrow(a), "time"] # Actual timestep, in case "time" column doesn't start at 0
    location <- SpatialPoints(coords = a[nrow(a), c(x,y)])
    range <- SpatialPolygons(
      list(
        Polygons(
          list(
            Polygon(circle.range(a[nrow(a), x], a[nrow(a), y], detection))
          ), 1
        )
      )
    ) # Create circle spatial polygon around current position with range as radius
    in_range <- mask(map_pref, range) # Get pref cells inside the range polygon
   
    if(all(is.na(in_range[])) == F){  ## Move if at least one pref in range
      
      ### !!!(Will go on impassable FIX AT SOME POINT)!!! ###
      
      in_range.dist <- distanceFromPoints(in_range, location) # Get distance of all cells in range
      found.pref <- which((in_range == preferance)[]) # Get cell IDs of cells with pref in range
      dist.pref <- in_range.dist[found.pref] # Get distances from location of cells with pref in range
      found.pref.closest.location <- coordinates(in_range[
                                                          found.pref[which.min(dist.pref)], drop = F
                                                            ]) # Get coordinates of closest cell with pref in range
      
      if (distance > min(dist.pref)){ # If pref point in movement range then move to it
        a[nrow(a) + 1, c(x,y)] <- c(
                                    found.pref.closest.location[1],
                                    found.pref.closest.location[2]
                                    ) # Move to closest pref cell
        a[nrow(a), "time"] <- real.time + 1 # Advance time
                                } 
      else{ # If pref point visible but outside movement range, move as close as possible
        a[nrow(a) + 1, c(x,y)] <- c(
                                    a[nrow(a), x] + cos(acos((found.pref.closest.location[1]-a[nrow(a), x])/min(dist.pref))*180/pi*pi/180) * distance,
                                    a[nrow(a), y] + sin(asin((found.pref.closest.location[2]-a[nrow(a), y])/min(dist.pref))*180/pi*pi/180) * distance
                                    ) # Move allowed distance
        a[nrow(a), "time"] <- real.time + 1 #Advance time
          }
                                }
    else{ ## If no pref in range, pick a direction, move a bit and search again
       distance_temp <- distance # Temp
       if (commitment_timer == 0){ # Commitment timer is zero on first loop or if moved towards picked distance "commitment" times
          
          if (exists("rand.card.back") == F){ # If backtrack direction hasn't been set, pick direction and find backtrack
              rand.card <- sample(compass,1) # Pick direction (0-90 deg, 90-180 deg, 180-270 deg, 270-360 deg) (directly up,left,right,down slightly more likely to be picked because of "compass" setup)
              if (all(rand.card <= 180)) {rand.card.back <- rand.card[,1] + 180} 
              if (all(rand.card >= 180)) {rand.card.back <- rand.card[,1] - 180} # Save opposite direction, a.k.a backtrack direction
          }
          else{ # If backtrack direction has been set, pick direction excluding backtrack and find backtrack of that
              rand.card <- sample(compass[- unique(which(compass[] == rand.card.back, arr.ind = T)[,2])],1)
              if (all(rand.card <= 180)) {rand.card.back <- rand.card[,1] + 180}
              if (all(rand.card >= 180)) {rand.card.back <- rand.card[,1] - 180}
          }
         
       commitment_timer <- commitment # Reset commitment timer
       }
       
       while (distance_temp > 0){ # As long as some movement left
         destination.check <- F
         avoid.check <- 0
         
         while(destination.check == F & avoid.check <= 10){ # Check that destination is valid and havent looped more than 10 times
         rand_dir <- sample(rand.card[,1],1) # Pick a random direction
         destination <- c(
                          pmax(pmin(coordinates(location)[1] + (distance/10) * cos(rand_dir*pi/180),xmax(map)),xmin(map)),
                          pmax(pmin(coordinates(location)[2] + (distance/10) * sin(rand_dir*pi/180),ymax(map)),ymin(map))
                         ) # Move 1/10th of distance random direction, if hit edges move to edge (pmin,pmax)
         
         # If destination not on "avoid" keep, otherwise pick again
         ifelse(test = extract(map, matrix(destination, nrow = 1, ncol = 2)) != avoid,
                yes = {a[nrow(a) + 1, c(x, y)] <- destination
                       distance_temp <- distance_temp - distance/10 # Reduce distance by distance covered

                ### !!!(If hit edges distance moved is not 1/10th FIX AT SOME POINT)!!! ###
                       
                       a[nrow(a), "time"] <-  real.time + 1 # Add row to time without advancing time
                       destination.check <-  T},
                no = {destination.check  <-  F 
                      avoid.check <- avoid.check + 1}
                )
         }
         if (avoid.check > 10){ # If checked 10 times and didnt find way to avoid, go back
           a[nrow(a) + 1, c(x, y)] <- a[nrow(a) - 1, c(x, y)]
           distance_temp <- distance_temp - distance/10
           a[nrow(a), "time"] <-  real.time + 1
         }
         
         
         ## Look again ##
         location <- SpatialPoints(coords = a[nrow(a), c(x,y)])
         range <- SpatialPolygons(
           list(
             Polygons(
               list(
                 Polygon(circle.range(a[nrow(a), x], a[nrow(a), y], detection))
               ), 1
             )
           )
         ) # Create circle spatial polygon around current position with range as radius
         in_range <- mask(map_pref, range) # Get pref cells in range polygon
         if(all(is.na(in_range[])) == F){ 
           in_range.dist <- distanceFromPoints(in_range, location) # Get distance of all cells in range
           found.pref <- which((in_range == preferance)[]) # Get cell IDs of cells with pref in range
           dist.pref <- in_range.dist[found.pref] # Get distances from location of cells with pref in range
           found.pref.closest.location <- coordinates(in_range[
                                                              found.pref[which.min(dist.pref)], 
                                                              drop = F
                                                              ]
                                                      ) # Get coordinates of closest cell with pref in range
           if (distance_temp > min(dist.pref)){
             a[nrow(a) + 1, c(x,y)] <- c(
               found.pref.closest.location[1],
               found.pref.closest.location[2]) # Move to closest pref cell
             distance_temp <- 0 # Deplete remaining distance
             a[nrow(a), "time"] <- real.time + 1 #Advance time
                                          }
           else{
             a[nrow(a) + 1, c(x,y)] <- c(
                                         a[nrow(a), x] + cos(acos((found.pref.closest.location[1]-a[nrow(a), x])/min(dist.pref))*180/pi*pi/180) * distance_temp,
                                         a[nrow(a), y] + sin(asin((found.pref.closest.location[2]-a[nrow(a), y])/min(dist.pref))*180/pi*pi/180) * distance_temp
                                         ) # Move allowed distance
             distance_temp <- 0 # Deplete remaining distance
             a[nrow(a), "time"] <- real.time + 1 #Advance time
                }
                                      } # End of found pref after random movement
                                          } # End of random movement
       commitment_timer <- commitment_timer - 1 # count down commitment timer
    }
  }
  return(a)
}


## Function for running "moving" multiple times, plotting movement line on map and saving as .png in "dest"

## dest : file path, character like "/MyFiles/Output"
## args : list of arguments for "moving" (list(a=.., x=..., y=..., etc.))

moving.save <- function(args, times, dest){
  for(i in 1:times){
   message(paste("Running",i,"/", times,"...", sep = " "))
   a <- do.call(moving, args = args)
   coordinates(a) <- ~lon+lat # Convert dataframe to spatial points
   proj4string(a) <- args$map@srs # Projection same as map
   
   list.of.lines <- list()
   for (j in 0:max(a@data$time)){
     line.temp <- coordinates(a[a$time == j,])
     line.temp <- rbind(line.temp, head(coordinates(a[a$time == j + 1,]), 1))
     list.of.lines[[j+1]] <- SpatialLines(list(Lines(list(Line(line.temp)),j)))
   }
   population_moved.lines <- do.call(rbind, list.of.lines)
   
   png(paste(dest , "/", i,".png", sep = ""))
   
   image(args$map, 
         col = rgb(0:(max(args$map[])-1)*2/(max(args$map[])*2),
                   0:(max(args$map[])-1)*2/(max(args$map[])*2),
                   0),
         main = paste("Looking for ", args$preferance , " | Avoiding ", args$avoid, sep = "")
         )
   
   plot(population_moved.lines,
        col = rgb(1:length(population_moved.lines)/length(population_moved.lines),
                  0,
                  length(population_moved.lines):1/length(population_moved.lines)),
        lwd = 2, add = T)
   
   dev.off()

   message(paste(i,"/", times,"completed.", sep = " "))
  }
}

## Function for running "moving" step by step, plotting movement line on map and saving as png in "dest"
moving.step <- function(args, times, dest){

    t <- args$time
    c <- args$a
  
    for(i in 1:times){
    
    message(paste("Running",i,"/", times, "...", sep = " "))
    
    b <- c
    list.of.lines <- list()
    img.list <- NA
    
    for(o in 1:t){
    args$time <- 1
    args$a <- b
    a <- do.call(moving, args = args)
    b <- a
    
    coordinates(a) <- ~lon+lat # Convert dataframe to spatial points
    proj4string(a) <- args$map@srs # Projection same as map
      line.temp <- coordinates(a[a$time == o,])
      line.temp <- rbind(tail(coordinates(a[a$time == o - 1,]), 1), line.temp)
      list.of.lines[[o]] <- SpatialLines(list(Lines(list(Line(line.temp)),o)))
    population_moved.lines <- do.call(rbind, list.of.lines)
    
    dir.create(paste(dest, "/Gif", i, "/", sep = ""), showWarnings = F)
    png(paste(paste(dest, "/Gif", i, "/", sep = "") ,i,"_",o, ".png", sep = ""))
    
    image(args$map, 
          col = rgb(0:(max(args$map[])-1)*2/(max(args$map[])*2),
                    0:(max(args$map[])-1)*2/(max(args$map[])*2),
                    0),
                    main = paste("Looking for ", args$preferance , 
                       " | Avoiding ", args$avoid, 
                       " | Timestep ", o, sep = "")
    )
    
    plot(population_moved.lines,
         col = rgb(1:length(population_moved.lines)/length(population_moved.lines),
                   0,
                   length(population_moved.lines):1/length(population_moved.lines)),
         lwd = 2, add = T)
    
    dev.off()
    
    img.list[o] <- paste(paste(dest, "/Gif", i, "/", sep = "") ,i,"_",o, ".png", sep = "")
    
    message(paste("Tick", o ,"/", t, "completed.", sep = " "))
    }
    
    img.joined <- image_join(image_read(img.list))
    img.animated <- image_animate(img.joined, fps = 5)
    image_write(image = img.animated,
                path = paste(dest,"/GIF", i, ".gif", sep = ""))
    
    message(paste(i,"/", times, "completed.", sep = " "))
  }
}
