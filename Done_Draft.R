  # Required packages

install.packages("openxlsx")
install.packages("xlsx")
install.packages("maps")
install.packages("stringr")
install.packages("tidyr")
library(tidyr)
library(openxlsx)
library(xlsx)
library(maps)
library(stringr)

  # Function to feed the data to be clustered the data and return it

separate_data <- function(universe, zone, q = 14) {
  zone_data <- subset(universe, area_code == zone)
  zone_data <- zone_data[complete.cases(zone_data),]
  lat_long_data <- data.frame(LatLong = zone_data$LatLong)
  lat_long_data[,c("Latitude", "Longitude")] <- str_split_fixed(zone_data$LatLong, ",", 2)
  lat_long_data$Latitude  <- as.numeric(lat_long_data$Latitude) ; lat_long_data$Longitude <- as.numeric(lat_long_data$Longitude)
  lat_long_data$LocationID <- 1:nrow(lat_long_data)
  lat_long_data <- lat_long_data[,c(2,3,4)]
  lat_long_data <- lat_long_data[complete.cases(lat_long_data),]
  
  # Saying q/2. Currently no idea if the limit is q or above or below. q is subjected to variation.
  
  if(nrow(lat_long_data) %% q <= ceiling(q/2)) {
    k <- floor(nrow(lat_long_data)/q)
  }
  else {
    k <- ceiling(nrow(lat_long_data)/q)
  }
  
  # dirichletClusters_constrained function returns a list containing, "centers", "cluster", "LocationID",
  # "Latitude", "Longitude", "k", "iterations", "error.diff".
  
  clustered_data <- dirichletClusters_constrained(lat_long_data, k, max.iter=25, tolerance=.0001, plot.iter=TRUE)
  zone_data$cluster <- clustered_data$cluster
  zone_data <- zone_data[order(zone_data$cluster),]
  return(zone_data)
}

  # Function to convert to radians

as_radians <- function(theta = 0) {
  return(theta * pi / 180)
}

  # Function to calculate geographical distances

calc_dist <- function(fr, to) {
  lat1 = as_radians(fr$lat)
  lon1 = as_radians(fr$lon)
  lat2 = as_radians(to$lat)
  lon2 = as_radians(to$lon)
  a = 3963.191;
  b = 3949.903;
  numerator = ( a^2 * cos(lat2) )^2 + ( b^2 * sin(lat2) ) ^2
  denominator = ( a * cos(lat2) )^2 + ( b * sin(lat2) )^2
  radiusofearth = sqrt(numerator/denominator) #Accounts for the ellipticity of the earth.
  d = radiusofearth * acos( sin(lat1) * sin(lat2) + cos(lat1)*cos(lat2)*cos(lon2 - lon1) )
  d.return = list(distance_miles=d)
  return(d.return)
}

  # Function to cluster the data

dirichletClusters_constrained <- function(orig.data, k=5, max.iter =50, tolerance = 1, plot.iter=TRUE) {
  fr = to = NULL
  r.k.start = sample(seq(1:k))
  n = nrow( orig.data )
  k.size = ceiling(n/k)
  initial.clusters = rep(r.k.start, k.size)
  if(n%%length(initial.clusters)!=0){
    exclude.k = length(initial.clusters) - n%%length(initial.clusters)
  } else {
    exclude.k = 0
  }
  orig.data$cluster = initial.clusters[1:(length(initial.clusters)-exclude.k)]
  orig.data$cluster_original = orig.data$cluster
  ## Calc centers and merge
  mu = cbind( by(orig.data$Latitude, orig.data$cluster, mean), by(orig.data$Longitude, orig.data$cluster, mean), seq(1:k) )
  tmp1 = matrix( match(orig.data$cluster, mu[,3]) )
  orig.data.centers = cbind(as.matrix(orig.data), mu[tmp1,])[,c(1:2,4:6)]
  ## Calc initial distance from centers
  fr$lat = orig.data.centers[,3]; fr$lon = orig.data.centers[,4]
  to$lat = orig.data.centers[,1]; to$lon = orig.data.centers[,2]
  orig.data$distance.from.center = calc_dist(fr, to)$distance_miles
  orig.data$distance.from.center_original = orig.data$distance.from.center
  ## Set some initial configuration values
  is.converged = FALSE
  iteration = 0
  error.old = Inf
  error.curr = Inf
  while ( !is.converged && iteration < max.iter ) { # Iterate until threshold or maximum iterations
    if(plot.iter==TRUE){
      plot(orig.data$Longitude, orig.data$Latitude, col=orig.data$cluster, pch=16, cex=.6,
           xlab="Longitude",ylab="Latitude")
    }
    iteration = iteration + 1
    start.time = as.numeric(Sys.time())
    cat("Iteration ", iteration,sep="")
    for( i in 1:n ) {
      # Iterate over each observation and measure the distance each observation' from its mean center
      # Produces an exchange. It takes the observation closest to it's mean and in return it gives the observation
      # closest to the giver, k, mean
      fr = to = distances = NULL
      for( j in 1:k ){
        # Determine the distance from each k group
        fr$lat = orig.data$Latitude[i]; fr$lon = orig.data$Longitude[i]
        to$lat = mu[j,1]; to$lon = mu[j,2]
        distances[j] = as.numeric( calc_dist(fr, to) )
      }
      # Which k cluster is the observation closest.
      which.min.distance = which(distances==min(distances), arr.ind=TRUE)
      previous.cluster = orig.data$cluster[i]
      orig.data$cluster[i] = which.min.distance # Replace cluster with closest cluster
      # Trade an observation that is closest to the giving cluster
      if(previous.cluster != which.min.distance){
        new.cluster.group = orig.data[orig.data$cluster==which.min.distance,]
        fr$lat = mu[previous.cluster,1]; fr$lon = mu[previous.cluster,2]
        to$lat = new.cluster.group$Latitude; to$lon = new.cluster.group$Longitude
        new.cluster.group$tmp.dist = calc_dist(fr, to)$distance_miles
        take.out.new.cluster.group = which(new.cluster.group$tmp.dist==min(new.cluster.group$tmp.dist), arr.ind=TRUE)
        LocationID = new.cluster.group$LocationID[take.out.new.cluster.group]
        orig.data$cluster[orig.data$LocationID %in% LocationID] = previous.cluster
      }
    }
    # Calculate new cluster means
    mu = cbind( by(orig.data$Latitude, orig.data$cluster, mean), by(orig.data$Longitude, orig.data$cluster, mean), seq(1:k) )
    tmp1 = matrix( match(orig.data$cluster, mu[,3]) )
    orig.data.centers = cbind(as.matrix(orig.data), mu[tmp1,])[,c(1:2,4:6)]
    mu = cbind( by(orig.data$Latitude, orig.data$cluster, mean), by(orig.data$Longitude, orig.data$cluster, mean), seq(1:k) )
    ## Calc initial distance from centers
    fr$lat = orig.data.centers[,3]; fr$lon = orig.data.centers[,4]
    to$lat = orig.data.centers[,1]; to$lon = orig.data.centers[,2]
    orig.data$distance.from.center = calc_dist(fr, to)$distance_miles
    # Test for convergence. Is the previous distance within the threshold of the current total distance from center
    error.curr = sum(orig.data$distance.from.center)
    error.diff = abs( error.old - error.curr )
    error.old = error.curr
    if( !is.nan( error.diff ) && error.diff < tolerance ) {
      is.converged = TRUE
    }
    # Set a time to see how long the process will take is going through all iterations
    stop.time = as.numeric(Sys.time())
    hour.diff = (((stop.time - start.time) * (max.iter - iteration))/60)/60
    cat("\n Error ",error.diff," Hours remain from iterations ",hour.diff,"\n")
    # Write out iterations. Can later be used as a starting point if iterations need to pause
    write.table(orig.data, paste(iteration,"_data.csv", sep=""), sep=",", row.names=F)
  }
  centers = data.frame(mu)
  ret.val = list("centers" = centers, "cluster" = factor(orig.data$cluster), "LocationID" = orig.data$LocationID,
                 "Latitude" = orig.data$Latitude, "Longitude" = orig.data$Longitude,
                 "k" = k, "iterations" = iteration, "error.diff" = error.diff)
  return(ret.val)
}

  # Function to sample our data and return the final set

Sample_Compilation <- function(universe) {
  samples <- list()
  week <- list()
  week[[1]] <- data.frame(matrix(NA, ncol = 6, nrow = ceiling(3/5*nrow(universe))))
  colnames(week[[1]]) <- c("rest_id", "rest_name", 
                           "area_code", "cuisine", 
                           "cft", "orders" )
  
  # insert a function here to get create the "samples" list (a list with various combinations of dataframes)
  
  combinations <- combn(nrow(universe), 3/5 * nrow(universe))
  for(i in 1:ncol(combinations)) {
    samples[[i]] <- universe[combinations[,i],]
  }
  for(k in 2:5) {
    print(k)
    w <- 0
    samples_final <- list()
    for(l in 1:length(samples)) {
      if(nrow(merge(samples[[l]], week[[(k-1)]], by = "rest_id")) <= ceiling(0.3 * nrow(samples[[l]]))) {
        w <- w+1
        samples_final[[w]] <- samples[[l]]
      }
    }
    decider <- data.frame(matrix(NA, ncol = 6, nrow = length(samples_final)))
    colnames(decider) <- c("sample_number", "avg_order",
                           "no_of_cuisines", "abs_dev_mean_cuisines",
                           "no_of_cft", "abs_dev_mean_cft")
    for(i in 1:(length(samples_final))) {
      cuisines_append <- vector()
      cft_append <- vector()
      decider[i,"sample_number"] <- i
      decider[i,"avg_order"] <- mean(as.numeric(samples_final[[i]][,"orders"]))
      for(j in 1:nrow(samples_final[[i]])) {
        cuisines_append <- append(cuisines_append, unlist(strsplit(as.character(samples_final[[i]][j,"cuisine"]), ",")))
        cft_append <- append(cft_append, samples_final[[i]][j,"cft"])
      }
      unique_cuisines <- length(unique(cuisines_append))
      unique_cft <- length(unique(cft_append))
      decider[i,"no_of_cuisines"] <- unique_cuisines
      decider[i,"no_of_cft"] <- unique_cft
      cuisines_dataframe <- as.data.frame(table(cuisines_append))
      cft_dataframe <- as.data.frame(table(cft_append))
      cuisines_dataframe[,"abs_mean_dev"] <- abs(mean(cuisines_dataframe[,"Freq"]) - 
                                                   cuisines_dataframe[,"Freq"])
      cft_dataframe[,"abs_mean_dev"]      <- abs(mean(cft_dataframe[,"Freq"]) - 
                                                   cft_dataframe[,"Freq"])
      decider[i,"abs_dev_mean_cuisines"] <- sum(cuisines_dataframe[,"abs_mean_dev"])
      decider[i,"abs_dev_mean_cft"]      <- sum(cft_dataframe[,"abs_mean_dev"])
    }
    if(all(decider$abs_dev_mean_cuisines == 0)) {
      for(m in 1:nrow(decider)) {
        decider[m, "rating"] <- 0.55 * (decider[m, "avg_order"] / max(decider[, "avg_order"])) + 
                                0.30 * (0.6 * (decider[m, "no_of_cuisines"] / max(decider[, "no_of_cuisines"])) + 
                                        0.4 * 1) + 
                                0.15 * (0.6 * (decider[m, "no_of_cft"] / max(decider[, "no_of_cft"])) + 
                                        0.4 * ((max(decider[, "abs_dev_mean_cft"] - decider[m, "abs_dev_mean_cft"])) / 
                                               (max(decider[, "abs_dev_mean_cft"]) - min(decider[, "abs_dev_mean_cft"]))))
      }
    }
    else if(all(decider$abs_dev_mean_cft == 0)) {
      for(m in 1:nrow(decider)) {
        decider[m, "rating"] <- 0.55 * (decider[m, "avg_order"] / max(decider[, "avg_order"])) + 
                                0.30 * (0.6 * (decider[m, "no_of_cuisines"] / max(decider[, "no_of_cuisines"])) + 
                                        0.4 * ((max(decider[, "abs_dev_mean_cuisines"] - decider[m, "abs_dev_mean_cuisines"])) / 
                                               (max(decider[, "abs_dev_mean_cuisines"]) - min(decider[, "abs_dev_mean_cuisines"])))) + 
                                0.15 * (0.6 * (decider[m, "no_of_cft"] / max(decider[, "no_of_cft"])) + 
                                        0.4 * 1)
      }
    }
    else if(all(decider$abs_dev_mean_cuisines == 0) & all(decider$abs_dev_mean_cft == 0)) {
      for(m in 1:nrow(decider)) {
        decider[m, "rating"] <- 0.55 * (decider[m, "avg_order"] / max(decider[, "avg_order"])) + 
                                0.30 * (0.6 * (decider[m, "no_of_cuisines"] / max(decider[, "no_of_cuisines"])) + 
                                        0.4 * 1) + 
                                0.15 * (0.6 * (decider[m, "no_of_cft"] / max(decider[, "no_of_cft"])) + 
                                        0.4 * 1)
      }
    }
    else {
      for(m in 1:nrow(decider)) {
        decider[m, "rating"] <- 0.55 * (decider[m, "avg_order"] / max(decider[, "avg_order"])) + 
                                0.30 * (0.6 * (decider[m, "no_of_cuisines"] / max(decider[, "no_of_cuisines"])) + 
                                        0.4 * ((max(decider[, "abs_dev_mean_cuisines"] - decider[m, "abs_dev_mean_cuisines"])) / 
                                               (max(decider[, "abs_dev_mean_cuisines"]) - min(decider[, "abs_dev_mean_cuisines"])))) + 
                                0.15 * (0.6 * (decider[m, "no_of_cft"] / max(decider[, "no_of_cft"])) + 
                                        0.4 * ((max(decider[, "abs_dev_mean_cft"] - decider[m, "abs_dev_mean_cft"])) / 
                                               (max(decider[, "abs_dev_mean_cft"]) - min(decider[, "abs_dev_mean_cft"]))))
      }
    }
    print(decider)
    if(k %in% c(1,2) | nrow(decider) == 1) {
      best <- which.max(decider[, "rating"])
      samples_final[[best]]$week <- k - 1
      week[[k]] <- samples_final[[best]]
    }
    else {
      best <- decider[order(decider$rating, decreasing = TRUE),]$sample_number[2]
      samples_final[[best]]$week <- k - 1
      week[[k]] <- samples_final[[best]]
    }
  }
  return(week[2:5])
}

head(data)
clustered_data <- separate_data(data, 90, 14)
nrow(clustered_data)
head(clustered_data)
as.data.frame(table(clustered_data$cluster))
clustered_data_split <- split(clustered_data, clustered_data$cluster)
clustered_data_split
rajouri_data <- lapply(clustered_data_split, FUN = Sample_Compilation)
rajouri_data <- do.call(rbind, unlist(rajouri_data, recursive = FALSE))
rajouri_data <- rajouri_data[order(rajouri_data$week),]
write.csv(rajouri_data, "rajouri553015draft.csv")
lapply(split(rajouri_data, rajouri_data$week), nrow)
wr1 <- rajouri_data[rajouri_data$week %in% 1,]
wr2 <- rajouri_data[rajouri_data$week %in% 2,]
wr3 <- rajouri_data[rajouri_data$week %in% 3,]
wr4 <- rajouri_data[rajouri_data$week %in% 4,]
nrow(merge(wr4, wr3, by = "rest_id"))
nrow(merge(wr2, wr4, by = "rest_id"))
head(merge(wr1, wr2, by = "rest_id"))
head(merge(wr2, wr3, by = "rest_id"))
tail(merge(wr1, wr2, by = "rest_id"))
tail(merge(wr2, wr3, by = "rest_id"))
janakpuri_data <- lapply(clustered_data_split, FUN = Sample_Compilation)
janakpuri_data <- do.call(rbind, unlist(janakpuri_data, recursive = FALSE))
janakpuri_data <- janakpuri_data[order(janakpuri_data$week),]
write.csv(janakpuri_data, "janakpuri702010draft.csv")
lapply(split(janakpuri_data, janakpuri_data$week), nrow)
w1 <- janakpuri_data[janakpuri_data$week %in% 1,]
w2 <- janakpuri_data[janakpuri_data$week %in% 2,]
w3 <- janakpuri_data[janakpuri_data$week %in% 3,]
w4 <- janakpuri_data[janakpuri_data$week %in% 4,]
nrow(merge(w2, w4, by = "rest_id"))
head(merge(w2, w3, by = "rest_id"))
tail(merge(w1, w2, by = "rest_id"))
tail(merge(w2, w3, by = "rest_id"))
falcalz <- list()
zdtdd   <- list()
merge1 <- read.csv("rhi_june.csv")
merge2 <- read.csv("merge2j.csv")
merge3 <- read.csv("merge3j.csv")
merge23<- rbind(merge2, merge3)
tddata <- merge(merge1, merge23, by = "rest_id")
tddata
colnames(tddata)
tddata <- tddata[,c(1,4,5,11,47,48,51,52,55)]
colnames(tddata) <- c("rest_id", "rest_name", "area", "orders", "Locality", "area_code", "cuisine", "cft", "LatLong")
tddata <- tddata[complete.cases(tddata),]
for(i in 1:nrow(tddata)) {
  if(tddata$cft[i] <= 250) {
    tddata$cft[i] <- "<= 250"
  }
  else if(tddata$cft[i] > 250 & tddata$cft[i] <= 500) {
    tddata$cft[i] <- "> 250 & <= 500"
  }
  else if(tddata$cft[i] > 500 & tddata$cft[i] <= 1000) {
    tddata$cft[i] <- "> 500 & <= 1000"
  }
  else if(tddata$cft[i] > 1000 & tddata$cft[i] <= 1500) {
    tddata$cft[i] <- "> 1000 & <= 1500"
  }
  else if(tddata$cft[i] > 1500 & tddata$cft[i] <= 2500) {
    tddata$cft[i] <- "> 1500 & <= 2500"
  }
  else {
    tddata$cft[i] <- "> 2500"
  }
}
class(tddata$cft)
head(tddata)
nrow(tddata)
nrow(tddata[complete.cases(tddata),])
uacd <- unique(tddata$area_code)
for(i in 27:length(uacd)){
  print(uacd[i])
  cdln       <- separate_data(tddata, uacd[i], 15)
  cdfdfln    <- as.data.frame(table(cdln$cluster))
  for(j in 1:nrow(cdfdfln)) {
    if(cdfdfln$Freq[j] > 16) {
      lu     <- length(unique(cdln$cluster))
      cdlnsd <- separate_data(cdln[cdln$cluster %in% j,], uacd[i], 10)
      print(j)
      cdln   <- subset(cdln, cluster != j)
      levels(cdlnsd$cluster)[1:length(levels(cdlnsd$cluster))] <- c(j, seq((lu+1),(lu-1+length(unique(cdlnsd$cluster)))))
      cdln   <- rbind(cdln, cdlnsd)
      cdln   <- cdln[order(cdln$cluster),]
    }
  }
  repeat {
    cdfdfln  <- as.data.frame(table(cdln$cluster))
    if(nrow(cdfdfln) >= 2) {
      tcm      <- as.data.frame(t(combn(nrow(cdfdfln), 2)))
      colnames(tcm) <- c("r1", "r2")
      for(k in 1:nrow(tcm)) {
        tcm$rs[k] <- cdfdfln$Freq[tcm$r1[k]] + cdfdfln$Freq[tcm$r2[k]]
      }
      if(nrow(tcm[tcm$rs <= 16,]) == 0) {
        break()
      }
      levels(cdln$cluster)[c(tcm$r1[which.min(tcm$rs)], tcm$r2[which.min(tcm$rs)])] <- min(c(tcm$r1[which.min(tcm$rs)], tcm$r2[which.min(tcm$rs)]))
      levels(cdln$cluster)[1:length(unique(cdln$cluster))] <- 1:length(unique(cdln$cluster))
    }
    else {
      break()
    }
  }
  falcalz[[i]] <- cdln
  sdfal        <- split(falcalz[[i]], falcalz[[i]]$cluster)
  zdtdd[[i]]   <- lapply(sdfal, FUN = Sample_Compilation)
  zdtdd[[i]]   <- do.call(rbind, unlist(zdtdd[[i]], recursive = FALSE))
  zdtdd[[i]]   <- zdtdd[[i]][order(zdtdd[[i]]$week),]
}
lapply(falcalz, function(x) {as.data.frame(table(x$cluster))})

smsfo <- split(falcalz[[16]], falcalz[[16]]$cluster)
length(smsfo)
for(i in 1:length(smsfo)) {
  print(i)
  print(Sample_Compilation(smsfo[[i]]))
}
write.csv(do.call(rbind, zdtdd), "s44ztd.csv")
Sample_Compilation(smsfo[[1]])
smsfo[[8]]
write.csv(do.call(rbind, zdtdd), "s15ztd.csv")
as.data.frame(table(falcalz[[16]]$cluster))
falcalz[[16]][falcalz[[16]]$cluster %in% 1,]


lapply(split(falcalz[[3]], falcalz[[3]]$cluster), FUN = Sample_Compilation)

