library(geosphere)
# distance
distsq <- function(lon1, lon2, lat1, lat2){
  #(lon1-lon2)^2+(lat1-lat2)^2
  (distm(c(lon1, lat1), c(lon2, lat2), fun = distVincentyEllipsoid))^2
}

# summary for weights
sumidw <- function(distv){
  sum(sapply(distv, function(x) 1/x))
}

# weight by distance
idw <- function(lon, lat, lonv, latv, varv){
  n <- length(lonv)
  distv <- vector()
  nvarv <- vector()
  for(i in 1:n){
    distv[i] <- distsq(lon, lonv[i], lat, latv[i])
    nvarv[i] <- 1/distv[i] * varv[i]
  }
  return(sum(nvarv)/sumidw(distv))
}