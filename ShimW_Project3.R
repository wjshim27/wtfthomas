# Name: WooJin Shim
# Course: 44-149 Scientific Computing
# Assignment # Project 3
# Due Date: April 13, 2018
# Brief: Using clusters to plot the census file
# By submitting this, I pledge that the code in this file was written by the author indicated above,
#  and that all assistance from outside sources was correctly attributed in comments.  Additionally, I
#  agree to abide by the rules expressed in the CSIS Academic Honesty Policy

census <- read.csv('us_census.csv')
contiguous <- census[!census$state %in% c('AK', 'HI', 'PR'), ]
plot(contiguous$longitude, contiguous$latitude, col = contiguous$state)
N <- 12
Iters <- 3
chosen_counties <- sample(1:nrow(contiguous), N)
lat <- contiguous[chosen_counties, 3]
long <- contiguous[chosen_counties, 4]
centers <- data.frame(lat, long)


dist_sq <- function(county, center) {
  deltax <- county['latitude'] - center[1]
  deltay <- county['longitude'] - center[2]
  deltax ^ 2 + deltay ^ 2
}

#deltax <- contiguous[1, 'latitude'] - centers[1,1]
#deltay <- contiguous[1, 'longitude'] - centers[1,2]
belongs_to <- rep(0, nrow(contiguous))
for (i in 1:Iters) {
  for (county in 1:nrow(contiguous)) {
    closest_center <- 1
    closest_distance <- dist_sq(contiguous[county, ], centers[1, ])
    for (cluster in 2:N) {
      d <- dist_sq(contiguous[county, ], centers[cluster, ])
      if (d < closest_distance) {
        closest_distance <- d
        closest_center <- cluster
      }
    }
    belongs_to[county] <- closest_center
  }
  plot(contiguous$longitude,contiguous$latitude, type = 'p', col = belongs_to)
  for (num in 1:N) {
    clust_of_interest <- contiguous[belongs_to == num, ]
    #print(sum(clust_of_interest$population))
    #print(nrow(cluster_of_interest))45
    total_pop <- sum(clust_of_interest$population)
    new_latitude <- sum(clust_of_interest$latitude * clust_of_interest$population) / total_pop
    new_longitude <- sum(clust_of_interest$longitude * clust_of_interest$population) / total_pop
    centers[num, 1] <- new_latitude
    centers[num, 2] <- new_longitude
  }
  plot(contiguous$longitude,contiguous$latitude, type = 'p', col = belongs_to)
}
