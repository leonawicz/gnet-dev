
# @knitr setup_data
setwd("/workspace/UA/mfleonawicz/leonawicz/GlobalNetworks/workspaces")
#setwd("C:/leonawicz/GlobalNetworks/workspaces")

ap <- read.csv("../data/airports.dat", header=F, stringsAsFactors=F)
flights <- read.csv("../data/airline_routes.dat", header=F, stringsAsFactors=F)

names(ap) <- c("apID", "Name", "City", "Country", "IATA", "ICAO", "Lat", "Lon", "Alt", "TZ", "DST", "TZdb")
#names(al) <- c("Airline", "alID", "Name", "Alias", "IATA", "ICAO", "Callsign", "Country", "Active")
names(flights) <- c("Airline", "alID", "src_ap", "src_apID", "des_ap", "des_apID", "Codeshare", "Stops", "Equipment")

ind.src <- match(flights$src_apID, ap$apID)
ind.des <- match(flights$des_apID, ap$apID)
ind.drp <- which(is.na(ind.src) | is.na(ind.des))
ind.src <- ind.src[-ind.drp]
ind.des <- ind.des[-ind.drp]
flights <- flights[-ind.drp,]

flights$src_city <- ap$City[ind.src]
flights$src_country <- ap$Country[ind.src]
flights$src_lon <- ap$Lon[ind.src]
flights$src_lat <- ap$Lat[ind.src]
flights$src_alt <- ap$Alt[ind.src]

flights$des_city <- ap$City[ind.des]
flights$des_country <- ap$Country[ind.des]
flights$des_lon <- ap$Lon[ind.des]
flights$des_lat <- ap$Lat[ind.des]
flights$des_alt <- ap$Alt[ind.des]

flights <- flights[, -c(3:9)]

# @knitr distances
library(parallel)
n.cores=32
n <- nrow(flights)
sq <- round(seq(1, n+1, length.out=n.cores + 1))

dm <- function(i, x1y1, x2y2) distMeeus(x1y1[i,], x2y2[i,])
a <- cbind(flights$src_lon, flights$src_lat)
b <- cbind(flights$des_lon, flights$des_lat)
dst <- unlist(mclapply(1:n, dm, x1y1=a, x2y2=b, mc.cores=n.cores))

d.min <- 100
flights <- flights[dst >= d.min,]
a <- a[dst >= d.min,]
b <- b[dst >= d.min,]
dst <- dst[dst >= d.min]
n <- nrow(flights)
sq <- round(seq(1, n+1, length.out=n.cores + 1))

# @knitr direction
# reduce data frame, table unique paths and count flights in both directions as one path
z1 <- apply(cbind(a, b), 1, paste, collapse=" ")
z1f <- factor(z1)
lz <- levels(z1f)
lz1 <- strsplit(levels(z1f), " ")
lz1 <- sapply(1:length(lz1), function(i, x) paste(x[[i]][3], x[[i]][4], x[[i]][1], x[[i]][2], collapse=" "), x=lz1)

ignore <- c()
for(i in 1:nlevels(z1f)){
	ind1 <- which(z1==lz[i])
	ind1 <- ind1[!(ind1 %in% ignore)]
	ignore <- c(ignore, ind1)
	ind2 <- which(z1==lz1[i])
	ind2 <- ind2[!(ind2 %in% ignore)]
	if(length(ind2)){
		z1[ind2] <- lz[i]
		ignore <- c(ignore, ind2)
	}
	print(i)
}
length(unique(z1))

freq <- table(z1)
ind <- match(names(freq), z1)
dst <- dst[ind]
n <- length(ind)
ab <- do.call(rbind, lapply(strsplit(names(freq), " "), as.numeric))

save(ab, dst, n, file="flights.RData")
