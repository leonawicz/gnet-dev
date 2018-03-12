library(geosphere)

adjustExtrema <- function(p){
	n <- length(p)
	for(i in 1:n){
		bbx <- p@bbox[1,]
		bby <- p@bbox[2,]
		if(any(bbx <= -180 | bbx >= 180 | bby <= -90 | bby >= 90)){
			for(j in 1:length(p[i,]@polygons)){
				for(k in 1:length(p[i,]@polygons[[j]]@Polygons)){
					x <- x.hold <- p[i,]@polygons[[j]]@Polygons[[k]]@coords
					x[x[,1] >= 180,1] <- 179.9
					x[x[,1] <= -179.9,1] <- -179.9
					x[x[,2] >= 90,2] <- 89.9
					x[x[,2] <= -90,2] <- -89.9
					if(!is.logical(all.equal(x, x.hold))){
						pg.list <- p[i,]@polygons[[j]]@Polygons
						pg.list[[k]] <- Polygon(x)
						pgs.list <- p[i,]@polygons
						pgs.list[[j]] <- Polygons(pg.list, ID=pgs.list[[1]]@ID)
						SPs <-  SpatialPolygons(pgs.list)
						d <- p[i,]@data
						rownames(d) <- pgs.list[[1]]@ID
						SPDF <- SpatialPolygonsDataFrame(SPs, d)
						proj4string(SPDF) <- CRS(proj4string(p))
						if(i > 1) ind1 <- 1:(i-1) else ind1 <- NULL
						if(i < n) ind2 <- (i+1):n else ind2 <- NULL
						if(n==1){
							p <- SPDF
						} else {
							if(!is.null(ind1)) SPDF <- rbind(p[ind1,], SPDF)
							if(!is.null(ind2)) SPDF <- rbind(SPDF, p[ind2,])
							p <- SPDF
						}
					}
				}
			}
		}
	}
	p
}

m <- getMap("coarse")
sapply(1:length(m), function(i, x) rownames(x[i,]@data), x=m)
d <- m@data
rownames(d) <- sapply(slot(m, "polygons"), slot, "ID")
m <- SpatialPolygonsDataFrame(m, d)
m <- adjustExtrema(m)

p <- makePoly(m, interval=100, r=1, sp=FALSE)

library(rgl)
library(sphereplot)

bg3d("black")
rgl.clear(type="lights")
plot3d(df,col=1,type="s", radius=rad, box=F, axes=F)
plot3d(sph2car(long=pts$Longitude, lat=pts$Latitude, radius=rad, deg=T), col="white", add=T, type = "p", size=1)

n.max <- 100

for(i in 7){
	lc <- lapply(p[i,]@polygons[[1]]@Polygons, coordinates)
	for(j in 1:length(lc)){
		x <- sph2car(long=lc[[j]][,1], lat=lc[[j]][,2], radius=rad, deg=T)
		x <- x[unique(round(seq(1, nrow(x), length=n.max))),]
		polygon3d(x, col="yellow", add=T, lwd=2, alpha=1, partial=T, random=T)
		lines3d(x, col="dodgerblue", add=T, lwd=1, alpha=1)
	}
	print(i)
}


