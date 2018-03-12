comargs <- (commandArgs(TRUE))
if(!length(comargs)) q("no") else for(i in 1:length(comargs)) eval(parse(text=comargs[[i]]))

if(interactive()) ID <- 1

# @knitr setup_gridlines_seq_batch
# grid lines sequence frame batch
setwd("/workspace/UA/mfleonawicz/leonawicz/GlobalNetworks/workspaces")
#setwd("C:/leonawicz/GlobalNetworks/workspaces")

library(parallel)
library(raster)
library(prodlim)

source("../code/grid_seq_functions.R")
source("../code/path_seq_functions.R")

name <- "gridlines_sequence2"

# important values
grid.points.factor <- 1 # keep small for higher resolution grids
landmask <- TRUE # no need to mask if raster already continents-only
recursion.level <- 39 # require deeper recursion for higher resolution grids, longer random paths requires fewer paths.
n <-200 # serial starting points (lapply), more = slower
n.cores <- 32 # parallel wrapper of lapply (mclapply)
full.extend <- TRUE # extend raster to global extent
agg <- FALSE # aggregate raster to coarser resolution
agg.fac <- 2 # aggregation factor for rows and columns
rot <- TRUE # If raster rotation from PC lat lon to standard WGS84 required
set.seed(47 + ID)

# Choose an imput tif template raster
#b <- brick("../data/tas_Amon_GFDL-CM3_rcp60_r1i1p1_200601-210012.tif") # res 2.5 x 2
b <- brick("../data/tas_mean_C_AR5_GFDL-CM3_rcp60_365_2006.tif") # res 2.5 x 2.5
#b <- brick("../data/tas_mean_C_AR5_GFDL.CM3_rcp60_01_2006.tif") # res ~ 0.167 x 0.167 (10-minute res). This is way to fine for this process.

if(xmin(b) < 0 & xmax(b) < 360) b <- extend(b, c(xmin(b), xmax(b) + res(b)[1], ymin(b), ymax(b)))
r <- subset(b, 1)

if(xmin(r) < 0 & all(is.na(r[ncol(r)]))){
	r[,ncol(r)] <- r[,1]
	r <- resample(r, raster(extent(0, 360, -90, 90), nrow=nrow(r)-1, ncol=ncol(r)-1))
}	
if(rot) r <- rotate(r) # rotate from PC Lat Lon to standard WGS84
if(full.extend) r <- extend(r, c(-180, 180, -90, 90))
if(agg) r <- aggregate(r, 2) # optional aggregation to coarser resolution

if(landmask){
	library(rworldmap)
	spdf <- getMap()
	r <- mask(r, spdf)
}
m <- as.matrix(r)
m <- t(m[nrow(m):1,])

xy <- xyFromCell(r, which(!is.na(r[])))
a <- 0.5*res(r)
#image(x=seq(-180+a[1], 180-a[1], length=nrow(m)), y=seq(-90+a[2], 90-a[2], length=ncol(m)), m, xlim=c(-180,180), ylim=c(-90,90))
corners <- do.call(rbind, list(
	cbind(xy[,1] - a[1], xy[,2] - a[2]),
	cbind(xy[,1] + a[1], xy[,2] - a[2]),
	cbind(xy[,1] - a[1], xy[,2] + a[2]),
	cbind(xy[,1] + a[1], xy[,2] + a[2])
	)
)
corners <- corners[!duplicated(corners),]
#points(corners[,1], corners[,2], cex=0.2)

start.pts <- lapply(1:n.cores, function(i, x, n) x[sample(1:nrow(x), n, rep=TRUE),], x=corners, n=n)
system.time( grid.paths <- mclapply(1:n.cores, par_gridPaths, x.list=start.pts, m=corners, res=res(r), recur=recursion.level) )
grid.paths <- do.call(c, grid.paths)
if(grid.points.factor > 1) grid.paths <- lapply(grid.paths, interpGridPaths, fac=grid.points.factor)

sq <- round(seq(1, length(grid.paths)+1, length.out=n.cores + 1))
system.time( paths <- mclapply(1:n.cores, lines_df, sq=sq, lines.list=grid.paths, col.vec=paste0(sample(colorRampPalette(c("orange", "white"))(100), length(grid.paths), replace=TRUE), 25), mc.cores=n.cores) )
paths <- do.call(rbind, paths)
names(paths)[1:2] <- c("lon", "lat")

n.consec <- 60 # consecutive paths, defines length of time sequence and series of paths
n.concur <- 1024 # concurrent paths on map for a single time stamp
n.max.segs <- 10001 # maximum sequential segmanents to plot, if/once filled, get_path_seq() will break/exit

start <- Sys.time()
p.list <- mclapply(1:n.concur, get_path_seq, paths=paths, seg.size=50, n.consec=n.consec, n.max.segs=n.max.segs, rm.na.dl.brk=FALSE, mc.cores=n.cores)
file.out <- paste0("intermediary/", name, "_ws", ID, ".RData") #else paste0("intermediary/gridlines_sequence1_continuous_ws", ID, ".RData")
save(p.list, file=file.out)
end <- Sys.time()
difftime(end, start)
