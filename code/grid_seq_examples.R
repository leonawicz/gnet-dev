
# @knitr setup_grid_seq_2dmap
# 2D map grid sequence frames
setwd("/workspace/UA/mfleonawicz/leonawicz/GlobalNetworks/workspaces")
#setwd("C:/leonawicz/GlobalNetworks/workspaces")

library(parallel)
library(raster)
library(RColorBrewer)
library(rworldmap)

source("../code/grid_seq_functions.R")

n.cores <- 32
set.seed(47)

b <- brick("../data/tas_Amon_GFDL-CM3_rcp60_r1i1p1_200601-210012.tif")
r <- rotate(subset(b, 1))
#r <- aggregate(r, 2)

spdf <- getMap()
r <- mask(r, spdf)
m <- as.matrix(r)
m <- t(m[nrow(m):1,])

w <- 12; h <- w*(diff(c(-90, 90))/diff(c(-180,180)))
lat.lim <- c(-90,90)
lon.lim <- c(-180,180)
w <- 6000; h <- round(w*(diff(lat.lim)/diff(lon.lim)))

clrs <- lapply(c("Blues", "Greens", "Oranges", "Purples", "Reds", "YlOrBr"), function(i) colorRampPalette(rev(brewer.pal(9, i)))(100))

# @knitr grid_frames
# Generate frames
nn <- 360
ind <- paste0(rep(0:9, each=1000), rep(paste0(rep(0:9, each=100), rep(c(paste0(0,0:9), 10:99), 10)), 10))[1:nn]
set.seed(47)
size <- prod(dim(m))/nn

framesetID <- paste0("worldgrid1_temp", c("Horiz", "Vert", "RandCSR", "RandL2R", "RandB2T" ,"RandBL2TR"), "_1")
outDir <- c()
for(i in 1:length(framesetID)) dir.create(outDir[i] <- file.path("../plots/frames/grids", framesetID[i]), recursive=T, showWarnings=F)

frames.args <- fill.args <- list()
for(i in 1:length(framesetID)){
	frames.args[[i]] <- list(w=3000, h=1500, bg="transparent", clrs=clrs[[i]], alpha=NULL)
	
	fill.args[[i]] <- list(x=m, size=size)
	fill.args[[i]]$fill <- if(i %in% c(1,2)) "ordered" else "random"
	if(i==2) fill.args[[i]]$byrow <- TRUE
	if(i==4) fill.args[[i]]$prob.row <- (nrow(m):1)^5
	if(i %in% c(5,6)) fill.args[[i]]$prob.col <- (ncol(m):1)^5
	if(i==6) fill.args[[i]]$prob.row <- (nrow(m):1)^10
}

f <- function(i, set, Dir, fill.args, frames.args){
	set <- set[i]
	Dir <- Dir[i]
	fill.args <- fill.args[[i]]
	frames.args <- frames.args[[i]]
	frames.args$file <- file.path(Dir, "frame")
	fill.args$frames.args <- frames.args
	do.call(fill_grid, fill.args)
	print(paste("Set", i, "completed"))
}

mclapply(1:length(framesetID), f, set=framesetID, Dir=outDir, fill.args=fill.args, frames.args=frames.args, mc.cores=n.cores)

#### One at a time, same as above
# Ordered bottom to top, left to right
framesetID <- "worldgrid1_tempHoriz1"
dir.create(outDir <- file.path("../plots/frames", framesetID), recursive=T, showWarnings=F)
fill.args <- list(file=file.path(outDir, "frame"), w=3000, h=1500, bg="transparent", clrs=clrs, alpha=0.5)

fill_grid(m, size=size, frames.args=fill.args, fill="ordered")

# Ordered left to right, bottom to top
framesetID <- "worldgrid1_tempVert1"
dir.create(outDir <- file.path("../plots/frames", framesetID), recursive=T, showWarnings=F)
fill.args$file <- file.path(outDir, "frame")

fill_grid(m, size=size, frames.args=fill.args, fill="ordered", byrow=TRUE)

# CSR
framesetID <- "worldgrid1_tempRandCSR1"
dir.create(outDir <- file.path("../plots/frames", framesetID), recursive=T, showWarnings=F)
fill.args$file <- file.path(outDir, "frame")

fill_grid(m, size=size, frames.args=fill.args, fill="random", prob.row=NULL, prob.col=NULL)

# Left to right
framesetID <- "worldgrid1_tempRandL2R1"
dir.create(outDir <- file.path("../plots/frames", framesetID), recursive=T, showWarnings=F)
fill.args$file <- file.path(outDir, "frame")

fill_grid(m, size=size, frames.args=fill.args, fill="random", prob.row=(nrow(m):1)^5, prob.col=NULL)

# Bottom to top
framesetID <- "worldgrid1_tempRandB2T1"
dir.create(outDir <- file.path("../plots/frames", framesetID), recursive=T, showWarnings=F)
fill.args$file <- file.path(outDir, "frame")

fill_grid(m, size=size, frames.args=fill.args, fill="random", prob.row=NULL, prob.col=(1:ncol(m))^5)

# Bottom left to top right diagonal
framesetID <- "worldgrid1_tempRandBL2TR1"
dir.create(outDir <- file.path("../plots/frames", framesetID), recursive=T, showWarnings=F)
fill.args$file <- file.path(outDir, "frame")

fill_grid(m, size=size, frames.args=fill.args, fill="random", prob.row=(nrow(m):1)^10, prob.col=(1:ncol(m))^5)
