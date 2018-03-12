comargs <- (commandArgs(TRUE))
if(!length(comargs)) q("no") else for(i in 1:length(comargs)) eval(parse(text=comargs[[i]]))

# @knitr setup_cities_seq_2dmap
# 2D map cities sequence frames
setwd("/workspace/UA/mfleonawicz/leonawicz/GlobalNetworks/workspaces")
#setwd("C:/leonawicz/GlobalNetworks/workspaces")
load("cities_sequence2_breakDL_final.RData")
#load("cities_sequence3_breakDL_final.RData")
load("points.RData")

library(parallel)
#library(png)
library(ggplot2)
library(gridExtra)

source("../code/plot_seq_functions.R")

n.cores <- 32
set.seed(47)
pts <- pt.samp

w <- 12; h <- w*(diff(c(-90, 90))/diff(c(-180,180)))
lat.lim <- c(-90,90)
lon.lim <- c(-180,180)
w <- 6000; h <- round(w*(diff(lat.lim)/diff(lon.lim)))

# redefine here, doesn't work from ggmap package
theme_nothing <- function(base_size=12){
	theme(
		line=element_blank(), text=element_blank(), axis.ticks.length=unit(0, "cm"), axis.ticks.margin=unit(0.01, "cm"), # change back to 0 when grid is fixed
			legend.position="none", panel.margin=unit(0, "lines"), plot.margin=unit(c(0, 0, -.5, -.5), "lines"), complete=TRUE)
}

#library(ggmap)
#img <- readPNG("../plots/textures/bathymetry/basic/bg_transparent/bathymetry3.png")

# @knitr 2dmap_frames
# Generate frames
framesetID <- "tpbg1_citiesseq2_GrnYelWht"
nn <- n.frames #min(n.frames, 90)
ind <- formatC(1:nn, width=4, flag="0")
dir.create(outDir <- file.path("../plots/frames", framesetID), recursive=T, showWarnings=F)

#unlink(list.files(outDir, full=T))
if(interactive()) frames <- 1:length(ind) else if(!exists("frames")) stop("Must provide sequence, e.g.: frames=1:1000, in non-interactive mode.")
start <- Sys.time()
mclapply(frames, save2Dplot, outDir=outDir, w=w, h=h, pts=pts, bg="transparent", increase.alpha=0, min.alpha=20, check.dl.brk=TRUE, txt=ppf.seq.txt, mc.cores=n.cores)
end <- Sys.time()
difftime(end, start)
