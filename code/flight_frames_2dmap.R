
# @knitr setup_flight_seq_2dmap
# 2D map flight sequence frames
setwd("/workspace/UA/mfleonawicz/leonawicz/GlobalNetworks/workspaces")
#setwd("C:/leonawicz/GlobalNetworks/workspaces")
load("flight_sequence_breakDL_10kboot.RData")
load("points.RData")

library(parallel)
library(png)
library(ggplot2)
library(gridExtra)

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
img <- readPNG("../plots/textures/bathymetry/basic/bg_transparent/bathymetry3.png")

# @knitr 2dmap_frames
# Generate frames
framesetID <- "worldmap1_flightseq1"
nn <- min(n.frames, 90)
ind <- paste0(rep(0:9, each=1000), rep(paste0(rep(0:9, each=100), rep(c(paste0(0,0:9), 10:99), 10)), 10))[1:nn]
dir.create(outDir <- file.path("../plots/frames", framesetID), recursive=T, showWarnings=F)
set.seed(47)

save2Dplot <- function(i, outDir, w, h, line.color="orangered", pt.frac=0.75, line.sublayer=TRUE){
	d <- df.list[[i]]
	if(!is.null(pts)) { nrp <- nrow(pts); rows <- sample(1:nrp, round(pt.frac*nrp)) }
	file <- paste0(outDir, "/", basename(outDir), "_frame", ind[i], ".png")
	png(file, width=w, height=h)
	g <- ggplot(data=data.frame(x=c(-180, 180, 180, -180), y=c(-90, -90, 90, 90)), aes(x=x, y=y)) + theme_nothing() + theme(plot.background = element_rect(color="transparent", fill="transparent"), panel.background=element_rect(fill="transparent",color = NA)) +
		annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), -Inf, Inf, -Inf, Inf) +
		scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0))
	if(!is.null(pts)) g <- g + geom_point(data=pts[rows,], aes(x=Longitude, y=Latitude), size=1, colour="white", alpha=0.8)
	if(line.sublayer) g <- g + geom_path(data=d, aes(x=lon, y=lat, group=grp), color="white", size=1, alpha=runif(1, 0.35, 0.65))
	g <- g + geom_path(data=d, aes(lon, lat, group=grp), color=line.color, size=0.5, alpha=runif(1, 0.35, 0.65))
	print(g)
	dev.off()
}

#unlink(list.files(outDir, full=T))
start <- Sys.time()
mclapply(1:length(ind), save2Dplot, outDir=outDir, w=w, h=h, pts=pts, mc.cores=n.cores)
end <- Sys.time()
difftime(end, start)
