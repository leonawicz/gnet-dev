comargs <- (commandArgs(TRUE))
if(!length(comargs)) q("no") else for(i in 1:length(comargs)) eval(parse(text=comargs[[i]]))

# @knitr setup_cities_seq_3dglobe
# 3D globe cities sequence frames
setwd("/workspace/UA/mfleonawicz/leonawicz/GlobalNetworks/workspaces")
#setwd("C:/leonawicz/GlobalNetworks/workspaces")
load("cities_colors_ordered2.RData")
#load("points.RData")

library(parallel)
#set.seed(47)
n.cores <- 32

source("../code/path_seq_functions.R")

# Choose between breaking at dateline (2D maps) or continuous paths (3D globes)
dateline.break <- TRUE
if(dateline.break) rm.na.dl.brk <- FALSE else rm.na.dl.brk <- TRUE

# @knitr city_path_sequence
n.consec <- 60 # consecutive paths, defines length of time sequence and series of paths
n.concur <- 1*96 # concurrent paths on map for a single time stamp
n.max.segs <- 10001 # maximum sequential segmanents to plot, if/once filled, get_path_seq() will break/exit

#set.seed(47)
sample.ind <- sort(sample(unique(paths$grp), 50000))
paths.sample <- paths[paths$grp %in% sample.ind,]

start <- Sys.time()
p.list <- mclapply(1:n.concur, get_path_seq, paths=paths.sample, seg.size=5, n.consec=n.consec, n.max.segs=n.max.segs, rm.na.dl.brk=rm.na.dl.brk, mc.cores=n.cores)

n.rows <- sapply(p.list, nrow)

groups.list <- lapply(p.list, function(x) unique(x$grp[!is.na(x$lon)]))
n.frames <- 32#min(sapply(groups.list, length))

df.list <- mclapply(1:n.frames, get_frames_df, p.list=p.list, n.frames=n.frames, mc.cores=n.cores)

if(interactive()) ID <- 1
file.out <- if(dateline.break) paste0("cities_sequence2_breakDL_ws", suffix, ".RData") else paste0("cities_sequence2_continuous_10kboot_5L_ws", suffix, ".RData")
save(df.list, n.frames, file=file.out)
end <- Sys.time()
difftime(end, start)


library(rgl)
library(sphereplot)
#df <- data.frame(x=c(0), y=c(0),z=c(0))
rad <- 1
pts <- pt.samp

# transform data to match up with texture wraps
reorient <- function(x){
	x <- x[,c(1,3,2)]
	x[,3] <- 0 - x[,3]
	x <- x[,c(3,2,1)]
	x[,3] <- 0 - x[,3]
	x
}



line.ind <- sort(sample(1:length(gclines.ordered), 1000))
gc.sub <- gclines.ordered[line.ind]
col.sub <- col.op.vec[line.ind]
uni.col.sub <- unique(col.sub)
sub.ind <- which(col.sub==uni.col.sub[100])
do.call(rbind, 

bg3d("black")
sph2 <- rgl.spheres(0,0,0,texture=file.path(bath_textures[1]), textype="rgba", specular="black", emission="black", point_antialias=T, line_antialias=T)
#rgl.material(color="orangered")
#sph <- rgl.spheres(0,0,0,texture=file.path(rwm_textures[2]), textype="rgba", specular="black", emission="black", point_antialias=T, line_antialias=T, depth_test="equal")
#plot3d(df,col=1,type="s", radius=rad, box=F, axes=F)

# @knitr 3dglobe_points
x <- sph2car(long=pts$Longitude, lat=pts$Latitude, radius=rad, deg=T)
x <- reorient(x)
rgl.points(x, color="white", size=1, alpha=0.2)

system.time(
for(i in sort(unique(paths$freq))){
	grps <- unique(paths$grp[paths$freq==i])
	for(j in grps){
		ind <- which(paths$freq==i & paths$grp==j)
		#clrs <- colors[line12.colors[i]]
		#lines3d(sph2car(long=paths$lon[ind], lat=paths$lat[ind], radius=rad, deg=T), col="#0000FF", add=T, lwd=0.01, alpha=1)
		points3d(sph2car(long=paths$lon[ind], lat=paths$lat[ind], radius=rad, deg=T), col="#0000FF", add=T, pch=".", size=1, alpha=1)
		print(length(grps)-which(grps==j))
	}
	print(paste("######## FLIGHT FREQUENCY:", i, "########"))
}
)

system.time(
for(i in sort(unique(paths$freq))){
	grps <- unique(paths$grp[paths$freq==i])
	for(j in grps){
		ind <- which(paths$freq==i & paths$grp==j)
		#clrs <- colors[line12.colors[i]]
		lines3d(sph2car(long=paths$lon[ind], lat=paths$lat[ind], radius=rad, deg=T), col="#FFFFFF", add=T, lwd=0.01, alpha=op[ind][1])
		print(length(grps)-which(grps==j))
	}
	print(paste("######## FLIGHT FREQUENCY:", i, "########"))
}
)



