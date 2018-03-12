
# @knitr setup_flight_seq_3dglobe
# 3D globe flight sequence frames
#setwd("/workspace/UA/mfleonawicz/leonawicz/GlobalNetworks/workspaces")
setwd("C:/leonawicz/GlobalNetworks/workspaces")
load("flight_sequence_continuous_10kboot.RData")
load("points.RData")

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

# @knitr texture_sphere
#rwm_texture_path <- "../plots/textures/rwm/basic/bg_transparent"
bath_texture_path <- "../plots/textures/bathymetry/basic/bg_transparent"
#river_texture_path <- "../plots/textures/rivers_dark"
#rwm_textures <- list.files(rwm_texture_path, full=T, recursive=T)
bath_textures <- list.files(bath_texture_path, full=T)
#river_textures <- list.files(river_texture_path, full=T)
bg3d("black")
#sph0 <- rgl.spheres(0,0,0,texture="../plots/world.topo.200412.3x5400x2700.png", specular="black", emission="black", point_antialias=T, line_antialias=T)
sph2 <- rgl.spheres(0,0,0,texture=file.path(bath_textures[1]), textype="rgba", specular="black", emission="black", point_antialias=T, line_antialias=T)
#rgl.material(color="orangered")
#sph <- rgl.spheres(0,0,0,texture=file.path(rwm_textures[2]), textype="rgba", specular="black", emission="black", point_antialias=T, line_antialias=T, depth_test="equal")
#plot3d(df,col=1,type="s", radius=rad, box=F, axes=F)

# @knitr 3dglobe_points
x <- sph2car(long=pts$Longitude, lat=pts$Latitude, radius=rad, deg=T)
x <- reorient(x)
rgl.points(x, color="white", size=1, alpha=0.2)

# @knitr 3dglobe_frames
# Generate frames
framesetID <- "globe6_flightseq1"
fac <- 1
nn <- 360*fac
ind <- paste0(rep(0:9, each=1000), rep(paste0(rep(0:9, each=100), rep(c(paste0(0,0:9), 10:99), 10)), 10))[1:nn]
dir.create(outDir <- file.path("../plots/frames", framesetID), recursive=T, showWarnings=F)
set.seed(47)

for(i in 1:nn){
	rgl.viewpoint(-(i-1)/fac, 0, zoom=0.55) # rotation rate and zoom level
	di.seq <- rep(c(1:n.frames)[1:(nn/4)], length=nn)
	d <- df.list[[di.seq[i]]]
	l <- sph2car(long=d[,1], lat=d[,2], radius=rad, deg=T)
	l <- reorient(l)
	l1 <- lines3d(l, color="white", add=T, lwd=1, alpha=runif(1, 0.35, 0.65))
	l2 <- lines3d(l, color="orangered", add=T, lwd=1, alpha=runif(1, 0.35, 0.65), depth_test="equal")
	rgl.snapshot(paste0(outDir, "/", basename(outDir), "_frame", ind[i],".png"), fmt="png")
	rgl.pop(id=l1)
	rgl.pop(id=l2)
	print(nn-i)
}
