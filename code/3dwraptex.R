
# @knitr setup_wraptex
# Wrap 2D map textures on spheres
#load("points.RData")
library(rgl)
library(sphereplot)
#df <- data.frame(x=c(0), y=c(0),z=c(0))
rad <- 1

# transform data to match up with texture wraps
reorient <- function(x){
	x <- x[,c(1,3,2)]
	x[,3] <- 0 - x[,3]
	x <- x[,c(3,2,1)]
	x[,3] <- 0 - x[,3]
	x
}

rwm_texture_path <- "../plots/textures/rwm/basic/bg_transparent"
bath_texture_path <- "../plots/textures/bathymetry/basic/bg_transparent"
river_texture_path <- "../plots/textures/rivers_dark"
rwm_textures <- list.files(rwm_texture_path, full=T, recursive=T)
bath_textures <- list.files(bath_texture_path, full=T)
river_textures <- list.files(river_texture_path, full=T)

# @knitr wraptex
bg3d("black")
#sph0 <- rgl.spheres(0,0,0,texture="../plots/cities/BlkGrnYelWht1_transparent.png", specular="black", emission="black", point_antialias=T, line_antialias=T)

#sph0 <- rgl.spheres(0,0,0,texture="../plots/world.topo.200412.3x5400x2700.png", specular="black", emission="black", point_antialias=T, line_antialias=T)

#sph2 <- rgl.spheres(0,0,0,texture=file.path(bath_textures[3]), textype="rgba", specular="black", emission="black", point_antialias=T, line_antialias=T)
#sph <- rgl.spheres(0,0,0,texture=file.path(rwm_textures[2]), textype="rgba", specular="black", emission="black", point_antialias=T, line_antialias=T, depth_test="equal")

sph <- rgl.spheres(0,0,0,texture=rwm_textures[2], textype="rgba", specular="black", emission="black", point_antialias=T, line_antialias=T, depth_test="equal")
sph2 <- rgl.spheres(0,0,0,texture=bath_textures[4], textype="rgba", specular="black", emission="black", point_antialias=T, line_antialias=T, depth_test="equal")

#x <- sph2car(long=pts$Longitude, lat=pts$Latitude, radius=rad, deg=T)
#x <- reorient(x)
#rgl.points(x, color="white", size=1, alpha=0.3)

# @knitr 3dglobe_frames1
# Make frames more explicitly
fac <- 10
n.frames <- 360*fac
ind <- paste0(rep(0:9, each=1000), rep(paste0(rep(0:9, each=100), rep(c(paste0(0,0:9), 10:99), 10)), 10))[1:n.frames]
set.seed(47)
for(i in 1:n.frames){
	rgl.viewpoint(-(i-1)/10, 0, zoom=0.55)
	riv <- sample(river_textures, 1)
	sph3 <- rgl.spheres(0,0,0, texture=riv, textype="rgba", specular="black", emission="black", point_antialias=T, line_antialias=T, depth_test="equal")
	rgl.snapshot(paste0("C:/leonawicz/GlobalNetworks/plots/frames/globe5_rivers/globe5_frame", ind[i],".png"), fmt="png")
	rgl.pop(id=sph3)
	print(n.frames-i)
}

# @knitr 3dglobe_frames2
# Make frames using movie3d()
outDir <- "C:/leonawicz/GlobalNetworks/plots/frames/globe2"
rgl.viewpoint(0, 0, zoom=0.55)
play3d(spin3d(axis=c(0,1,0), rpm=10), duration=5)
movie3d(spin3d(axis=c(0,1,0), rpm=0.5), dir=outDir, duration=120, fps=30, convert=FALSE, frames="globe2_frame")

# @knitr 3dglobe_frames3
# Show spinning sprites, and rotate the whole view
open3d()
rgl.viewpoint(0, 0, zoom=4)
bg3d("lightblue")
spriteid <- NULL
 
spin1 <- spin3d(axis=c(0,1,0), rpm=1 ) # the scene spinner
spin2 <- spin3d(axis=c(0,1,0), rpm=5 ) # the sprite spinner
 
f <- function(time) {
    par3d(skipRedraw = TRUE) # stops intermediate redraws
    on.exit(par3d(skipRedraw=FALSE)) # redraw at the end
 
    rgl.pop(id=spriteid) # delete the old sprite
    #moonid <- plot3d(-5,0,-5, col="gray", type="s", radius=rad/4, , )
	moonid <- rgl.spheres(-3,0,-3, radius=0.5, texture="../plots/flights_globe03.png", box=F, axes=F, specular="black", emission="black",
	point_antialias=T, line_antialias=T)
    spriteid <<- sprites3d(0,0,0, shape=moonid,
                   userMatrix=spin2(time, 
                     base=spin1(time)$userMatrix)$userMatrix)
	sph <- rgl.spheres(0,0,0,texture="../plots/texture.png", specular="black", emission="black",
		point_antialias=T, line_antialias=T, , box=F, axes=F, add=T)
    spin1(time)
}
if (!rgl.useNULL())
  play3d(f, duration=6)
  