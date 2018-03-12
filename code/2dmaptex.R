
# @knitr setup_gentex
# Generate 2D map textures
library(rworldmap) # country data
library(rworldxtra) # hi-res world map
library(marmap) # bathymetry data
library(mapdata) # world rivers
library(sp) # object manipulation and plotting
library(maptools) # object manipulation
library(latticeExtra) # layering individual rivers
library(RColorBrewer) # choropleth maps
library(classInt) # classify numeric data
library(parallel) # generating frames

# no margins for spplot
theme.nomar <- list(axis.line=list(col=NA),
layout.heights=list(top.padding=0, main.key.padding=0, key.axis.padding=0, axis.xlab.padding=0, xlab.key.padding=0, key.sub.padding=0, bottom.padding=0),
layout.widths=list(left.padding=0, key.ylab.padding=0, ylab.axis.padding=0, axis.key.padding=0, right.padding=0))

# bathymetry color palette
clrs <- colorRampPalette(c("black", "cadetblue4"))(100)

# transformations, if desired
f <- function(x) {
	ind <- which(x >= 0)
	#x <- sqrt(x - min(x, na.rm=TRUE) + 1)
	#x[ind] <- NA
	x
}

# @knitr gentex_data
# rivers
m <- map("rivers", plot=F)
IDs <- sapply(strsplit(m$names, ":"), function(x) x[1])
m <- map2SpatialLines(m, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))

# bathymetey matrix
bath <- getNOAA.bathy(lon1=-180, lon2=180, lat1=-90, lat2=90, resolution=10, keep=TRUE)[]
bath <- matrix(1:4, 2, 2) # dummy matrix, helpful for river-only plots
bath.f <- f(bath)

# convert to data frame to be used by ggplot, and spatial grid data frame for spplot
d.bath <- expand.grid(x=seq(-180, 180, length=nrow(bath.f)), y=seq(-90, 90, length=ncol(bath.f)))
d.bath$z <- as.numeric(bath.f)

d.bath.sp <- d.bath
coordinates(d.bath.sp) <- ~x+y
gridded(d.bath.sp) <- TRUE
d.bath.sp <- as(d.bath.sp, "SpatialGridDataFrame")

# @knitr gentex_bathymetry
# Plot bathymetry map with base graphics, great resolution, medium file size
dir.create(bathDir <- "../plots/textures/bathymetry/basic/bg_transparent", recursive=T, showWarnings=FALSE)
png(file.path(bathDir, "/bathymetry4.png"), width=3600, height=1800, bg="transparent")
par(mai=c(0,0,0,0))
image(bath.f, col=clrs, useRaster=T, axes=F)
dev.off()

# Plot bathymetry map with spplot, optionally add rivers as spatial lines, smallest files and worst resolution
add.rivers <- FALSE
plot.args <- list(par.settings=theme.nomar, col.regions=clrs, col=NA, xlim=c(-180,180), ylim=c(-90,90), colorkey=F)
if(add.rivers) plot.args$sp.layout <- list("sp.lines", m, lwd=1, col="white")

dir.create(bathDir <- "../plots/textures/bathymetry/rivers/bg_transparent", recursive=T, showWarnings=FALSE)
png(file.path(bathDir, "/bathymetry2.png"), width=3600, height=1800, bg="transparent")
do.call(spplot, c(list(obj=d.bath.sp["z"]), plot.args))
dev.off()

dir.create(bathDir <- "../plots/textures/bathymetry/basic/bg_transparent", recursive=T, showWarnings=FALSE)
png(file.path(bathDir, "/bathymetry6.png"), width=3600, height=1800, bg="transparent")
do.call(spplot, c(list(obj=d.bath.sp["z"]), plot.args))
dev.off()

# bathymetry ggplot version, great resolution, but large file size, more work required to control color palette
dir.create(bathDir <- "../plots/textures/bathymetry/basic/bg_black", recursive=T, showWarnings=FALSE)
png(file.path(bathDir, "/bathymetry5.png"), width=3600, height=1800)
ggplot() + theme_nothing() + theme(plot.background = element_rect(color="black", fill="black"), panel.background=element_rect(fill="black",color = NA)) +
	scale_x_continuous(expand = c(0,0)) +
	scale_y_continuous(expand = c(0,0)) +
	geom_tile(data=d.bath, aes(x=x, y=y, fill=z))
dev.off()

# @knitr gentex_river_frames
# rivers only, multiple frames
set.seed(8478)
n.river.plots <- 100
rivers.ind <- c(paste0(0, 0:9), 10:99)
river_frames <- function(k){
#for(k in 1:n.river.plots){
	river.col <- colorRampPalette(c("dodgerblue4", "cadetblue1"))(100)
	river.op <- 85:99
	mdf <- SpatialLinesDataFrame(m, data=data.frame(IDs), match.ID=TRUE)
	plot.args <- list(par.settings=theme.nomar, col.regions=NA, col=NA, xlim=c(-180,180), ylim=c(-90,90), colorkey=F)
	dir.create(bathDir <- "../plots/textures/rivers_dark", recursive=T, showWarnings=FALSE)
	png(file.path(bathDir, paste0("rivers", rivers.ind[k], ".png")), width=3600, height=1800, bg="transparent")
	p <- do.call(spplot, c(list(obj=d.bath.sp["z"]), plot.args))
	p <- p + layer(sp.lines(mdf, lwd=1, col="blue"), data=list(mdf=mdf))
	for(i in 1:nrow(mdf)){
		clr <- paste0(sample(river.col, 1, replace=T), sample(river.op, 1, replace=T))
		p <- p + layer(sp.lines(subset(mdf, IDs==i), lwd=1, col=clr), data=list(mdf=mdf, i=i, clr=clr))
		print(nrow(mdf)-i)
	}
	print(p)
	dev.off()
}

n.cores <- 32
mclapply(1:n.river.plots, river_frames, mc.cores=n.cores)

# @knitr gentex_choropleth
# country choropleths
data(countryExData)
SPDF <- joinCountryData2Map(countryExData, joinCode="ISO3", nameJoinColumn="ISO3V10", mapResolution="high")

save_rwm_textures <- function(data, outDir, vars=NULL, file.prefix="rwm--", spplot.args=list(...), ...){
	if(is.null(vars)) vars <- names(data)
	if(!file.exists(outDir)) dir.create(outDir, recursive=TRUE)
	files <- file.path(outDir, paste0(file.prefix, vars, ".png"))
	for(i in 1:length(files)){
		png(files[i], ...)
		par(mai=c(0,0,0,0))
		spplot.list <- list(obj=data, zcol=vars[i])
		spplot.list <- c(spplot.list, spplot.args)
		p <- do.call(spplot, spplot.list)
		print(p)
		dev.off()
	}
}

spdf <- SPDF
pals <- brewer.pal.info
vars <- names(spdf)#[sample(1:ncol(spdf), 10)]
rwm_texture_path <- "../plots/textures/rwm/basic/bg_transparent"

# save textures for all relevant combinations of RColorBrew palettes and rworldmap data sets
par_rwm <- function(i, ...){
#for(i in 1:length(vars)){
	skipped <- FALSE
	v <- vars[i]
	dir.create(path.i <- file.path(rwm_texture_path, v), recursive=T, showWarnings=F)
	num <- is.numeric(spdf[[v]])
	if(num) pals.ind <- 1:nrow(pals) else pals.ind <- which(pals$category != "qual")
	for(j in pals.ind){
		spdf.tmp <- spdf
		pal <- rownames(pals)[j]
		pal.n <- pals$maxcolors[j]
		pal.cat <- pals$category[j]
		plot.args <- list(par.settings=theme.nomar, col.regions=brewer.pal(pal.n, pal), col=NA, xlim=c(-180,180), ylim=c(-90,90), colorkey=F)
		if(num){
			if(length(unique(spdf.tmp[[v]])) < 4) break
			classes <- classIntervals(spdf.tmp[[v]], n=pals$maxcolors[j], style="jenks")
			brks <- classes$brks
			if(length(unique(brks)) < 5) break
			labs <- unlist(dimnames(print(classes)))
			ind.dup <- which(table(brks) > 1)
			if(length(ind.dup)){ brks <- unique(brks); labs <- labs[-ind.dup] }
			spdf.tmp[[v]] <- cut(spdf.tmp[[v]], breaks=brks, labels=labs)
		} else if(length(unique(as.character(spdf.tmp[[v]]))) < 4) break
		save_rwm_textures(spdf.tmp, path.i, vars=v, file.prefix=paste0("rwm_", pal.cat, "_", pal, "--"), spplot.args=plot.args, ...)
	}
	print(length(vars)-i)
}

n.cores <- 32
mclapply(1:length(vars), par_rwm, width=3600, height=1800, bg="transparent", mc.cores=n.cores)

#bpy.colors(7)
