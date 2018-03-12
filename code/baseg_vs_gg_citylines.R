#setwd("C:/leonawicz/GlobalNetworks/workspaces")
setwd("/workspace/UA/mfleonawicz/leonawicz/GlobalNetworks/workspaces")
load("break_DL_all.RData")

pts <- pt.samp
w <- 12; h <- w*(diff(c(-90, 90))/diff(c(-180,180)))
lat.lim <- c(-90,90)
lon.lim <- c(-180,180)
w <- 6000; h <- round(w*(diff(lat.lim)/diff(lon.lim)))

library(ggplot2)
library(gridExtra)

# redefine here, doesn't work from ggmap package
theme_nothing <- function(base_size=12){
	theme(
		line=element_blank(), text=element_blank(), axis.ticks.length=unit(0, "cm"), axis.ticks.margin=unit(0.01, "cm"), # change back to 0 when grid is fixed
			legend.position="none", panel.margin=unit(0, "lines"), plot.margin=unit(c(0, 0, -.5, -.5), "lines"), complete=TRUE)
}
#library(ggmap)

# ggplot, single color, not very exciting
start <- Sys.time()
png("../Graphics/2015/grn_a01_27l_0p.png", width=w, height=h, res=100, pointsize=12)
ggplot() + theme_nothing() + theme(plot.background = element_rect(color="black", fill="black"), panel.background=element_rect(fill="black",color = NA)) +
	scale_x_continuous(expand = c(0,0)) +
	scale_y_continuous(expand = c(0,0)) +
	geom_point(data=data.frame(x=c(-180, 180, 180, -180), y=c(-90, -90, 90, 90)), aes(x=x, y=y), colour="black") +
	geom_path(data=paths[paths$grp<=270000,], aes(lon, lat , group = grp), color="green", alpha=0.01)
dev.off()
end <- Sys.time()
difftime(end, start)

# base graphics, old school
load("points.RData")
load("paths.RData")
load("linecolors.RData")
choose.OS <- function(x) return(if(x=="Windows") drive <- c("X:","Y:","Z:") else if(x=="Linux") drive <- c("/workspace/UA/mfleonawicz","/workspace/Shared/Users","/Data"))
drive <- choose.OS(Sys.info()["sysname"])
path.fns <- paste(drive[1],"/Leonawicz/Play_data/CustomFunctions/",sep="")
plotlines.lonshift <- source(paste(path.fns,"plotlines.lonshift.function.txt",sep=""))$value

png("../plots/cities/test_BlOrWt.png"), width=w, height=h, res=100, pointsize=12)
plotlines.lonshift(lns=sample12.lines, plt.ord=plot12.order[seq(1,250000,by=2)],
	clr.seq=colors, ln.clrs=line12.colors, ln.op=opacity, lon.center=0,
	lines.only=F,lines.first=T,pts=pt.samp[seq(1,100000,by=1),],xlim=lon.lim,ylim=lat.lim,bg=colors[1])
dev.off()
