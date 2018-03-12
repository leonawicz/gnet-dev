#########################################
#### Connections to SNAP (Fairbanks) ####
#########################################

##########################################
#### Network a large sample of cities ####
##########################################

choose.OS <- function(x) return(if(x=="Windows") drive <- c("X:","Y:","Z:") else if(x=="Linux") drive <- c("/workspace/UA/mfleonawicz","/workspace/Shared/Users","/Data"))
drive <- choose.OS(Sys.info()["sysname"])

#### Load required packages ####
library(geosphere)
library(parallel)
n.cores <- 32
set.seed(47) # v3 # set.seed(470) # v1 and v2
#### Load world cities data ####
setwd("/workspace/UA/mfleonawicz/leonawicz/GlobalNetworks/workspaces")
data.all <- read.table("../data/WorldCitiesUniqueLocs.txt",header=T)
dim(data.all)

#####################################
#### Sample cities, create pairs ####
#####################################

data <- subset(data.all,Population>=10000)
dim(data)

x1 <- sample(1:length(data[,1]),1000,rep=F,prob=sqrt(data$Population)) ## Weighted by sqrt population
x2 <- x1[((length(x1)/2)+1):length(x1)]
x1 <- x1[1:(length(x1)/2)]
sample.cities1 <- data[x1,7:6]  ## Store lat/lon coordinates for the sample
sample.cities2 <- data[x2,7:6]  ## Store lat/lon coordinates for the sample


##################################################
#### Calculate distances for each sample pair ####
##################################################

## Obtain distances from each city in sample 1 to each in sample 2
dm <- function(i) distMeeus(sample.cities1[i,], sample.cities2)
sample.distances12 <- unlist(mclapply(1:length(x1), dm, mc.cores=n.cores))
sample.distances12[is.na(sample.distances12)] <- 10


##################################
#### Compute weighting scheme ####
##################################

#### Calculate distance weights ####
dist12.wts <- 99*(1-sample.distances12/max(sample.distances12))  ## Inverse weight by distance
#dist12.wts <- 1/(sample.distances12^(.25))

#### Calculate population weights ####
pop.wts <- with(data,99*(sqrt(Population)/max(sqrt(Population))))  ## Weight by average log population
pw <- function(i) (pop.wts[x1[i]] + pop.wts[x2])/2
pop12.wts <- unlist(mclapply(1:length(x1),pw,mc.cores=n.cores))


#### Calculate country weights ####
country.pops <- data.frame(cbind(pop=with(data,tapply(Population,Country,sum)),
                           cities=with(data,tapply(Population,Country,length))))
country.pops[is.na(country.pops)] <- 10
country.wts <- with(country.pops,rep(pop/sum(pop),times=cities))
cw <- function(i) (country.wts[x1[i]] + country.wts[x2])/2
country12.wts <- unlist(mclapply(1:length(x1),cw,mc.cores=n.cores))
country.wts <- 99*(country.wts/max(country.wts))


########################################################################################
#### Add bias, assign weights, compile a bootstrapped weighted sample of city pairs ####
########################################################################################

bias.wts <- c(5.6,2.3,1.1)
line12.wts <- (dist12.wts*exp(bias.wts[1]) + pop12.wts*exp(bias.wts[2]) + country12.wts*exp(bias.wts[3]))/(3*exp(mean(bias.wts)))  ## Combined weight for each great cirlce arc
line12.wts <- 99*(line12.wts)/(max(line12.wts))

city.pairs.x1y1 <- cbind(Longitude=rep(sample.cities1[,1],each=length(sample.cities2[,1])),
                         Latitude=rep(sample.cities1[,2],each=length(sample.cities2[,1])))
city.pairs.x2y2 <- cbind(Longitude=rep(sample.cities2[,1],length(sample.cities1[,1])),
                         Latitude=rep(sample.cities2[,2],length(sample.cities1[,1])))
wtd.sample <- sample(1:length(city.pairs.x1y1[,1]),250000,rep=T,prob=as.numeric(line12.wts)) ## Weighted by inverse dist and combined avg log pop
city.pairs <- list(city.pairs.x1y1[wtd.sample,],city.pairs.x2y2[wtd.sample,])
city.dists <- sample.distances12[wtd.sample]

#########################################################################
#### Create list of sets of intermediate points via gcIntermediate() ####
#########################################################################

#sample12.lines <- gcIntermediate(city.pairs[[1]], city.pairs[[2]], n=(round(38*(city.dists/max(city.dists)),0)+10), addStartEnd=T, breakAtDateLine=T, sepNA=F)  ## Obtain great circle arcs (about 20 secs for 10K pairs)

n <- nrow(city.pairs[[1]])
sq <- round(seq(1, n+1, length.out=n.cores + 1))
#dst - min(dst)/diff(range(dst))

get_gc_lines <- function(i, sq, x1y1, x2y2, dst, dl.brk=TRUE, dl.na.sep=TRUE){
	ind <- sq[i]:(sq[i+1]-1)
	x1y1 <- x1y1[ind,]
	x2y2 <- x2y2[ind,]
	d <- dst[ind]
	x <- gcIntermediate(x1y1, x2y2, n=(round(200*(d/max(d)),0)+10), addStartEnd=T, breakAtDateLine=dl.brk, sepNA=F)
	if(dl.na.sep){
		dateline.ind <- which(sapply(x, class)=="list")
		if(length(dateline.ind)) x[dateline.ind] <- lapply(x[dateline.ind], function(x) rbind(rbind(x[[1]], NA), x[[2]]))
	}
	x
}

gclines <- mclapply(1:n.cores, get_gc_lines, sq=sq, x1y1=city.pairs[[1]], x2y2=city.pairs[[2]], dst=city.dists, dl.brk=TRUE, mc.cores=n.cores)
gclines <- do.call(c, gclines)

##################################
#### Preparation for plotting ####
##################################

path.fns <- paste("../../Play_data/CustomFunctions/") ## function directory
col.pal <- source(paste(path.fns,"colorpalettes.function.txt",sep=""))$value

#### Create a plot order by line weight ####
plot12.order <- order(line12.wts[wtd.sample])  ## More heavily weighted (closer to white and more opaque) lines are plotted last

#### Create a color palette ####
colors <- col.pal("boyw") ## Vector of colors defining the color palette

#### Assign a color from the palette to each line by line weight ####
line12.colors <- round((256/99)*(line12.wts[wtd.sample]),0)
line12.colors <- round(256*(line12.colors^.75)/max(line12.colors^.75),0)
line12.colors[line12.colors < 10] <- 10

#### Assign a line opacity value to each line by line weight ####
opacity <- as.character(round(99*line12.colors/max(line12.colors),0))
a1 <- 0.05 ## Rescale/adjustment
opacity <- 99*exp(a1*as.numeric(opacity))/max(exp(a1*as.numeric(opacity))) 
opacity[opacity < 1] <- 1
#opacity <- (opacity - ( (sum(range(opacity))/2) - 45 ))
opacity <- opacity^0.75
#opacity[opacity<10] <- 10
opacity <- as.character(round(opacity,0))
singles <- as.character(0:9)  ## Establish 2-digit character strings for line opacity by weight
for(i in singles){
opacity[opacity==i] <- paste("0",opacity[opacity==i],sep="")
}
#opacity <- rep("10",length(opacity))


##########################
#### Plot the network ####
##########################

#### Obtain a sample of points if plotting points ####
#data.pts <- subset(data.all,Population>=1000)
#pt.samp <- data.pts[,1] #sample(1:length(data.pts[,1]),100000,rep=F)
pt.samp1 <- which(data.all$Population>=1000)
pt.samp2 <- which(data.all$Population<1000)
pt.samp <- c(pt.samp1,sample(pt.samp2,100000-length(pt.samp1),rep=F))
pt.samp <- data.all[,7:6][pt.samp,]
pt.samp <- pt.samp[pt.samp$Latitude<70,]
#pt.samp <- rbind(sample.cities1,sample.cities2)

#### Old plot
setwd("/workspace/UA/mfleonawicz/leonawicz/GlobalNetworks/workspaces")
#load("CitiesNet1.Rdata")
#colors <- colorRampPalette(c("black", "darkgreen", "green", "yellow","white"))(256) # v2
colors <- colorRampPalette(c("darkgreen", "green", "yellow","white"))(256) # v3

#### Import plotting functions ####
#path.fns <- "C:/Leonawicz/Leonawicz_tmp/CustomFunctions/"
#path.plots <- "C:/Leonawicz/Leonawicz_tmp/WorldCitiesMapping/Graphics/"
path.fns <- paste("../../Play_data/CustomFunctions/") ## function directory
pts.lonshift <- source(paste(path.fns,"pts.lonshift.function.txt",sep=""))$value
plotlines.lonshift <- source(paste(path.fns,"plotlines.lonshift.function.txt",sep=""))$value
plotlines.lonshift2 <- source(paste(path.fns,"plotlines.lonshift.function2.txt",sep=""))$value
plotlines.lonshift3 <- source(paste(path.fns,"plotlines.lonshift.function3.txt",sep=""))$value

#gcmapdata <- list(colors=colors,line12.colors=line12.colors,opacity=opacity,plot12.order=plot12.order,pt.samp=pt.samp,sample12.lines=sample12.lines)
w <- 12; h <- w*(diff(c(-65,88))/diff(c(-180,180)))
#jpeg(paste(path.plots,"/test.jpg",sep=""), width=w, height=h, units = "in", pointsize = 12, quality = 100, bg = "white", res=300)
#pdf(paste(path.plots,"/test.pdf",sep=""), width=w, height=h, pointsize = 12, bg = "white")
lat.lim <- c(-56,84)
lon.lim <- c(-168,168)
#lat.lim <- c(-11,68)
#lon.lim <- c(-88,52)

w <- 12; h <- w*(diff(c(-90,90))/diff(c(-180,180)))
lat.lim <- c(-90,90)
lon.lim <- c(-180,180)
w <- 6000; h <- round(w*(diff(lat.lim)/diff(lon.lim))) # gives h of 1440 # use 6000w and 2500h for large figures
png("../plots/cities/BlkGrnYelWht1_transparent2b.png", width=w, height=h, res=100, pointsize=12, bg="transparent")

plotlines.lonshift(lns=gclines, plt.ord=plot12.order[seq(1,250000,by=2)],
               clr.seq=colors, ln.clrs=line12.colors, ln.op=opacity, lon.center=0,
               lines.only=F,lines.first=T,pts=pt.samp[seq(1,100000,by=1),],xlim=lon.lim,ylim=lat.lim,bg="transparent")#colors[1])
#library(gcmap);data(gcmapdata);attach(gcmapdata)
#plotlines.lonshift3(lns=sample12.lines, plt.ord=plot12.order[seq(1,250000,by=1)],
#               clr.seq=colors, ln.clrs=line12.colors, ln.op=opacity, lon.center=0,
#               lines.only=F,lines.first=T,pts=pt.samp[seq(1,100000,by=1),],ylim=c(-65,88),bg=colors[1],win.plot=F,
#               save.stills=T,stills.rate=1000,stills.path=paste(drive[1],"/Leonawicz/Play_data/WorldCitiesMapping/Graphics/stills/testing/",sep=""),wid.in=w,ht.in=h)
#
dev.off()

col.op.vec <- paste0(colors[line12.colors], opacity)[plot12.order]
gclines.ordered <- gclines[plot12.order]
uni.colors <- unique(col.op.vec)

source("../code/path_seq_functions.R")
paths <- mclapply(1:n.cores, lines_df, sq=sq, gclines=gclines.ordered, col.vec=col.op.vec, mc.cores=n.cores)
paths <- do.call(rbind, paths)

# save(paths, uni.colors, file="cities_colors_ordered2.RData") # v2
save(paths, uni.colors, file="cities_colors_ordered_DrkGrnYelWht.RData") # v3






library(ggmap)
library(rworldmap)

s <- getMap()

gc_lines_df <- function(i, sq, gclines){
	ind <- sq[i]:(sq[i+1]-1)
	x <- gclines[ind]
	x <- lapply(1:length(ind), function(i, x, ind) { x <- data.frame(x[[i]]); x$grp=ind[i]; x }, x=x, ind=ind)
	x <- do.call(rbind, x)
}

n <- length(gclines)
sq <- round(seq(1, n+1, length.out=n.cores + 1))
paths <- mclapply(1:n.cores, gc_lines_df, sq=sq, gclines=gclines, mc.cores=n.cores)
paths <- do.call(rbind, paths)


plot12.order <- c(1:length(cl.ind), plot12.order + length(cl.ind))
opacity <- c(opacity[cl.ind], opacity)
line12.colors <- c(line12.colors[cl.ind], line12.colors)
save(pt.samp, paths, opacity, colors, line12.colors, plot12.order, file="break_DL_all.RData")


theme_black=function(base_size=12,base_family="") {
  theme_grey(base_size=base_size,base_family=base_family) %+replace%
    theme(
      # Specify axis options
      axis.line=element_blank(), 
      axis.text.x=element_text(size=base_size*0.8,color="white",
                               lineheight=0.9,vjust=1), 
      axis.text.y=element_text(size=base_size*0.8,color="white",
                               lineheight=0.9,hjust=1), 
      axis.ticks=element_line(color="white",size = 0.2), 
      axis.title.x=element_text(size=base_size,color="white",vjust=1), 
      axis.title.y=element_text(size=base_size,color="white",angle=90,
                                vjust=0.5), 
      axis.ticks.length=unit(0.3,"lines"), 
      axis.ticks.margin=unit(0.5,"lines"),
      # Specify legend options
      legend.background=element_rect(color=NA,fill="black"), 
      legend.key=element_rect(color="white", fill="black"), 
      legend.key.size=unit(1.2,"lines"), 
      legend.key.height=NULL, 
      legend.key.width=NULL,     
      legend.text=element_text(size=base_size*0.8,color="white"), 
      legend.title=element_text(size=base_size*0.8,face="bold",hjust=0,
                                color="white"), 
      #legend.position="right", 
      legend.text.align=NULL, 
      legend.title.align=NULL, 
      #legend.direction="vertical", 
      legend.box=NULL,
      # Specify panel options
      panel.background=element_rect(fill="black",color = NA), 
      panel.border=element_rect(fill=NA,color="white"), 
      #panel.grid.major=element_blank(), 
      #panel.grid.minor=element_blank(), 
	  panel.grid.major = element_line(colour = "grey10", size = 0.2), # test alternate
	  panel.grid.minor = element_line(colour = "grey2", size = 0.5), # test alternate
      panel.margin=unit(0.25,"lines"),  
      # Specify facetting options
      strip.background=element_rect(fill="grey30",color="grey10"), 
      strip.text.x=element_text(size=base_size*0.8,color="white"), 
      strip.text.y=element_text(size=base_size*0.8,color="white",
                                angle=-90), 
      # Specify plot options
      plot.background=element_rect(color="black",fill="black"), 
      plot.title=element_text(size=base_size*1.2,color="white"), 
      plot.margin=unit(c(1,1,0.5,0.5),"lines")
    )
}

m <- ggplot() + theme_black() +
	#geom_polygon(data = s, aes(x=long, y=lat, group=group)) + 
	#geom_path(data = s, aes(x=long, y=lat, group=group), colour="black") + 
	geom_path(data = paths, aes(lon, lat , group = grp), color="white") +
	# coord_map("bicentric", lon = 0)
	# coord_map("bonne", lat= 0)
	coord_map("ortho", orientation=c(31, -74, 0)) # for ortho maps

png(paste(path.plots,"/test_globe.png",sep=""), width=w, height=h, res=100, pointsize=12)
print(m)
dev.off()














WD <- getwd()
setwd(paste(drive[1],"/Leonawicz/Play_data/WorldCitiesMapping/Graphics/stills/testing/",sep=""))
shell("convert -limit memory 1000mb -limit map 900mb -limit area 1500mb -delay 1x30 -loop 1 *.png MapTestBeta.gif")
#shell("C:/progra~1/swftools/png2swf.exe -o MapTest2.swf -r 30 *.png")

shell("convert MapTest.gif -coalesce coalesce.gif
    convert  coalesce.gif     -deconstruct   deconstruct.gif
    gif_anim_montage  coalesce.gif     coalesce_frames.gif
    gif_anim_montage  deconstruct.gif  deconstruct_frames.gif



savePlot(paste(path.plots,"Klns.Kpts",sep=""),type="jpg",device = dev.cur())


#########################################################################################################
