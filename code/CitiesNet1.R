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
set.seed(470)
#### Load world cities data ####
path.data <- paste(drive[1],"/Leonawicz/Play_data/WorldCitiesMapping/Data/",sep="")
data.all <- read.table(paste(path.data,"WorldCitiesUniqueLocs.txt",sep=""),header=T)
dim(data.all)

#####################################
#### Sample cities, create pairs ####
#####################################

data <- subset(data.all,Population>=5000)
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
sample.distances12 <- unlist(mclapply(1:length(x1),dm,mc.cores=16))
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
pop12.wts <- unlist(mclapply(1:length(x1),pw,mc.cores=16))


#### Calculate country weights ####
country.pops <- data.frame(cbind(pop=with(data,tapply(Population,Country,sum)),
                           cities=with(data,tapply(Population,Country,length))))
country.pops[is.na(country.pops)] <- 10
country.wts <- with(country.pops,rep(pop/sum(pop),times=cities))
cw <- function(i) (country.wts[x1[i]] + country.wts[x2])/2
country12.wts <- unlist(mclapply(1:length(x1),cw,mc.cores=16))
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

sample12.lines <- gcIntermediate(city.pairs[[1]], city.pairs[[2]], n=(round(38*(city.dists/max(city.dists)),0)+10), addStartEnd=T, breakAtDateLine=T, sepNA=F)  ## Obtain great circle arcs (about 20 secs for 10K pairs)


##################################
#### Preparation for plotting ####
##################################

path.fns <- paste(drive[1],"/Leonawicz/Play_data/CustomFunctions/",sep="") ## function directory
col.pal <- source(paste(path.fns,"colorpalettes.function.txt",sep=""))$value

#### Create a plot order by line weight ####
plot12.order <- order(line12.wts[wtd.sample])  ## More heavily weighted (closer to white and more opaque) lines are plotted last

#### Create a color palette ####
colors <- col.pal("boyw") ## Vector of colors defining the color palette
colors <- colorRampPalette(c("black","orangered","white"))(256)

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
pt.samp1 <- which(data.all$Population>=100)
pt.samp2 <- which(data.all$Population<100)
pt.samp <- c(pt.samp1,sample(pt.samp2,100000-length(pt.samp1),rep=F))
pt.samp <- data.all[,7:6][pt.samp,]
pt.samp <- pt.samp[pt.samp$Latitude<70,]
#pt.samp <- rbind(sample.cities1,sample.cities2)

#### Import plotting functions ####
#path.fns <- "C:/Leonawicz/Leonawicz_tmp/CustomFunctions/"
#path.plots <- "C:/Leonawicz/Leonawicz_tmp/WorldCitiesMapping/Graphics/"
path.fns <- paste(drive[1],"/Leonawicz/Play_data/CustomFunctions/",sep="") ## function directory
dir.create(path.plots <- paste(drive[1],"/Leonawicz/Play_data/WorldCitiesMapping/Graphics/120213",sep=""), showWarnings=F) ## Save plots here
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
w <- 6000; h <- round(w*(diff(lat.lim)/diff(lon.lim))) # gives h of 1440 # use 6000w and 2500h for large figures
png(paste(path.plots,"/test_BlOrWt_large.png",sep=""), width=w, height=h, res=100, pointsize=12)

plotlines.lonshift(lns=sample12.lines, plt.ord=plot12.order[seq(1,250000,by=2)],
               clr.seq=colors, ln.clrs=line12.colors, ln.op=opacity, lon.center=0,
               lines.only=F,lines.first=T,pts=pt.samp[seq(1,100000,by=1),],xlim=lon.lim,ylim=lat.lim,bg=colors[1])

#library(gcmap);data(gcmapdata);attach(gcmapdata)
#plotlines.lonshift3(lns=sample12.lines, plt.ord=plot12.order[seq(1,250000,by=1)],
#               clr.seq=colors, ln.clrs=line12.colors, ln.op=opacity, lon.center=0,
#               lines.only=F,lines.first=T,pts=pt.samp[seq(1,100000,by=1),],ylim=c(-65,88),bg=colors[1],win.plot=F,
#               save.stills=T,stills.rate=1000,stills.path=paste(drive[1],"/Leonawicz/Play_data/WorldCitiesMapping/Graphics/stills/testing/",sep=""),wid.in=w,ht.in=h)
#
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
