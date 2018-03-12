##########################################
#### Network a large sample of cities ####
##########################################

# Load packages and data
library(geosphere)
library(parallel)
library(data.table)
library(dtplyr)
library(dplyr)
library(purrr)
n.cores <- 32
set.seed(47) # v3 # set.seed(470) # v1 and v2
#### Load world cities data ####
setwd("/workspace/UA/mfleonawicz/leonawicz/GlobalNetworks/workspaces")
d_all <- fread("../data/WorldCitiesUniqueLocs.txt", header=T) %>% tbl_df %>% rename(lon=Longitude, lat=Latitude)

# Store lat/lon coordinates for the sample weighted by sqrt of populations over 10,000
min_pop <- 10000
size <- 10000

# Sample cities, create pairs, compute distances
expand_table <- function(x, lon, lat, distance=TRUE, keep=TRUE){
  n <- nrow(x)
  if(n %% 10 == 1) n <- n - 1
  idx <- split(sample(1:n, size=n), rep(1:2, each=n/2))
  idx <- expand.grid(R1=idx[[1]], R2=idx[[2]])
  v0 <- c("lon0", "lat0", "lon1", "lat1")
  v1 <- c("lon0"=lon, "lat0"=lat)
  v2 <- c("lon1"=lon, "lat1"=lat)
  sfx <- c("0", "1")
  if(lon == "lon0") stop("Use a different column name than 'lon0'.")
  if(lat == "lat0") stop("Use a different column name than 'lat0'.")
  y <- data.table::data.table(lon0=x[[lon]][idx$R1], lat0=x[[lat]][idx$R1],
                              lon1=x[[lon]][idx$R2], lat1=x[[lat]][idx$R2]) %>% dplyr::tbl_df()
  if(keep){
    y <- dplyr::left_join(
      dplyr::left_join(y, x, by=v1, suffix=sfx),
      dplyr::left_join(y, x, by=v2, suffix=sfx),
      by=v0, suffix=sfx)
  }
  if(distance){
    distances <- geosphere::distMeeus(dplyr::select(y, lon0, lat0), dplyr::select(y, lon1, lat1))
    y <- dplyr::mutate(y, Dist=distances)
  }
  dplyr::sample_n(y, nrow(y))
}

wtsFun <- function(x) (x - min(x)) / (max(x) - min(x)) # simple rescale to [0,1]
distFun <- function(x) 1 - x / max(x) # inverse distance weighting

d1 <- filter(d_all, Population >= min_pop) %>% select(lon, lat, Population) %>%
  mutate(Pop_wts=wtsFun(sqrt(Population))) %>% sample_n(size, weight=Pop_wts) # use population-based weights
d2 <- expand_table(d1, "lon", "lat") %>% mutate(Dist_wts=distFun(Dist)) %>%
  sample_n(size, replace=TRUE, weight=(Pop_wts0 + Pop_wts1)/2 + Dist_wts) # use distance-based weights

# generate great circle arcs table
arc_paths <- function(data, lon0, lat0, lon1, lat1, n=50, breakAtDateLine=FALSE, addStartEnd=TRUE){
  x <- list(lon0, lat0)
  y <- list(lon1, lat1)
  if(is.character(n)){
    if(!n %in% names(data)) stop("If 'n' is character, it must refer to a column in 'data'.")
    if(!is.integer(data[[n]])) data[[n]] <- as.integer(data[[n]])
    n <- data[[n]]
  }
  if(length(n) != 1 & length(n) != nrow(data)) stop("'n' must have length 1 or length equal to the number of rows in 'data'.")
  if(any(n < 1)) stop("Column 'n' must contain positive integers.")
  data <- gcIntermediate(dplyr::select_(data, .dots=x), dplyr::select_(data, .dots=y),
    n=n, breakAtDateLine=breakAtDateLine, addStartEnd=addStartEnd)
  if(!is.list(data)) { rownames(data) <- NULL; data <- list(data) }
  f <- function(x, idx){
    x <- if(is.list(x)) purrr::map2(x, idx + c(0, 0.5), ~data.frame(.x, .y)) %>% bind_rows else data.frame(x, idx)
    x <- setNames(x, c("lon", "lat", "group"))
    tbl_df(x)
  }
  purrr::map2(data, seq_along(data), ~f(x=.x, idx=.y)) %>% bind_rows
}

paths <- arc_paths(d2, "lon0", "lat0", "lon1", "lat1")
paths <- arc_paths(d2, "lon0", "lat0", "lon1", "lat1", n=c(1,2))
paths <- arc_paths(d2, "lon0", "lat0", "lon1", "lat1", n=1)
paths <- arc_paths(mutate(d2, n=Dist_wts*150 + 50), "lon0", "lat0", "lon1", "lat1", n="n")
paths <- arc_paths(mutate(d2, n=round(Dist_wts*150 + 50)), "lon0", "lat0", "lon1", "lat1", n="n")

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
