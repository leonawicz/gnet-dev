
# @knitr setup_gc
setwd("/workspace/UA/mfleonawicz/leonawicz/GlobalNetworks/workspaces")
load("flights.RData")
library(parallel)
library(geosphere)

# Choose between breaking at dateline (2D maps) or continuous paths (3D globes)
dateline.break <- TRUE

n.cores <- 32
sq <- round(seq(1, n+1, length.out=n.cores + 1))

rel.dst <- ((dst - min(dst))/diff(range(dst)))
rel.dst[rel.dst < 0.1] <- 0.1
rel.dst <- sqrt(rel.dst)
rel.dst <- round(60*(1 + rel.dst))

# @knitr gclines
get_gc_lines <- function(i, sq, x1y1, x2y2, dst, dl.brk=TRUE, dl.na.sep=TRUE){
	ind <- sq[i]:(sq[i+1]-1)
	x1y1 <- x1y1[ind,]
	x2y2 <- x2y2[ind,]
	d <- dst[ind]
	gclines <- gcIntermediate(x1y1, x2y2, n=d, addStartEnd=T, breakAtDateLine=dl.brk, sepNA=F)  ## Obtain great circle arcs (about 20 secs for 10K pairs)
	if(dl.na.sep){
		dateline.ind <- which(sapply(gclines, class)=="list")
		if(length(dateline.ind)) gclines[dateline.ind] <- lapply(gclines[dateline.ind], function(x) rbind(rbind(x[[1]], NA), x[[2]]))
	}
	gclines
}

gc_lines_df <- function(i, sq, gclines, freq=NULL, col.vec=NULL, dl.na.sep=TRUE){
	ind <- sq[i]:(sq[i+1]-1)
	x <- gclines[ind]
	if(dl.na.sep){
		dateline.ind <- which(sapply(paths, class)=="list")
		if(length(dateline.ind)) paths[dateline.ind] <- lapply(paths[dateline.ind], function(x) rbind(rbind(x[[1]], NA), x[[2]]))
	}
		
	x <- lapply(1:length(ind),
		function(i, x, freq, clr, ind) {
			p <- ind[i]
			x <- data.frame(x[[i]])
			x$grp=p
			if(!is.null(freq)) x$freq=freq[p]
			if(!is.null(clr)) x$Color <- col.vec[p]
			x
		}, x=x, freq=freq, clr=col.vec, ind=ind)
	x <- do.call(rbind, x)
}

gclines <- mclapply(1:n.cores, get_gc_lines, sq=sq, x1y1=ab[,1:2], x2y2=ab[,3:4], dst=rel.dst, dl.brk=dateline.break, mc.cores=n.cores)

gclines <- do.call(c, gclines)
cl <- sapply(gclines, class)
(cl.ind <- which(cl=="list")) # test, should be none
#if(length(cl.ind)){
#	gclines <- c(do.call(c, gclines[cl.ind]), gclines[-cl.ind])
#	n <- length(gclines)
#	sq <- round(seq(1, n+1, length.out=n.cores + 1))
#}

paths <- mclapply(1:n.cores, gc_lines_df, sq=sq, gclines=gclines, freq=freq, mc.cores=n.cores)
paths <- do.call(rbind, paths)
paths <- paths[order(paths$freq),]
file.out <-  if(dateline.break) "flights_breakDL.RData" else "flights_continuous.RData"
save(paths, file=file.out)
