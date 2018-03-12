# Vectorized grid cell corner coordinates
nbrVertices <- function(i, x, m, res, previous=NULL){
	require(prodlim)
	x <- if(is.matrix(x)) x[i,] else if(is.list(x)) x[[i]] else if(is.numeric(x) && length(x)==2) matrix(x, nrow=1)
	if(is.matrix(x) && nrow(x) > 1) x <- tail(x, 1)
	x <- do.call(rbind, list(x + c(0, res[2]), x - c(0, res[2]), x + c(res[1], 0), x - c(res[1], 0)))
	ind <- row.match(data.frame(m), x)
	ind <- sort(ind[!is.na(ind)])
	probs <- rep(1, length(ind))
	if(!is.null(previous)){
		prev.ind <- which(as.numeric(x[ind,1])==previous[[i]][1] & as.numeric(x[ind,2])==previous[[i]][2])
		if(length(prev.ind)) probs[prev.ind] <- 0.1
	}
	x[ind[sample(1:length(ind), 1, prob=probs)],]
}

# Vectorized grid path coordinates
gridPaths <- function(x, m, res, recur=1, previous=NULL){
	if(!is.list(x)){
		if(!is.matrix(x)) if(is.numeric(x) && length(x)==2) x <- matrix(x, nrow=1)
		hold <- vector("list", nrow(x))
		for(i in 1:length(hold)) hold[[i]] <- x[i,]
	} else hold <- x
	new <- lapply(1:length(hold), nbrVertices, x=x, m=m, res=res, previous=previous)
	hold <- lapply(1:length(hold), function(i, new, old) rbind(old[[i]], new[[i]]), new=new, old=hold)
	recur <- recur - 1
	if(recur > 0) Recall(x=hold, m=m, res=res, recur=recur, previous=new) else return(hold)
}

# Parallelized wrapper function
par_gridPaths <- function(k, ...){
	x <- list(...)$x.list[[k]]
	m <- list(...)$m
	res <- list(...)$res
	recur <- list(...)$recur
	gridPaths(x, m=m, res=res, recur=recur)
}

# Interpolate along grid paths
interpGridPaths <- function(x, fac=10){
	x <- matrix(rep(x, each=fac), ncol=2)
	m.ind <- mapply(seq, seq(2, nrow(x)-fac, by=fac), seq(fac+1, nrow(x), by=fac))
	for(i in 1:ncol(m.ind)){
		interp.ind <- which(apply(diff(x[m.ind[,i],]), 2, function(x) any(x!=0)))
		r <- unique(x[m.ind[,i], interp.ind], na.rm=TRUE)
		x[m.ind[,i], interp.ind] <- seq(r[1], r[2], length=fac)
	}
	x <- x[-c(1, (nrow(x) - fac + 1):nrow(x)),]
	x
}


# frames.args is a list that takes limited arguments and passes them to internal png and image calls
# row and column cell probability vectors are independent
fill_grid <- function(x, size=1, frames.args=list(file="frame", w=3000, h=1500, bg="transparent"), fill="ordered", byrow=FALSE, prob.row=NULL, prob.col=NULL){
	stopifnot(is.matrix(x))
	nr <- nrow(x)
	nc <- ncol(x)
	n <- nr*nc
	stopifnot(size <= n)
	stopifnot(fill %in% c("ordered", "random"))
	if(fill=="random"){
		if(!is.null(prob.row) && length(prob.row) != nr) stop("Row cell probabilities must have length==nrow(x).")
		if(!is.null(prob.col) && length(prob.col) != nc) stop("Column cell probabilities must have length==ncol(x).")
	}
	
	plot_mat <- function(mdata, frame.number, fill, byrow, file="frame", w=3000, h=1500, bg="transparent", clrs=heat.colors(30), alpha=NULL){
		if(fill=="ordered" & !byrow) mdata <- t(mdata)
		frame.ind <- paste0(paste0(rep(0, 4-nchar(frame.number)), collapse=""), frame.number)
		png(filename=paste0(file, frame.ind, ".png"), width=w, height=h, bg=bg)
		par(mai=c(0,0,0,0))
		image(mdata, col=clrs, useRaster=T, axes=F)
		dev.off()
	}
	
	alpha <- frames.args$alpha
	if(!is.null(alpha)){
		alpha <- max(1, min(100*alpha, 99))
		if(alpha < 10) alpha <- paste0(0, alpha)
		frames.args$clrs <- paste0(frames.args$clrs, alpha)
	}
	end.ind <- unique(round(c(seq(size, n, by=size), n)))
	if(fill=="ordered" & !byrow) x <- t(x)
	x.tmp <- x
	x.tmp[] <- NA
	k0 <- k <- 1
	ind <- k0:end.ind[k]
	frames.args$byrow <- byrow
	frames.args$fill <- fill
	if(fill=="ordered") {
		x.tmp[ind] <- x[ind]
		frames.args$mdata <- x.tmp
		frames.args$frame.number <- k
		do.call(plot_mat, frames.args)
		for(k in 2:length(end.ind)){
			k0 <- end.ind[k-1] + 1
			ind <- k0:end.ind[k]
			x.tmp[ind] <- x[ind]
			frames.args$mdata <- x.tmp
			frames.args$frame.number <- k
			do.call(plot_mat, frames.args)
			print(length(end.ind)-k)
		}
	} else if(fill=="random") {
		if(is.null(prob.row)) prob.row <- rep(rep(1, nr), nc) else prob.row <- rep(prob.row, nc)
		if(is.null(prob.col)) prob.col <- rep(rep(1, nc), each=nr) else prob.col <- rep(prob.col, each=nr)
		probs <- prob.row*prob.col
		grd <- as.matrix(expand.grid(1:nr, 1:nc))
		rows <- sample(1:n, size=n, replace=FALSE, prob=probs)
		grd <- grd[rows,]
		ind <- grd[ind,]
		x.tmp[ind] <- x[ind]
		frames.args$mdata <- x.tmp
		frames.args$frame.number <- k
		do.call(plot_mat, frames.args)
		for(k in 2:length(end.ind)){
			k0 <- end.ind[k-1] + 1
			ind <- k0:end.ind[k]
			ind <- grd[ind,]
			x.tmp[ind] <- x[ind]
			frames.args$mdata <- x.tmp
			frames.args$frame.number <- k
			do.call(plot_mat, frames.args)
			print(length(end.ind)-k)
		}
	}
}
