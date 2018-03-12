# @knitr func_lines_df
lines_df <- function(i, sq, lines.list, freq=NULL, col.vec=NULL, dl.na.sep=TRUE){
	ind <- sq[i]:(sq[i+1]-1)
	x <- lines.list[ind]
	if(dl.na.sep){
		dateline.ind <- which(sapply(x, class)=="list")
		if(length(dateline.ind)) x[dateline.ind] <- lapply(x[dateline.ind], function(x) rbind(rbind(x[[1]], NA), x[[2]]))
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


# @knitr func_get_path_seq
get_path_seq <- function(i, paths, n.consec, grp.probs=NULL, seg.size="uniform", seg.frac.max=1/3, n.max.segs, rm.na.dl.brk, rm.na.tail=TRUE, add.na.grp.sep=FALSE){
	nc <- ncol(paths)
	consec.paths <- sample(unique(paths$grp), n.consec, prob=grp.probs)
	p <- data.frame(matrix(NA, n.max.segs, nc))
	names(p) <- names(paths)
	
	df_segs <- function(d, seg.size, seg.frac.max){
		n <- nrow(d)
		if(n < 3) stop("Data not appropriate for this operation.")
		if(is.numeric(seg.size)){
			stopifnot(seg.size >= 2)
			z <- round(runif(2, 2, seg.size))
			z[z > n] <- n
		} else {
			if(n*seg.frac.max < 3) stop("seg.frac.max is too small.")
			if(seg.size=="uniform"){
				z <- round(runif(2, 2, n*seg.frac.max))
			} else if(seg.size=="normal") {
				mn <- mean(1:n)
				stddev <- mn/6
				z <- round(rnorm(2, mn, stddev))
			}
		}
		n1 <- ceiling(diff(c((z[1] - z[2]), n))/z[1])
		
		f <- function(k, d, n, n1, z){
			ind2 <- z[1]*k
			ind1 <- max(ind2 - z[2], 1)
			if(ind2 > n) ind2 <- n
			ind <- ind1:ind2
			d.sub <- d[ind,]
			d.sub$grp <- paste(d.sub$grp, k, sep=".")
			d.sub
		}
		
		d <- do.call(rbind, lapply(1:n1, f, d=d, n=n, n1=n1, z=z))
		d
	}

	for(j in 1:n.consec){
		gc()
		a <- subset(paths, grp==consec.paths[j])
		if(rm.na.dl.brk) a <- a[!is.na(a[,1]),]
		a$grp <- paste(a$grp, i, sep=".")
		a <- df_segs(a, seg.size, seg.frac.max)
		if(j==1){
			p[1:nrow(a),] <- a
		} else {
			gap <- if(add.na.grp.sep) 2 else 1
			start.ind <- tail(which(!is.na(p[,1])), 1) + gap
			end.ind <- start.ind + nrow(a) - 1
			if(end.ind > n.max.segs) break
			p[seq(start.ind, end.ind),] <- a
		}
	}
	#if(rm.na.grp.sep) p <- p[!is.na(p[,1]),] # fix this, it conflicts with rm.na.dl.brk
	if(!add.na.grp.sep) { na.ind <- which(substr(rownames(p), 1, 1)=="N"); if(length(na.ind)) p <- p[-na.ind,] }# fix this, it conflicts with rm.na.dl.brk
	if(rm.na.tail){
		na.tail.1 <- tail(which(!is.na(p[,1])), 1) + 1
		if(na.tail.1 <= nrow(p)) p <- p[1:(na.tail.1 - 1),]
	}
	print(i)
	p
}

# @knitr func_get_frames_df
get_frames_df <- function(i, p.list, n.frames, paths.per.frame=NULL, cycle.frames=TRUE){
	print(n.frames-i)
	if(is.numeric(paths.per.frame) && length(paths.per.frame)==n.frames){
		p.list <- p.list[1:paths.per.frame[i]]
	}
	segs <- lapply(1:length(p.list),
		function(k, x, z){
			glk <- groups.list[[k]]
			gz <- glk[z]
			if(cycle.frames && length(glk) < z) gz <- tail(rep(glk, length=z), 1)
			x[[k]][x[[k]]$grp==gz,]
		}, x=p.list, z=i)
	ind.drp <- which(sapply(segs, function(x) nrow(x) < 3 && any(is.na(x))))
	if(length(ind.drp)) segs <- segs[-ind.drp]
	if(length(segs) > 1) d <- do.call(rbind, segs) else d <- NULL
	d
}
