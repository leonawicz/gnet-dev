save2Dplot <- function(i, outDir, w, h, pts, pt.frac=0.75, line.sublayer=FALSE, bg="transparent", increase.alpha=NULL, min.alpha=0, background.image=NULL, check.dl.brk=FALSE, txt=NULL){
	if(!is.null(pts)) { nrp <- nrow(pts); rows <- sample(1:nrp, round(pt.frac*nrp)) }
	d <- df.list[[i]]
	names(d)[1:2] <- c("lon", "lat")
	file <- paste0(outDir, "/", basename(outDir), "_frame", ind[i], ".png")
	png(file, width=w, height=h, bg=bg)
	g <- ggplot(data=data.frame(x=c(-180, 180, 180, -180), y=c(-90, -90, 90, 90)), aes(x=x, y=y)) +
		theme_nothing() + theme(plot.background=element_rect(color="transparent", fill="transparent"), panel.background=element_rect(fill="transparent",color = NA))
	if(!is.null(background.image)) g <- g + annotation_custom(rasterGrob(background.image, width=unit(1,"npc"), height=unit(1,"npc")), -Inf, Inf, -Inf, Inf)
	g <- g + scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) + geom_point(aes(x, y), color="transparent", size=0.1)
	if(!is.null(pts)) g <- g + geom_point(data=pts[rows,], aes(x=Longitude, y=Latitude), size=1, colour="white", alpha=runif(1, 0.7, 0.9))
	if(line.sublayer) g <- g + geom_path(data=d, aes(x=lon, y=lat, group=grp), color="white", size=1, alpha=runif(1, 0.35, 0.65))
	uni.clrs <- unique(d$Color)
	for(j in 1:length(uni.clrs)){
		d.sub <- subset(d, Color==uni.clrs[j])
		if(check.dl.brk){
			dl.ind <- which(abs(diff(d.sub[,1])) == 360)
			if(length(dl.ind)){
				for(k in 1:length(dl.ind)){
					a <- dl.ind[k]
					grp <- d.sub$grp[a]
					d.sub$grp[1:a][d.sub$grp[1:a]==grp] <- paste0(grp, ".a")
				}
			}
		}
		clr <- if(is.null(increase.alpha)) uni.clrs[j] else paste0(substr(uni.clrs[j], 1, 7), max(min.alpha, min(99, (as.numeric(substr(uni.clrs[j], 8, 9)) + max(10, increase.alpha)))))
		g <- g + geom_path(data=d.sub, aes(lon, lat, group=grp), color=clr, size=2)#, alpha=runif(1, 0.35, 0.65))
		print(length(uni.clrs)-j)
	}
	if(!is.null(txt)) g <- g + annotate(geom="text", x=170, y=-75, label=txt[i], family="Courier", fontface="bold", color="white", size=50, hjust=1, vjust=1)
	print(g)
	dev.off()
}
