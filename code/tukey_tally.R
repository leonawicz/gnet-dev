# @knitr tukey_tally
# Tukey tally plot sequence
tukeyTally <- function(i, ...){
	stopifnot(as.numeric(i) && i %in% 1:10)
	par(mai=c(0,0,0,0))
	plot(0, 0, type="n", axes=F, xlab="", ylab="", xlim=c(0, 1), ylim=c(0, 1))
	m <- matrix(c(0,1,1,0,1,1,0,0), nrow=4)

	if(i>=5) segments(m[1,1], m[1,2], m[2,1], m[2,2], ...)
	if(i>=6) segments(m[2,1], m[2,2], m[3,1], m[3,2], ...)
	if(i>=7) segments(m[4,1], m[4,2], m[3,1], m[3,2], ...)
	if(i>=8) segments(m[1,1], m[1,2], m[4,1], m[4,2], ...)
	if(i>=9) segments(m[1,1], m[1,2], m[3,1], m[3,2], ...)
	if(i==10) segments(m[4,1], m[4,2], m[2,1], m[2,2], ...)

	points(m[1,1], m[1,2], ...)
	if(i>=2) points(m[2,1], m[2,2], ...)
	if(i>=3) points(m[3,1], m[3,2], ...)
	if(i>=4) points(m[4,1], m[4,2], ...)
}

dir.create(outDir <- "C:/leonawicz/GlobalNetworks/plots/frames/tukey_ticker_10to1", showWarnings=FALSE)
for(i in 1:10){
	png(paste0(outDir, "/tukey_ticker_10to1_frame", 10-i, ".png"), bg="transparent")
	tukeyTally(i, lwd=10,  pch=19, cex=4, col="greenyellow")
	dev.off()
}
