# @knitr setup_gridlines_seq_combine
# Combine grid lines sequence frame batches
setwd("/workspace/UA/mfleonawicz/leonawicz/GlobalNetworks/workspaces")

library(parallel)
n.cores <- 32

source("../code/path_seq_functions.R")

name <- "gridlines_sequence2"
ws.files <- list.files("intermediary", pattern=paste0(name, "_ws.*.RData"), full=TRUE)
for(i in 1:length(ws.files)){
	load(ws.files[i])
	p.list <- lapply(p.list, function(x, i) { x$grp <- paste0(x$grp, ".batch", i); x }, i=i)
	p.list.final <- if(i==1) p.list else c(p.list.final, p.list)
	print(length(ws.files)-i)
}

rm(p.list)
gc()

print(length(p.list.final)) # concurrent pathways
print(60*length(p.list.final)) # total pathways

system.time( groups.list <- lapply(p.list.final, function(x) unique(x$grp[!is.na(x[,1])])) )
n.frames <- sapply(groups.list, length)
print(sum(n.frames)) # total pathway lines: number of concurrent pathways * this sum
print(summary(n.frames))

n.rows <- sapply(p.list.final, nrow)
print(sum(n.rows)) # total pathway coordinates
print(summary(n.rows))

p.list.final <- p.list.final[rev(order(n.frames))] # sort by descending number of frames
system.time( groups.list <- lapply(p.list.final, function(x) unique(x$grp[!is.na(x[,1])])) )
n.frames <- sapply(groups.list, length)

n.frames <- 2400 # v1 # 
n.intro <- 30
size.intro <- 100
n.outro <- 1200 # v1 # 
ppf.seq <- c(rep(0, n.intro), seq(0, 1, length=n.frames-n.intro-n.outro), rep(1, n.outro))
ppf.seq <- ppf.seq^(3)
ppf.seq <- round(ppf.seq*(length(p.list.final) - size.intro)) + size.intro
ppf.seq.txt <- formatC(ppf.seq, width=5, flag="0", format="d")
ind <- ppf.seq < 10000
ppf.seq.txt[ind] <- substr(ppf.seq.txt[ind], 2, nchar(ppf.seq.txt[ind]))

system.time( df.list <- mclapply(1:n.frames, get_frames_df, p.list=p.list.final, n.frames=n.frames, paths.per.frame=ppf.seq, mc.cores=n.cores) )
gc()

dateline.break <- TRUE
file.out <- if(dateline.break) paste0(name, "_final.RData") #else paste0("cities_sequence3_continuous_final.RData")
save(df.list, n.frames, ppf.seq.txt, file=file.out)
