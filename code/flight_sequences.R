
# @knitr setup_flight_sequence
setwd("/workspace/UA/mfleonawicz/leonawicz/GlobalNetworks/workspaces")
#setwd("C:/leonawicz/GlobalNetworks/workspaces")

library(parallel)
set.seed(47)
n.cores <- 32

source("../code/path_seq_functions.R")

# Choose between breaking at dateline (2D maps) or continuous paths (3D globes)
dateline.break <- TRUE
if(dateline.break){
	load("flights_breakDL.RData")
	rm.na.dl.brk <- FALSE
} else {
	load("flights_continuous.RData")
	rm.na.dl.brk FALSE # still false, pass appropriate data, do not waste time doing this check in get_path_seq
}

#hist(tapply(1:nrow(paths), paths$grp, length)) # distribution of line segments per flight
grp.probs <- tapply(paths$freq, paths$grp, function(x) unique(x))
grp.probs[is.na(grp.probs)] <- 1 # temporarily downweight
n.consec <- 120 # consecutive paths, defines length of time sequence and series of paths
n.concur <- 10000 # concurrent paths on map for a single time stamp
n.max.segs <- 3601 # maximum sequential segmanents to plot, if/once filled, get_path_seq() will break/exit

# @knitr flight_sequence
start <- Sys.time()
p.list <- mclapply(1:n.concur, get_path_seq, paths=paths, n.consec=n.consec, grp.probs=grp.probs, n.max.segs=n.max.segs, rm.na.dl.brk=rm.na.dl.brk, mc.cores=n.cores)

groups.list <- lapply(p.list, function(x) unique(x$grp[!is.na(x$lon)]))
n.frames <- min(sapply(groups.list, length))

df.list <- mclapply(1:n.frames, get_frames_df, p.list=p.list, n.frames=n.frames, mc.cores=n.cores)

file.out <- if(dateline.break) "flight_sequence_breakDL_10kboot.RData" else "flight_sequence_continuous_10kboot.RData"
save(df.list, n.frames, file=file.out)
end <- Sys.time()
difftime(end, start)
