comargs <- (commandArgs(TRUE))
if(!length(comargs)) q("no") else for(i in 1:length(comargs)) eval(parse(text=comargs[[i]]))

if(interactive()) ID <- 1

# @knitr setup_cities_seq_batch
# cities sequence frame batch
setwd("/workspace/UA/mfleonawicz/leonawicz/GlobalNetworks/workspaces")
#setwd("C:/leonawicz/GlobalNetworks/workspaces")
#load("cities_colors_ordered2.RData") # v2
load("cities_colors_ordered_DrkGrnYelWht.RData") # v3
#load("points.RData")

library(parallel)
#set.seed(47)
n.cores <- 32

source("../code/path_seq_functions.R")

name <- "cities_sequence3"

# Choose between breaking at dateline (2D maps) or continuous paths (3D globes)
dateline.break <- TRUE
if(dateline.break) rm.na.dl.brk <- FALSE else rm.na.dl.brk <- TRUE

# @knitr city_path_sequence
n.consec <- 60 # consecutive paths, defines length of time sequence and series of paths
n.concur <- 1024 # concurrent paths on map for a single time stamp
n.max.segs <- 10001 # maximum sequential segmanents to plot, if/once filled, get_path_seq() will break/exit

sample.ind <- sort(sample(unique(paths$grp), 50000))
paths.sample <- paths[paths$grp %in% sample.ind,]

start <- Sys.time()
p.list <- mclapply(1:n.concur, get_path_seq, paths=paths.sample, seg.size=5, n.consec=n.consec, n.max.segs=n.max.segs, rm.na.dl.brk=rm.na.dl.brk, mc.cores=n.cores)
file.out <- if(dateline.break) paste0("intermediary/", name, "_breakDL_ws", ID, ".RData") else paste0("intermediary/", name, "_continuous_ws", ID, ".RData")
save(p.list, file=file.out)
end <- Sys.time()
difftime(end, start)
