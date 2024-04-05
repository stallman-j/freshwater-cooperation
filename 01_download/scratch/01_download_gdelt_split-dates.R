
# create an iterator for going through the script
## Run first time through to set
#iter <- 1
#saveRDS(iter, file = file.path("P:","Projects","environment","code","01_download","iter.R"))

iter     <- readRDS(file.path("P:","Projects","environment","code","01_download","iter.R"))
print(paste0("iteration number: ",iter))

n_cores <- 20


year       <- "2019"
start_md <- "0101"
end_md   <- "1231"


start_ymd <- paste0(year,start_md)
end_ymd   <- paste0(year,end_md)



dates <- as.character(seq(as.Date(start_ymd,"%Y%m%d"), as.Date(end_ymd,"%Y%m%d"), by = "day"))

desired_iters <- 10

iters_list <- split_vector_to_list(vector = dates,
                                     n_chunks = desired_iters)


this_iter_dates <- split_vector_to_list(vector = iters_list[[iter]],
                                       n_chunks = n_cores)


max_iter <- length(starts)

if (iter>max_iter)stop("Reached end, stopping")


iter <- iter+1
#saveRDS(iter, file = file.path("P:","Projects","environment","code","01_download","iter.R"))


library(parallel)
n_cores <- detectCores() - 4

cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(gdeltr2)
})

tic()

parLapply(cl,this_iter_dates,gdelt_gkg_one_day)
toc()