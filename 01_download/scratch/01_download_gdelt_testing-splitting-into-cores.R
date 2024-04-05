# _______________________________#
# Environment
# download 01: download from GDELT and extract datasets
# 
# Stallman
# Started: 2023-05-29
# Last edited: 
#________________________________#


# Startup

rm(list = ls()) 

library(parallel)

# Functions ----

split_vector_to_list <- function(vector,
                                 n_chunks) {
  
  # Create a vector to split the data set up by.
  split_vector <- rep(1:n_chunks, each = length(vector) / n_chunks, length.out = length(vector))
  
  split_list   <- split(vector,split_vector)
  
  return(split_list)
}


# split into chunks that have the same number of elements in each chunk as the number of cores allocated
# and possibly a remainder chunk 

split_to_parallelize <- function(vector,
                                 n_cores) {
  
  # test if length(vector) is perfectly divisible by n_chunks
  
  perfectly_divisible <- length(vector)%%n_cores
  
  # if so, split the vector into these chunks
  if(perfectly_divisible==0){ 
    
    # Create a vector to split the data set up by.
    split_vector <- rep(1:n_chunks, each = length(vector) / n_cores, length.out = length(vector))
    
    split_list   <- split(vector,split_vector) 
    
    return(split_list)
  }
  
  else{
    
    # if not perfectly divisible, truncate and split, and then add an iter that's the remainder at the end
    truncate_length <- length(vector)-perfectly_divisible
    split_vector <- rep(1:n_chunks, each = truncate_length / n_chunks, length.out = truncate_length)
    split_list_tmp   <- split(vector[1:truncate_length],split_vector) 
    
    append_index_start   <- truncate_length+1
    append_index_end     <- truncate_length+perfectly_divisible
    
    
    split_list  <- append(split_list_tmp,list(vector[append_index_start:append_index_end]))
    return(split_list)
  }
  
}



n_cores <- detectCores() - 4


# create an iterator for going through the script
## Run first time through to set
# iter <- 1
# saveRDS(iter, file = file.path("P:","Projects","environment","code","01_download","iter.R"))

#iter     <- readRDS(file.path("P:","Projects","environment","code","01_download","iter.R"))

iter <- 1
# dates we're interested in.
## Finished: all of 2018

year       <- "2019"
#start_md   <- "0101"
# done through: 07 19
start_md     <- "0101"
end_md       <- "1231"

#end_md   <- "1231"


start_ymd <- paste0(year,start_md)
end_ymd   <- paste0(year,end_md)


# create date sequence
dates <- as.character(seq(as.Date(start_ymd,"%Y%m%d"), as.Date(end_ymd,"%Y%m%d"), by = "day"))
vector <- dates

print(paste0("Number of days to allocate: ",length(dates),"."))
# split the date sequence into this many iterations
# round or floor gets the lower limit; puts a little extra 
n_iters <- floor(length(dates)/n_cores)

n_chunks <- n_iters

print(paste0("N iters: ",n_iters))
# split the dates into iterations





iters_list <- split_to_parallelize(vector = dates,
                                   n_chunks = n_iters)

n_iters <- length(iters_list)
iters_list

this_iter_dates <- split_vector_to_list(vector = iters_list[[iter]],
                                        n_chunks = n_cores)


