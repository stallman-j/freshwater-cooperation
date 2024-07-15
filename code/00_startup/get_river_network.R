#' Get River Network from HydroSHEDS
#' @description from points_data which has lat, long, an id variable, and country assigned, as well as a main_river which defines a river network, get the lat-long coordinates of the nearest point on the river network to all points in the points_data data frame
#' @param current_main_river a specification in hydro rivers of a MAIN_RIV, character vector
#' @param out_path the place to put the cleaned river networks. The cleaned file will be called "MAIN_RIV_,current_main_river,_cleaned_river_network.rds"
#' @param rivers_data HydroRIVERS data frame
#' @param long_river_threshold default 4000 segments. If the river has more segments than this, just don't let it run
#' @param long_rivers_path the place to put a path for whether the river is a long river (i.e. more segments than long_river_threshold permits)
#' @export
#' 

get_river_network <- function(current_main_river,
                              out_path = file.path("E:","data","03_clean","HydroSHEDS"),
                              long_rivers_path = file.path("E:","data","02_temp","HydroSHEDS","long-rivers"),
                              long_river_threshold = 4000,
                              rivers_data = hydro_rivers # DO NOT Uncomment this - including it caused a recursive error
){
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(
    dplyr,
    riverdist
  )
  
  if (!dir.exists(long_rivers_path)) dir.create(long_rivers_path, recursive = TRUE) 
  
  if (file.exists(file = file.path(out_path,"river_networks",paste0("MAIN_RIV_",current_main_river,"_cleaned_river_network.rds")))){
    
    print(paste0("Current river network for main_river ",current_main_river," already exists. No need to create."))
    
  } else {
    single_river <- rivers_data %>%
      dplyr::filter(MAIN_RIV==current_main_river)%>%
      dplyr::mutate(width = 1/as.numeric(ORD_CLAS)) 
    
    if (nrow(single_river)>long_river_threshold){
      
      saveRDS(current_main_river,file = file.path(long_rivers_path,paste0("long_river_MAIN_RIV_",current_main_river,".rds")))
    } else {
      
      mouth_segment <- which(single_river$NEXT_DOWN == 0)
      
      current_river_network <- riverdist::line2network(sf = single_river)
      
      current_river_network$mouth$mouth.seg <- mouth_segment
      
      # get mouth vertex 
      # how many vertices are there on this segment of the river
      len <- dim(current_river_network$lines[[mouth_segment]])[1]
      
      # come back to this one, use showends() function to look at which vertex should be set as the final one
      #showends(seg = mouth_segment, rivers = current_river_network)
      #riverdist::zoomtoseg(seg = c(mouth_segment), rivers = current_river_network)
      
      # oftentimes getting the mouth.vert will require manually looking at the beginning and end segments
      current_river_network$mouth$mouth.vert<- len
      
      # try cleaning up the river network
      
      # tic(paste0("cleaning up river network",main_river," with ",nrow(single_river)," segments"))
      # # started around 6:26pm 2023-12-05
      # #current_river_network_fix <- cleanup(current_river_network)
      # 
      # #current_river_network <- current_river_network_fix
      # 
      # 
      # toc()
      
      riverdist::buildsegroutes(current_river_network,
                                lookup = TRUE,
                                verbose = FALSE)
      
      saveRDS(current_river_network, file = file.path(out_path,"river_networks",paste0("MAIN_RIV_",current_main_river,"_cleaned_river_network.rds")))
      
      rm(current_river_network)
      
      gc()
    } # end if-else that if river is really big we'll deal with it later
  } # end if-else that if river network already exists we won't create it
}
