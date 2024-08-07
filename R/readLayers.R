

#' Read TSA boundaries
#'
#' @param tsa which tsas. Must match exactly. To do - provide a list of valid tsa names
#' @param gdbpath
#' @param layername name of the shapefile contained in a geodatabase
#'
#' @return
#' @export
#'
#' @examples
read_tsa <- function(gdbpath=NULL,layername=NULL,tsa = "Lakes TSA"){
  #if no gdbpath given, then download the tsa from bc data catalogue
  if(is.null(gdbpath)){
    #bcdc_get_record("fadm-timber-supply-area-tsa")
    #bcdc_query_geodata("8daa29da-d7f4-401c-83ae-d962e3a28980")

    #get the TSA to collect other layers:
    tsa_bound <- bcdata::bcdc_query_geodata("8daa29da-d7f4-401c-83ae-d962e3a28980") %>%
      filter(TSA_NUMBER_DESCRIPTION==tsa) %>%
      collect()

    #I think this will give the right polygon - but could be glitchy
    #to do test other tsas
    tsa_bound <- tsa_bound %>%
      dplyr::filter(is.na(TSB_NUMBER))
    #st_write(tsa_bound,"./Inputs/ltsa.gpkg")
  }else{
    tsa_bound <- st_read(dsn=gdbpath, layer = layername)
  }
  return(tsa_bound)
}



#' Read habitat model (vri_bem) layer
#'
#' @param gdbpath
#' @param layername
#'
#' @description
#' it's possible to use the output of SSGM-VRI-BEM (vri_bem.shp) here, but more likely
#' it'll always be passed as a shapefile stored locally (or downloaded from sync)
#'
#' @return
#' @export
#'
#' @examples
read_vri_bem <- function(vri_bem_path=NA,layername,inputPath=NA){

  if(is.na(vri_bem_path)){

    Hab_layer <- st_read(paste0(inputPath,layername))

  }else{

    Hab_layer <- st_read(dsn = vri_bem_path, layer = layername)

  }

  return(Hab_layer)
}


#' Read cutblock layer
#' @param aoi area of interest to download the consolidated cutblock layer.
#' Right now, just using tsa, but could replace with any area of interest
#' @return
#' @export
#'
#' @examples
read_cutblocks <- function(aoi,gdbpath=NULL,layername=NULL){
  #to do: make this non-tsa based
  #area_poly <- read_tsa(tsa = aoi)

  if(is.null(gdbpath)){

    aoi_ccb <- read_tsa(tsa = aoi)

    ccb <- bcdata::bcdc_query_geodata("b1b647a6-f271-42e0-9cd0-89ec24bce9f7") %>%
      filter(WITHIN(aoi_ccb)) %>% #filter for records that are within the DPG boundary
      collect() #collect/download the data
    ccb <- st_make_valid(ccb)
    #st_write(ccb, "./Inputs/ccb.gpkg", append=FALSE)
  }else{
    ccb <- st_read(dsn = gdbpath, layer = layername)
  }


  return(ccb)
}


#' Read the roads layer
#'
#' @param aoi name of TSA. default set to "Lakes TSA"
#' @param aoi_gdbpath if no TSA name provided, where is the gdb containing the AOI
#' @param aoi_layername if no TSA name provided, what is the layername for the AOI
#' @param savetofile
#'
#' @return
#' @export
#'
#' @details
#' You have the option of saving the roads layer to file
#'
#'
#' @examples
get_roads <- function(aoi = "Lakes TSA", aoi_gdbpath=NULL, aoi_layername=NULL,
                      savetofile = FALSE){

    #get boundary
    if(is.character(aoi)){
      aoi_bound <- read_tsa(tsa = aoi, gdbpath = NULL, layername = NULL)
    }else{
      aoi_bound <- st_read(dsn = aoi_gdbpath, layer = aoi_layername)
    }

    #download the roads layer from bcdc:
    rds <- bcdata::bcdc_query_geodata("bb060417-b6e6-4548-b837-f9060d94743e") %>%
      filter(INTERSECTS(aoi_bound)) %>% #filter for records that are within the DPG boundary
      collect() #collect/download the data

    if(savetofile==TRUE){
      st_write(rds,"roads.gpkg")
    }


  return(rds)

}

#' Read historic fire perimeter polygons
#'
#'
#' @return sf object
#' @import sf
#' @export
read_fire <- function(aoi, firepath = NULL, fire_layer = NULL) {

  #If gdbpath is null read information from bcdata
  if(is.null(firepath)){

    aoi_fire <- read_tsa(tsa = aoi)

    fire_p <- bcdc_query_geodata(record =  "22c7cb44-1463-48f7-8e47-88857f207702") %>%
      filter(INTERSECTS(aoi_fire)) %>% #filter for records that are within the DPG boundary
      collect() %>% #collect/download the data
      dplyr::select(.,c(FIRE_NUMBER,FIRE_YEAR))

    fire_c <- bcdc_query_geodata(record = "cdfc2d7b-c046-4bf0-90ac-4897232619e1") %>%
      filter(INTERSECTS(aoi_fire)) %>% #filter for records that are within the DPG boundary
      collect()

    if (nrow(fire_c) == 0) {
      print("no fire records in 2023")

      fire <- fire_p

    }else{
      fire_c <- fire_c %>%
        dplyr::select(.,c(FIRE_NUMBER,FIRE_YEAR))

      fire <- rbind(fire_p,fire_c)
    }

    fire <- st_make_valid(fire)


  }else{
    fire <- st_read(dsn = firepath, layer = fire_layer, quiet = TRUE)
  }

  fire <- sf::st_make_valid(fire)
  return(fire)
}


#' Title
#'
#' @param huck_path
#' @param huck_layername
#'
#' @return
#' @export
#'
#' @importFrom terra rast
#'
#' @examples
read_huckleberry <- function(huck_path, huck_layername){

  huck_layer <- rast(paste0(huck_path,huck_layername))

  return(huck_layer)

}



#' Title
#'
#' @param wbp_path
#' @param wbp_layername
#'
#' @return
#' @export
#'
#' @importFrom terra rast
#'
#' @examples
read_wbp <- function(wbp_path, wbp_layername){

  wbp_layer <- rast(paste0(wbp_path, wbp_layername))

  return(wbp_layer)

}

#' Read territorial boundaries
#'
#' @param FN_bounds_path
#' @param FN_bounds_layernames
#' @param FN_names description
#'
#' @return
#' @export
#'
#' @importFrom terra rast
#'
#' @examples
read_FN_bounds <- function(FN_bounds_path, FN_bounds_layernames, FN_names){

  FN_file_names <- paste0(FN_bounds_path,FN_bounds_layernames)

  FN_bounds <-  FN_file_names %>%
    map2(FN_names, ~read_sf(.x) %>%
           mutate(FN_bound = .y)) %>%
    map(~st_transform(., crs = 3005)) %>%
    map(~select(., FN_bound, geometry)) %>%
    reduce(rbind)


  return(FN_bounds)

}


