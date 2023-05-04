

#' Get TSA boundaries
#' @param tsa which tsas. Must match exactly. To do - provide a list of valid tsa names
#' @return
#' @export
#'
#' @examples
get_tsa <- function(tsa = "Lakes TSA"){
  #get the TSA boundaries:
  #bcdc_get_record("fadm-timber-supply-area-tsa")
  #bcdc_query_geodata("8daa29da-d7f4-401c-83ae-d962e3a28980")

  #get the TSA to collect other layers:
  tsa_bound <- bcdc_query_geodata("8daa29da-d7f4-401c-83ae-d962e3a28980") %>%
    filter(TSA_NUMBER_DESCRIPTION==tsa) %>%
    collect()

  #I think this will give the right polygon - but could be glitchy
  #to do test other tsas
  tsa_bound <- tsa_bound %>%
    dplyr::filter(is.na(TSB_NUMBER))
  #st_write(tsa_bound,"./Inputs/ltsa.gpkg")

  return(tsa_bound)
}



#' Read habitat model (vri_bem) layer
#'
#' @param dsn
#' @param layer
#'
#' @description
#' it's possible to use the output of SSGM-VRI-BEM (vri_bem.shp) here, but more likely
#' it'll always be passed as a shapefile stored locally (or downloaded from sync)
#'
#' @return
#' @export
#'
#' @examples
read_vri_bem <- function(dsn,layer){
  Hab_layer <- st_read(dsn = dsn, layer = layer)
  return(Hab_layer)
}


#' Get cutblock layer
#' @param tsa area of interest to download the consolidated cutblock layer.
#' Right now, just using tsa, but could replace with any area of interest
#' @return
#' @export
#'
#' @examples
get_cutblocks <- function(tsa){
  area_poly <- get_tsa(tsa = tsa)

  ccb <- bcdc_query_geodata("b1b647a6-f271-42e0-9cd0-89ec24bce9f7") %>%
    filter(WITHIN(area_poly)) %>% #filter for records that are within the DPG boundary
    collect() #collect/download the data
  ccb <- st_make_valid(ccb)
  #st_write(ccb, "./Inputs/ccb.gpkg", append=FALSE)
  return(ccb)
}


#' Title
#'
#' @param tsa
#'
#' @return
#' @export
#'
#' @examples
get_roads <- function(tsa){
  rds <- bcdc_query_geodata("b1b647a6-f271-42e0-9cd0-89ec24bce9f7") %>%
    filter(INTERSECTS(ltsa)) %>% #filter for records that are within the DPG boundary
    collect() #collect/download the data

  return(rds)

}


