

#' Join with land ownership
#'
#' @param Hab_lay Habitat layer (sf) typically created in created by clean_vri_bem
#' @param landgdb Geodatabase pathway that contains the land ownerships.
#' @param landlayers names of the land ownship layers. This currently
#' defaults to the layers given in 2022
#'
#' @return
#' @export
#' @description
#' TO DO: import new tenures and land ownership boundaries
#'
#'
#' @examples
join_landTypes <- function(Hab_lay,
                          landgdb="D:/Github/Moose/SSAF_habitat_site_selection/data/SSAF_SiteSelection.gdb",
                          landlayers =c("land_ownership_icf","parks_ssaf_211119","tenures_ssaf_211125",
                                        "woodlot_ComFor_ssaf_211126","tfl_ssaf_211126"),
                          largest = TRUE){
  land_temp <- map(landlayers,~ st_read(dsn=landgdb,layer =.x) %>%
                     st_zm()%>%
                     st_make_valid())
  names(land_temp) <- landlayers

  #Hab_lay_sm <- Hab_lay[1:2000,]

  #join with ownerships
  Hab_lay_l <- land_temp$land_ownership_icf %>%
      dplyr::select(WNRSHPCLSS)%>%
      st_join(Hab_lay,., left = TRUE, largest = largest)

  #join with tenures
  Hab_lay_l <- land_temp$tenures_ssaf_211125 %>%
    dplyr::select(TENURE_TYPE)%>%
    st_join(Hab_lay_l,., left=TRUE, largest = largest)

  #join with parks and protected areas
  Hab_lay_l <- land_temp$parks_ssaf_211119 %>%
    dplyr::select(PROTECTED_LANDS_NAME)%>%
    st_join(Hab_lay_l,., left=TRUE, largest = largest)

  #join with tfls
  Hab_lay_l <- land_temp$woodlot_ComFor_ssaf_211126 %>%
    dplyr::select(CLNTNM,LFCCLSTTSC)%>%
    st_join(Hab_lay_l,., left=TRUE, largest = largest)

  setnames(Hab_lay_l, old = c("WNRSHPCLSS","PROTECTED_LANDS_NAME","CLNTNM","LFCCLSTTSC"),
           new = c("OWNER_TYPE","PARKS","WOODLOT_OWNER","WOODLOT_ACTIVE"))

  return(Hab_lay_l)
}



#' Join PEM with habitat layer
#'
#' @param Hab_lay
#' @param gdbpath
#' @param layername description
#' @return
#' @export
#'
#' @details
#' This function joins the pem data to the cleaned vri-bem habitat layer. The pem layer has complex
#' and different delineation from the vri-bem layer. As a result, we could cut the vri-bem polygons that overlap
#' multiple predicted site series. However that would create new poylgons for validation, and because the
#' objective is to validate the already created vri-bem model, modifying the polygons for site series does
#' not make sense. We can  assign all site series values that overlap the vri-bem polygon (largest=FALSE),
#' but not provide any spatial information about where within the polygon those site series occur,
#' or we can only include the predicted site series for the largest overlapping area (largest = TRUE).
#' Currently the default is set to largest = TRUE, which means only the most prominent site series is included
#' but this also comes at a computational cost.
#'
#' @description
#' This data comes as a shapefile, so no area of interest is required to define
#'
#'
#'
#' @examples
join_pem <- function(Hab_lay,gdbpath, layername, largest = TRUE, wbpflag=TRUE){

  pem <- read_pem(gdbpath, layername)

  pem_ss <- dom_siteseries(pem, wbpflag=TRUE)

  pem_ss <- pem_ss %>%
    dplyr::select(.,-"BGC_ZONE")

  Hab_lay_p <- st_join(Hab_lay,pem_ss, left=TRUE, largest=largest)

  Hab_lay_p <- st_make_valid(Hab_lay_p)
  #Hab_lay_p %>% dplyr::filter(TEIS_ID=="12006")#%>%
    #st_write(.,"hab_p.gpkg",append=FALSE)%>%
   # ggplot() +
    #geom_sf() +
    #theme_minimal()
  return(Hab_lay_p)

}



#' Join habitat layer with cutblocks
#'
#' @param Hab_lay
#' @param aoi
#' @param gdbpath
#' @param layername
#'
#' @return
#' @export
#'
#' @examples
join_ccb <- function(Hab_lay, aoi, gdbpath=NULL, layername=NULL){

  ccb <- read_cutblocks(aoi)

  ccb <- ccb %>%
    dplyr::select(.,c("OPENING_ID","HARVEST_YEAR"))

  Hab_lay_ccb <- st_join(Hab_lay, ccb, left=TRUE,largest=TRUE)

  return(Hab_lay_ccb)

}


#' Title
#'
#' @param Hab_lay
#' @param huck_path
#' @param huck_layername
#'
#' @return
#' @export
#'
#' @importFrom terra extract
#'
#' @examples
join_huckleberry <- function(Hab_lay, huck_path, huck_layername, poly_function = "mean"){

  huck_layer <- read_huckleberry(huck_path, huck_layername)

  #huck_layer <- project(huck_layer, crs(Hab_lay))

  Hab_lay_h <- terra::extract(huck_layer, Hab_lay,
                              fun = poly_function, bind=TRUE)

  Hab_lay_h <- st_as_sf(Hab_lay_h)
  Hab_lay_h <- rename(Hab_lay_h,
                      "PREDICTED_HUCKLEBERRY" = "HuckleberryDistribution2022_4classes-003_NAD83")

  Hab_lay_h <- Hab_lay_h %>%
    mutate(PREDICTED_HUCKLEBERRY = na_if(PREDICTED_HUCKLEBERRY, NaN))

  return(Hab_lay_h)

}










