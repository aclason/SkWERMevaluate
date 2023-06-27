


#' Calculate distance to nearest road
#'
#' @param aoi
#' @param Hab_lay
#'
#' @return
#' @export
#'
#' @details
#' Currently, any road. TO DO: add an argument to select road surface type
#'
#'
#' @examples
calc_rd_dist <- function(aoi = "Lakes TSA", Hab_lay){

  rds <- get_roads(aoi = aoi)

  rds <- rds %>%
    dplyr::select(ROAD_NAME_FULL,ROAD_CLASS,ROAD_SURFACE) %>%
    st_make_valid(.)

  rds <- st_transform(rds, crs = crs(Hab_lay))

  rd_d <- st_nearest_feature(Hab_lay,rds)

  #calculate the distance to the nearest road segment
  rd_dist <- st_distance(Hab_lay,rds[rd_d,], by_element = TRUE)

  #put road distances into kms and drop the unit
  rd_dist2 <- unclass(rd_dist/1000)

  Hab_lay_r <- Hab_lay %>%
    mutate(Road_dist = rd_dist2, .before = geometry)

  return(Hab_lay_r)

}
