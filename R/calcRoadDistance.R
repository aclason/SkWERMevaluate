


#' Calculate distance to nearest road
#'
#' @param aoi #right now, only clipped for bcdata download road layer
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
calc_rd_dist <- function(aoi, road_path, road_layer, Hab_lay){

  if(is.null(road_path)){
    rds <- get_roads(aoi = aoi)

    rds <- rds %>%
      dplyr::select(ROAD_NAME_FULL,ROAD_CLASS,ROAD_SURFACE) %>%
      st_make_valid(.)

    rds <- st_transform(rds, crs = crs(Hab_lay))

  }else{
    rds <- st_read(file.path(road_path, road_layer))

    rds <- rds %>%
      dplyr::select(OBJECTID,DRA_ROAD_C,DRA_ROAD_1) %>%
      st_make_valid(.)
  }
  hab_edge <- st_boundary(Hab_lay)

  #rd_d <- st_nearest_feature(Hab_lay,rds) #could maybe speed this up by clipping to AOI
  rd_d <- st_nearest_feature(hab_edge,rds)
  #calculate the distance to the nearest road segment
  rd_dist <- st_distance(Hab_lay,rds[rd_d,], by_element = TRUE)

  #put road distances into kms and drop the unit
  rd_dist2 <- unclass(rd_dist/1000)

  Hab_lay_r <- Hab_lay %>%
    mutate(Road_dist = rd_dist2, .before = geometry)

  return(Hab_lay_r)

}
