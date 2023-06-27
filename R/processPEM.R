#' Read and process PEM layer
#'
#' @param gdbpath
#' @param layername
#'
#' @return
#' @export
#'
#' @details
#' This function reads the pem layer. Currently we assume that it's stored locally.
#' An option is to apply the pem_wbp function to flag whether the pem polygon may contain whitebark pine
#'
#'
#' @examples
read_pem <- function(gdbpath, layername){

  pem <- st_read(dsn = gdbpath, layer = layername)

  return(pem)
}



#' Select the dominant site series to represent a PEM polygon
#'
#' @param pem the pem object (from read_pem)
#' @param wbpflag
#'
#' @return
#' @export
#'
#' @details
#' TO DO: fix import from or attach data.table to the package
#'
#'
#' @examples
dom_siteseries <- function(pem, wbpflag=TRUE){

  #Determine which is the dominant siteseries
  pem_s <- pem %>%
    dplyr::select(PROJPOLYID,BGC_ZONE,BGC_SUBZON,BGC_UNIT, SDEC_1,SITE_S1,
                  SDEC_2,SITE_S2,SDEC_3,SITE_S3)
  pem_s_dt <- as.data.table(pem_s, na.strings = "NA")

  #it can happen that the second ss has a larger decile than the primary
  #there are also polygons with no ss assignment
  dec <- melt(pem_s_dt, id.vars="PROJPOLYID",
              measure.vars = c("SDEC_1","SDEC_2","SDEC_3"),
              variable.name = "var_dec",
              value.name = "dec")
  site <- melt(pem_s_dt, id.vars="PROJPOLYID",
               measure.vars = c("SITE_S1","SITE_S2","SITE_S3"),
               variable.name = "var_site",
               value.name = "site")

  dec_site <- dec[,.(PROJPOLYID,var_dec, dec,
                     var_site = site[,var_site], ss=site[,site])]
  dec_site_m <- setDT(dec_site)[, .SD[which.max(dec)], by=PROJPOLYID] #NAs removed

  pem_s_dt_m <- merge(pem_s_dt, dec_site_m, by="PROJPOLYID", all=TRUE)
  pem_s_s <- st_as_sf(pem_s_dt_m[,.(PROJPOLYID,BGC_ZONE,BGC_SUBZON,BGC_UNIT,ss,Shape)])

  pem_s_s <- st_make_valid(pem_s_s)
  #add whitebark pine flag
  if(wbpflag==TRUE){

    pem_s_s <- pem_s_s %>%
      mutate(.,WBP_pem = pem_wbp(BGC_ZONE,BGC_SUBZON,ss))

  }

  #pem_s_s %>% dplyr::filter(.,!is.na(WBP_pem))
  rm(pem_s,pem_s_dt,pem_s_dt_m)
  gc()

  return(pem_s_s)

}
