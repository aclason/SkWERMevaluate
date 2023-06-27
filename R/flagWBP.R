


#' Identify potential whitebark pine from PEM
#'
#' @param Hab_lay
#'
#' @return
#' @export
#' @details
#' the Lakes TSA PEM does not identify site series in the mkp, so can't pull out which are more likely to
#' contain WBP from there. Can just call out the mkp in general? Please see Land Management Handbook 26
#' Supplement 2 for reference to the sites series in parkland ecosystems that may contain WBP (likely too
#' limited in the supplement)
#'
#' @examples
pem_wbp <- function(beczone, subzone, siteseries) {
    case_when(
      #subalpine ecosystems in Skeena that may house WBP:
      beczone == "ESSF" & subzone == "mk" & siteseries == "02" ~ "high",
      beczone == "ESSF" & subzone == "mk" & siteseries == "03" ~ "maybe",
      beczone == "ESSF" & subzone == "mk" & is.null(siteseries) ~ "possible",
      beczone == "ESSF" & subzone == "mk" & siteseries == "01" ~ "possible",
      beczone == "ESSF" & subzone == "mc" & siteseries == "02" ~ "maybe",
      beczone == "ESSF" & subzone == "mc" & siteseries == "03" ~ "maybe",
      beczone == "ESSF" & subzone == "mc" & is.null(siteseries) ~ "possible",
      beczone == "ESSF" & subzone == "mc" & siteseries == "01" ~ "unlikely",
      #parkland ecosystems in Skeena that may house WBP:
      beczone == "ESSF" & subzone == "mkp" ~ "likely",
      beczone == "ESSF" & subzone == "mcp" ~ "maybe",
      .default = NA
    )
}


#' Identify potential whitebark pine from predictive map
#'
#' @return
#' @export
#'
#' @details
#' Likely use Clayton Lambs and Clason et al maps here?
#'
#' @examples
pred_wbp <- function(){


}

#' Join with C.Lamb pred wbp
#'
#' @param Hab_lay
#' @param wbp_path
#' @param wbp_layername
#' @param poly_function
#'
#' @return
#' @export
#'
#' @examples
join_wbp <- function(Hab_lay, wbp_path, wbp_layername, poly_function ="mean"){

    wbp_layer <- read_wbp(wbp_path, wbp_layername)

    wbp_layer <- project(wbp_layer, crs(Hab_lay))

    Hab_lay_w <- terra::extract(wbp_layer, Hab_lay,
                                fun = poly_function, bind=TRUE)

    Hab_lay_w <- st_as_sf(Hab_lay_w)

    Hab_lay_w <- rename(Hab_lay_w,
                        "PREDICTED_WBP" = "PINUALB_occcov")

    return(Hab_lay_w)


  }
