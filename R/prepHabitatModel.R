
#' Prep the vri-bem object
#'
#' @param gdbpath pathway to geodatabase containing VRI-BEM layer
#' @param layername name of the VRI-BEM layer
#' @param minsize minimum area for a polygon (m2) to be included for site assessment
#' @param minhabscore maximum habitat score (numeric 1 - 5) to be included for site assessment
#' @param sec_beu %(0-100) of a secondary BEU class of a polygon allowed. Default = 0% (i.e.
#' no complex polygons selected for validation)
#' @param incl_hibernation
#'
#' @import data.table
#'
#' @return
#' @export
#' @description
#' This function imports the habitat layer (or calls it from memory), removes unnecessary columns
#' re-arranges rows to represent primary, secondary and tertiary BEU if multiple BEU classes/polygon
#' are allowed and re-organizes the data table to create columns for field assessment of the habitat model
#'
#' @details
#' The minsize parameter is included to remove right off the start numerous "sliver" polygons that are
#' created during the VRI-BEM combination process. Polygons smaller than this are meaningless, with a
#' default value of 10000 m2 (1 ha).
#'
#' murar = bear, mulal = moose
#' minhabscore=2, sec_beu = 0, incl_hibernation = FALSE
#'
#' @examples
prep_vri_bem <- function(vri_bem_path = NA, hab_layername, minsize=10000, inputPath=NA){

  if(is.na(vri_bem_path)){
    Hab_lay <- read_vri_bem(inputPath = inputPath, layername=hab_layername)
  }else{

    Hab_lay <- read_vri_bem(vri_bem_path = vri_bem_path, layername = hab_layername,
                            inputPath=NA)
  }



  #remove slivers
  #Hab_layDT <- Hab_layDT[Shape_Area >= minsize]
  Hab_lay <- Hab_lay %>% dplyr::filter(.,Shape_Area >= minsize)

  Hab_layDT <- as.data.table(Hab_lay)

  #which columns to keep - use ModelCodes data to thin
  colKeep <- ModelCodes[Keep_Remove=="Keep"]$FieldName
  #in 2024 - changed to CROWN_ALL:
  colKeep <- colKeep[!colKeep %in% c("CROWN_BEAR_1","CROWN_BEAR_2","CROWN_BEAR_3",
                                     "CROWN_MOOSE_1","CROWN_MOOSE_2","CROWN_MOOSE_3")]
  colKeep <- c(colKeep, "CROWN_ALL_1", "CROWN_ALL_2", "CROWN_ALL_3")
  #adding polyid from 2023 just to cross check
  colKeep <- c(colKeep, "PolyID")

  if(length(setdiff(colKeep,colnames(Hab_layDT)))>0){
    print("WARNING - Check which columns to keep in VRI-BEM")
    #colKeep2 <- colKeep[ !colKeep %in% c("Shape","CR_CLOSURE","COV_PCT_1","PROJ_AGE_1")]
    #colKeep <- c(colKeep2, "geom","CROWN_CLOSURE")
  }
  Hab_layDT <- Hab_layDT[,..colKeep]

  #1. add ID field----------------------------------------------------------------------------
  setnames(Hab_layDT, "PolyID", "PolyID_23")
  Hab_layDT[,PolyID := paste(FEATURE_ID,"_",TEIS_ID,sep = "")]

  #find duplicates - there can be a bunch of reasons for duplicate values - most seem like edge cases
  #e.g. on the outer boundary
  setDT(Hab_layDT)[, dupID := rowid(PolyID)]

  #update polyid by dup value
  Hab_layDT[,PolyID := paste(PolyID,"_",dupID,sep = "")][, dupID := NULL]

  setcolorder(Hab_layDT,c("PolyID","TEIS_ID","FEATURE_ID"))

  #2. rename the first BEU related columns ------------------------------------------------------

  #we used to remove the other deciles (only simple polygons allowed), but we will now pass that
  #choice to the user - we will base everything on the first BEU call (dominant) but a user could
  # include complex polys in their selection

  BEU_1_names <- c("SDEC_1","BEUMC_S1","REALM_1","GROUP_1","CLASS_1","KIND_1","STRCT_S1", "STAND_A1",
                   "FORESTED_1","CROWN_ALL_1",
                   "MALAN_WFD_6C_SU_1", "MALAN_GFD_6C_SU_1", "MALAN_WST_6C_SU_1", "MALAN_WFD_6C_CAP_1",
                   "MALAN_GFD_6C_CAP_1", "MALAN_WST_6C_CAP_1",
                   "MURAR_PEFD_6C_SU_1", "MURAR_PLFD_6C_SU_1", "MURAR_SFD_6C_SU_1", "MURAR_FFD_6C_SU_1",
                   "MURAR_HI_6C_SU_1", "MURAR_PEFD_6C_CAP_1", "MURAR_PLFD_6C_CAP_1", "MURAR_SFD_6C_CAP_1",
                   "MURAR_FFD_6C_CAP_1", "MURAR_HI_6C_CAP_1")

  newBEUcolnames <- gsub("_1","",BEU_1_names)
  newBEUcolnames <- gsub("_S1","", newBEUcolnames)
  newBEUcolnames <- gsub("_A1","",newBEUcolnames)

  setnames(Hab_layDT, BEU_1_names, newBEUcolnames)

  #3. Add a column that combined BEC call (from VRI)--------------------------------------
  Hab_layDT[,BGC_LABEL := paste0(BGC_ZONE,BGC_SUBZON,BGC_VRT)]


  #4. append min habitat scores -----------------------------------------------------------

  #i guess to pass the decision to the user, I'll add one col where hibernation is incl and one
  #where it isn't to calculate min hab score

  # no hibernation
  bear_hab_val_cols <- c("PolyID","MURAR_PEFD_6C_SU_WA", "MURAR_PLFD_6C_SU_WA",
                         "MURAR_SFD_6C_SU_WA","MURAR_FFD_6C_SU_WA")

  bear_hab_layDT_scores <- Hab_layDT[,..bear_hab_val_cols]
  bear_hab_layDT_scores[, murar_hab_min := min(.SD, na.rm = TRUE),
                        .SDcols = bear_hab_val_cols[!bear_hab_val_cols == 'PolyID'], by ="PolyID"]
  bear_hab_layDT_scores[murar_hab_min=="Inf", murar_hab_min:=NA]

  # hibernation
  bear_hab_val_cols_hib <- c("PolyID","MURAR_PEFD_6C_SU_WA", "MURAR_PLFD_6C_SU_WA",
                             "MURAR_SFD_6C_SU_WA","MURAR_FFD_6C_SU_WA",
                             "MURAR_HI_6C_SU_WA")

  bear_hab_layDT_scores_hib <- Hab_layDT[,..bear_hab_val_cols_hib]
  bear_hab_layDT_scores_hib[, murar_hab_min_hib := min(.SD, na.rm = TRUE),
                        .SDcols = bear_hab_val_cols_hib[!bear_hab_val_cols_hib == 'PolyID'],
                        by ="PolyID"]
  bear_hab_layDT_scores_hib[murar_hab_min_hib =="Inf", murar_hab_min_hib := NA]

  ### moose
  moose_hab_val_cols <- c("PolyID","MALAN_WFD_6C_SU_WA", "MALAN_GFD_6C_SU_WA",
                          "MALAN_WST_6C_SU_WA")

  #only include polys with at least one col of min habitat score
  moose_hab_layDT_scores <- Hab_layDT[,..moose_hab_val_cols]
  moose_hab_layDT_scores[, malan_hab_min := min(.SD, na.rm = TRUE),
                         .SDcols = moose_hab_val_cols[!moose_hab_val_cols == 'PolyID'], by ="PolyID"]
  moose_hab_layDT_scores[malan_hab_min=="Inf", malan_hab_min:=NA]


  #merge min moose and min bears together
  bear_bear_hab_score <- merge(bear_hab_layDT_scores[,.(PolyID,murar_hab_min)],
                               bear_hab_layDT_scores_hib[,.(PolyID,murar_hab_min_hib)],
                                by="PolyID", all=TRUE)
  bear_moose_hab_score <- merge(bear_bear_hab_score[,.(PolyID,murar_hab_min,murar_hab_min_hib)],
                                moose_hab_layDT_scores[,.(PolyID,malan_hab_min)],
                                by="PolyID", all=TRUE)
  #if multiple BEU, there would be unnecessary duplicates - not sure if this is true
  #bear_moose_hab_score <- unique(bear_moose_hab_score)

  Hab_layDT <- merge(Hab_layDT,bear_moose_hab_score,  by="PolyID", all.y=TRUE)


  #5. add new columns to represent SST ------------------------------------------------------------
  Hab_layDT[,`:=`(MALAN_SST_6C_SU_WA = NA, MURAR_SST_6C_SU_WA = NA)]


  #6. re-order and keep only needed columns -----------------------------------------------------
  #might not do this, to allow more decisions passed to user in app
  #colKeep <- c("PolyID","PolyID_23","TEIS_ID", "BCLCS_LV_2", "BCLCS_LV_3", "BGC_LABEL",
   #            "SOIL_MOISTURE_REGIME_1","SOIL_NUTRIENT_REGIME",
    #           "SDEC","BEUMC", "REALM","STAND", "STRCT","CROWN_ALL",
     #          "SPEC_CD_1","SPEC_PCT_1", "SPEC_CD_2",
      #         "SPEC_PCT_2", "SPEC_CD_3", "SPEC_PCT_3", "SPEC_CD_4", "SPEC_PCT_4",
       #        "SPEC_CD_5","SPEC_PCT_5", "SPEC_CD_6", "SPEC_PCT_6", "MEAN_ASP",
        #       "MEAN_SLOPE", "ELEV",
        #       "SITE_M3A",
         #      "MALAN_WFD_6C_SU_WA", "MALAN_GFD_6C_SU_WA",
          #     "MALAN_WST_6C_SU_WA", "MALAN_SST_6C_SU_WA", "MURAR_PEFD_6C_SU_WA",
           #    "MURAR_PLFD_6C_SU_WA", "MURAR_SFD_6C_SU_WA", "MURAR_FFD_6C_SU_WA",
            #   "MURAR_HI_6C_SU_WA", "MURAR_SST_6C_SU_WA",
             #  "murar_hab_min", "malan_hab_min","Salmon",
              # "Shape_Area","Shape")

  #Hab_layDT <- Hab_layDT[,..colKeep]

  #7. make shapefile ---------------------------------------------------------------------------
  Hab_lay <- st_as_sf(Hab_layDT)
  Hab_lay <- st_make_valid(Hab_lay)

  return(Hab_lay)
}













