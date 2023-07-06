
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
#'
#' @examples
prep_vri_bem <- function(gdbpath, hab_layername, minsize=10000,
                         minhabscore=2, sec_beu = 0, incl_hibernation = FALSE){
  Hab_lay <- read_vri_bem(gdbpath = gdbpath, layername=hab_layername)

  #remove slivers
  #Hab_layDT <- Hab_layDT[Shape_Area >= minsize]
  Hab_lay <- Hab_lay %>% dplyr::filter(.,Shape_Area >= minsize)

  Hab_layDT <- as.data.table(Hab_lay)

  #which columns to keep - use ModelCodes data to thin
  colKeep <- ModelCodes[Keep_Remove=="Keep"]$FieldName

  Hab_layDT <- Hab_layDT[,..colKeep]

  #1. add ID field----------------------------------------------------------------------------

  Hab_layDT[,PolyID := paste(FEATURE_ID,"_",TEIS_ID,sep = "")]

  #find duplicates - there can be a bunch of reasons for duplicate values - most seem like edge cases
  #e.g. on the outer boundary
  setDT(Hab_layDT)[, dupID := rowid(PolyID)]

  #update polyid by dup value
  Hab_layDT[,PolyID := paste(PolyID,"_",dupID,sep = "")][, dupID := NULL]

  setcolorder(Hab_layDT,c("PolyID","TEIS_ID","FEATURE_ID"))

  #does beu sdec always sum to 10?
  #Hab_layDT[,sum(.SD), .SDcols=c("SDEC_1","SDEC_2","SDEC_3"),by="PolyID"]

  #2. split out the 3 levels of BEU ecosystems and the remaining variables--------------------

  # Because of how the columns were labelled, the easiest way is to split the data into chunks
  # (BEU 1, 2 and 3), bind those together and then merge back with the main dataset based on id

  s1_dec_val <- 10 - sec_beu/10
  #only keep the types of polygons specified (complex or simple?)
  Hab_layDT <- Hab_layDT[SDEC_1 >= s1_dec_val]

  if(s1_dec_val < 10){
    print(paste("polygons with <", sec_beu,"% secondary BEU classes (complex) have been selected"))


    BEU_1_names <- c("PolyID","SDEC_1","BEUMC_S1","REALM_1","GROUP_1","CLASS_1","KIND_1","STRCT_S1", "STAND_A1",
                     "FORESTED_1","CROWN_BEAR_1","CROWN_MOOSE_1",
                     "MALAN_WFD_6C_SU_1", "MALAN_GFD_6C_SU_1", "MALAN_WST_6C_SU_1", "MALAN_WFD_6C_CAP_1",
                     "MALAN_GFD_6C_CAP_1", "MALAN_WST_6C_CAP_1",
                     "MURAR_PEFD_6C_SU_1", "MURAR_PLFD_6C_SU_1", "MURAR_SFD_6C_SU_1", "MURAR_FFD_6C_SU_1",
                     "MURAR_HI_6C_SU_1", "MURAR_PEFD_6C_CAP_1", "MURAR_PLFD_6C_CAP_1", "MURAR_SFD_6C_CAP_1",
                     "MURAR_FFD_6C_CAP_1", "MURAR_HI_6C_CAP_1")
    BEU_2_names <- gsub("_1","_2",BEU_1_names)
    BEU_2_names <- gsub("1","2",BEU_2_names)
    BEU_3_names <- gsub("_1","_3",BEU_1_names)
    BEU_3_names <- gsub("1","3",BEU_3_names)

    Hab_layDTl<- list()
    #trying to make sure BEUMC_S1 !is.na for # 1
    Hab_layDTl[[1]] <- Hab_layDT[,..BEU_1_names][,BEU_Group:=1]
    Hab_layDTl[[2]] <- Hab_layDT[,..BEU_2_names][,BEU_Group:=2]
    #remove rows from # 2 that have no secondary designated BEM class or decile associated
    Hab_layDTl[[2]] <- Hab_layDTl[[2]][!is.na(BEUMC_S2) | SDEC_2 >0]

    Hab_layDTl[[3]] <- Hab_layDT[,..BEU_3_names][,BEU_Group:=3]
    #remove rows from # 3 that have no secondary designated BEM classor decile associated
    Hab_layDTl[[3]] <- Hab_layDTl[[3]][!is.na(BEUMC_S3) | SDEC_3 >0]

    #note - it's possible to have a secondary or tertiary ecosystem class identified, but with
    #no associated decile amount (i.e. the primary ecosystem is ranked 10)
    newBEUcolnames <- gsub("_1","",BEU_1_names)
    newBEUcolnames <- gsub("_S1","", newBEUcolnames)
    newBEUcolnames <- gsub("_A1","",newBEUcolnames)

    Hab_layDTl <- lapply(Hab_layDTl, setNames, c(newBEUcolnames,"BEU_Group"))
    Hab_layDTl_BEUmelt <- rbindlist(Hab_layDTl)
    setcolorder(Hab_layDTl_BEUmelt,c("PolyID","BEU_Group"))

    #get the base columns
    colnamesBase <- c("PolyID",colnames(Hab_layDT)[!colnames(Hab_layDT) %in%
                                                     c(BEU_1_names,BEU_2_names,BEU_3_names)])
    Hab_layDT_base <- Hab_layDT[,..colnamesBase]
    #merge back together and set column order
    Hab_layDT <- merge(Hab_layDT_base, Hab_layDTl_BEUmelt, by="PolyID")

  }else{
    print("only polygons with a single BEU class (simple) have been selected")

    BEU_1_names <- c("SDEC_1","BEUMC_S1","REALM_1","GROUP_1","CLASS_1","KIND_1","STRCT_S1", "STAND_A1",
                     "FORESTED_1","CROWN_BEAR_1","CROWN_MOOSE_1",
                     "MALAN_WFD_6C_SU_1", "MALAN_GFD_6C_SU_1", "MALAN_WST_6C_SU_1", "MALAN_WFD_6C_CAP_1",
                     "MALAN_GFD_6C_CAP_1", "MALAN_WST_6C_CAP_1",
                     "MURAR_PEFD_6C_SU_1", "MURAR_PLFD_6C_SU_1", "MURAR_SFD_6C_SU_1", "MURAR_FFD_6C_SU_1",
                     "MURAR_HI_6C_SU_1", "MURAR_PEFD_6C_CAP_1", "MURAR_PLFD_6C_CAP_1", "MURAR_SFD_6C_CAP_1",
                     "MURAR_FFD_6C_CAP_1", "MURAR_HI_6C_CAP_1")
    BEU_2_names <- gsub("_1","_2",BEU_1_names)
    BEU_2_names <- gsub("1","2",BEU_2_names)
    BEU_3_names <- gsub("_1","_3",BEU_1_names)
    BEU_3_names <- gsub("1","3",BEU_3_names)

    Hab_layDT[,c(BEU_2_names,BEU_3_names):=NULL]

    newBEUcolnames <- gsub("_1","",BEU_1_names)
    newBEUcolnames <- gsub("_S1","", newBEUcolnames)
    newBEUcolnames <- gsub("_A1","",newBEUcolnames)

    setnames(Hab_layDT, BEU_1_names, newBEUcolnames)
  }


  #3. Add a column that combined BEC call (from VRI)--------------------------------------
  Hab_layDT[,BGC_LABEL := paste0(BGC_ZONE,BGC_SUBZON,BGC_VRT)]

  #4. select min habitat scores -----------------------------------------------------------
  ### bears
  if(incl_hibernation == TRUE){
    bear_hab_val_cols <- c("PolyID","MURAR_PEFD_6C_SU_WA", "MURAR_PLFD_6C_SU_WA",
                           "MURAR_SFD_6C_SU_WA","MURAR_FFD_6C_SU_WA",
                           "MURAR_HI_6C_SU_WA")
  }else{
    bear_hab_val_cols <- c("PolyID","MURAR_PEFD_6C_SU_WA", "MURAR_PLFD_6C_SU_WA",
                           "MURAR_SFD_6C_SU_WA","MURAR_FFD_6C_SU_WA")
  }
  #only include polys with at least one col of min habitat score
  bear_hab_layDT_scores <- Hab_layDT[,..bear_hab_val_cols]
  bear_hab_layDT_scores[, murar_hab_min := min(.SD, na.rm = TRUE),
                   .SDcols = bear_hab_val_cols[!bear_hab_val_cols == 'PolyID'], by ="PolyID"]
  bear_hab_layDT_scores[murar_hab_min=="Inf", murar_hab_min:=NA]

  ### moose
  moose_hab_val_cols <- c("PolyID","MALAN_WFD_6C_SU_WA", "MALAN_GFD_6C_SU_WA",
                          "MALAN_WST_6C_SU_WA")
  #only include polys with at least one col of min habitat score
  moose_hab_layDT_scores <- Hab_layDT[,..moose_hab_val_cols]
  moose_hab_layDT_scores[, malan_hab_min := min(.SD, na.rm = TRUE),
                   .SDcols = moose_hab_val_cols[!moose_hab_val_cols == 'PolyID'], by ="PolyID"]
  moose_hab_layDT_scores[malan_hab_min=="Inf", malan_hab_min:=NA]


  #merge min moose and min bears together
  bear_moose_hab_score <- merge(bear_hab_layDT_scores[murar_hab_min <= minhabscore,
                                                      .(PolyID,murar_hab_min)],
                                moose_hab_layDT_scores[malan_hab_min <= minhabscore,
                                                       .(PolyID,malan_hab_min)],
                                by="PolyID", all=TRUE)
  #if multiple BEU, there would be unnecessary duplicates
  bear_moose_hab_score <- unique(bear_moose_hab_score)

  Hab_layDT <- merge(Hab_layDT,bear_moose_hab_score,  by="PolyID", all.y=TRUE)

  #5. add new columns to represent SST
  Hab_layDT[,`:=`(MALAN_SST_6C_SU_WA = NA, MURAR_SST_6C_SU_WA = NA)]


  #6. re-order and keep only needed columns -----------------------------------------------------
  colKeep <- c("PolyID","TEIS_ID", "BCLCS_LV_2", "BCLCS_LV_3", "BGC_LABEL",
               "SOIL_MOISTURE_REGIME_1","SOIL_NUTRIENT_REGIME",
               "SDEC","BEUMC", "REALM","STAND", "STRCT",
               "PROJ_AGE_1","CR_CLOSURE",
               "SPEC_CD_1","SPEC_PCT_1", "SPEC_CD_2",
               "SPEC_PCT_2", "SPEC_CD_3", "SPEC_PCT_3", "SPEC_CD_4", "SPEC_PCT_4",
               "SPEC_CD_5","SPEC_PCT_5", "SPEC_CD_6", "SPEC_PCT_6", "MEAN_ASP",
               "MEAN_SLOPE", "ELEV",
               "SITE_M3A",
               "MALAN_WFD_6C_SU_WA", "MALAN_GFD_6C_SU_WA",
               "MALAN_WST_6C_SU_WA", "MALAN_SST_6C_SU_WA", "MURAR_PEFD_6C_SU_WA",
               "MURAR_PLFD_6C_SU_WA", "MURAR_SFD_6C_SU_WA", "MURAR_FFD_6C_SU_WA",
               "MURAR_HI_6C_SU_WA", "MURAR_SST_6C_SU_WA",
               "murar_hab_min", "malan_hab_min","Salmon",
               "Shape_Area","Shape")

  Hab_layDT <- Hab_layDT[,..colKeep]

  Hab_lay <- st_as_sf(Hab_layDT)
  Hab_lay <- st_make_valid(Hab_lay)

  return(Hab_lay)
}













