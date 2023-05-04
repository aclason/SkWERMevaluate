
#' Title
#'
#' @param dsn
#' @param layer
#'
#' @return
#' @export
#' @description
#' This function imports the habitat layer (or calls it from memory), removes unnecessary columns
#' re-arranges the rows to allow 3 rows to represent primary, secondary and tertiary BEU
#' ecosystem calls and re-organizes the data table to create logical groupings for site selection
#'
#' @examples
clean_vri_bem <- function(dsn = "./Inputs/SkWERM_Lakes.gdb", layer = "Lakes_10Mar2023_forVerification"){
  Hab_lay <- read_vri_bem(dsn = dsn, layer=layer)
  Hab_layDT <- as.data.table(Hab_lay)

  #which columns to keep - use ModelCodes data to thin
  colKeep <- ModelCodes[Keep_Remove=="Keep"]$FieldName

  Hab_layDT <- Hab_layDT[,..colKeep]
  #st_write(st_as_sf(Hab_layDT[TEIS_ID==12606]),"teis12606.gpkg")

  #murar = grizzly bear
  #malal = moose

  # Because of how the columns were labelled, the easiest way is to split the data into chunks
  # (BEU 1, 2 and 3), bind those together and then merge back with the main dataset based on id

  #1. add ID field
  Hab_layDT[,PolyID := seq(1,nrow(Hab_layDT))]
  setcolorder(Hab_layDT,c("PolyID","TEIS_ID","FEATURE_ID"))

  #2. split out the 3 levels of BEU ecosystems and the remaining variables
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
  Hab_layDTl[[1]] <- Hab_layDT[,..BEU_1_names][,BEU_Group:=1]
  Hab_layDTl[[2]] <- Hab_layDT[,..BEU_2_names][,BEU_Group:=2]
  Hab_layDTl[[3]] <- Hab_layDT[,..BEU_3_names][,BEU_Group:=3]
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

  #re-order the columns to be logical
  colOrder <- c("PolyID", "TEIS_ID", "FEATURE_ID", "BCLCS_LV_1", "BCLCS_LV_2", "BCLCS_LV_3",
                "BCLCS_LV_4","BCLCS_LV_5", "LAND_CD_1","COV_PCT_1", "SOIL_MOISTURE_REGIME_1",
                "LBL_VEGCOV","ECO_SEC", "BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE",
                "SPEC_CD_1", "SPEC_PCT_1", "SPEC_CD_2","SPEC_PCT_2","SPEC_CD_3", "SPEC_PCT_3",
                "SPEC_CD_4", "SPEC_PCT_4", "SPEC_CD_5", "SPEC_PCT_5", "SPEC_CD_6", "SPEC_PCT_6",
                "PROJ_AGE_1","AGE_CL_STS", "AGE_CL_STD","VRI_AGE_CL_STS", "VRI_AGE_CL_STD",
                "HARVEST_YEAR",
                "SITE_M3A","ABOVE_ELEV","ELEV", "MEAN_SLOPE","MEAN_ASP","SLOPE_MOD",
                "Salmon",
                "CR_CLOSURE", "CROWN_BEAR","CROWN_MOOSE",
                "BEU_Group","SDEC","BEUMC","REALM","GROUP","CLASS","KIND","STRCT",
                "STAND","FORESTED","MALAN_WFD_6C_SU","MALAN_GFD_6C_SU","MALAN_WST_6C_SU",
                "MALAN_WFD_6C_CAP", "MALAN_GFD_6C_CAP", "MALAN_WST_6C_CAP", "MURAR_PEFD_6C_SU",
                "MURAR_PLFD_6C_SU", "MURAR_SFD_6C_SU","MURAR_FFD_6C_SU", "MURAR_HI_6C_SU",
                "MURAR_PEFD_6C_CAP", "MURAR_PLFD_6C_CAP", "MURAR_SFD_6C_CAP",
                "MURAR_FFD_6C_CAP", "MURAR_HI_6C_CAP",
                "MALAN_WFD_6C_SU_HV","MALAN_WFD_6C_SU_WA", "MALAN_GFD_6C_SU_HV", "MALAN_GFD_6C_SU_WA",
                "MALAN_WST_6C_SU_HV", "MALAN_WST_6C_SU_WA", "MALAN_WFD_6C_CAP_HV", "MALAN_WFD_6C_CAP_WA",
                "MALAN_GFD_6C_CAP_HV", "MALAN_GFD_6C_CAP_WA", "MALAN_WST_6C_CAP_HV",
                "MALAN_WST_6C_CAP_WA", "MURAR_PEFD_6C_SU_HV", "MURAR_PEFD_6C_SU_WA","MURAR_PLFD_6C_SU_HV",
                "MURAR_PLFD_6C_SU_WA","MURAR_SFD_6C_SU_HV", "MURAR_SFD_6C_SU_WA", "MURAR_FFD_6C_SU_HV",
                "MURAR_FFD_6C_SU_WA", "MURAR_HI_6C_SU_HV","MURAR_HI_6C_SU_WA", "MURAR_PEFD_6C_CAP_HV",
                "MURAR_PEFD_6C_CAP_WA", "MURAR_PLFD_6C_CAP_HV", "MURAR_PLFD_6C_CAP_WA",
                "MURAR_SFD_6C_CAP_HV","MURAR_SFD_6C_CAP_WA","MURAR_FFD_6C_CAP_HV","MURAR_FFD_6C_CAP_WA",
                "MURAR_HI_6C_CAP_HV", "MURAR_HI_6C_CAP_WA",
                "Shape")

  setcolorder(Hab_layDT,colOrder)

  return(Hab_layDT)
}













