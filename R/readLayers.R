
#---------------------- Load libraries---------------------------------
ls <- c("bcdata")
ls <- c("tidyverse", "data.table") # Data Management and Manipulation
ls <- append(ls, c("terra","sf")) # geo comp.
# Install if needed -- then load.
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)

#---------------------- TSA boundaries ---------------------------------
#get the TSA boundaries:
bcdc_get_record("fadm-timber-supply-area-tsa")

bcdc_query_geodata("8daa29da-d7f4-401c-83ae-d962e3a28980")

bcdc_describe_feature('8daa29da-d7f4-401c-83ae-d962e3a28980')%>%
  print(.,n=19)

#get the lakes TSA to collect other layers:
ltsa <- bcdc_query_geodata("8daa29da-d7f4-401c-83ae-d962e3a28980") %>%
  filter(TSA_NUMBER_DESCRIPTION=="Lakes TSA") %>%
  collect()

ltsa <- ltsa %>%
  dplyr::filter(FEATURE_ID==321)

ltsa %>%
  ggplot() +
  geom_sf() +
  theme_minimal()
st_write(ltsa,"./Inputs/ltsa.gpkg")



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




#-------------------------- cutblocks ---------------------------------
ccb <- bcdc_query_geodata("b1b647a6-f271-42e0-9cd0-89ec24bce9f7") %>%
  filter(WITHIN(ltsa)) %>% #filter for records that are within the DPG boundary
  collect() #collect/download the data
ccb <- st_make_valid(ccb)
st_write(ccb, "./Inputs/ccb.gpkg", append=FALSE)


#-------------------------- Roads ---------------------------------
rds <- bcdc_query_geodata("b1b647a6-f271-42e0-9cd0-89ec24bce9f7") %>%
  filter(INTERSECTS(ltsa)) %>% #filter for records that are within the DPG boundary
  collect() #collect/download the data


##ignore:

st_write(ccb, "./Inputs/ccb.gpkg")



bcdc_search("roads")

















bcdc_get_record("results-forest-cover-silviculture")

bcdc_query_geodata('258bb088-4113-47b1-b568-ce20bd64e3e3')


dpg <- bcdc_query_geodata("natural-resource-nr-district") %>%
  filter(ORG_UNIT=="DPG") %>% # filter for Prince George Natural Resource District
  collect()

dpg %>%
  ggplot() +
  geom_sf() +
  theme_minimal()

bcdc_describe_feature('258bb088-4113-47b1-b568-ce20bd64e3e3')%>%
  print(.,n=160)

## Activity treatments
bcdc_get_record("results-activity-treatment-units")
('07cabbdf-d7bf-4c50-919d-5b7d80086ef5')

bcdc_query_geodata('07cabbdf-d7bf-4c50-919d-5b7d80086ef5')

bcdc_describe_feature('07cabbdf-d7bf-4c50-919d-5b7d80086ef5')%>%
  print(.,n=52)

nadBnd <- st_read("D:/Sync/BVRC/Fire program/Maps/NadinaBndry.shp")
ccb_nadinaBnd %>%
  ggplot() +
  geom_sf() +
  theme_minimal()

#get results for Nadina:
res_nadinaBnd <-
  bcdc_query_geodata("07cabbdf-d7bf-4c50-919d-5b7d80086ef5") %>%
  filter(INTERSECTS(nadBnd)) %>% #filter for records that are within the DPG boundary
  collect() #collect/download the data

st_write(res_nadinaBnd,"D:/Sync/BVRC/Fire program/Maps/Nad_res.gpkg" )

res_ndt <- as.data.table(res_nadinaBnd)
unique(res_ndt$SILV_BASE_CODE)
unique(res_ndt$SILV_METHOD_CODE)
hist(res_ndt$ACTUAL_TREATMENT_AREA)

length(unique(res_ndt[SILV_BASE_CODE=="SP"]$OPENING_ID))
res_ndt[SILV_BASE_CODE=="LB"]
st_write(st_as_sf(res_ndt[ACTUAL_TREATMENT_AREA < 600]),
         "D:/Sync/BVRC/Fire program/Maps/Nad_res.gpkg",append=FALSE)

#what about consolidated cutblocks?
bcdc_get_record("b1b647a6-f271-42e0-9cd0-89ec24bce9f7")

ccb_nadinaBnd <-
  bcdc_query_geodata("b1b647a6-f271-42e0-9cd0-89ec24bce9f7") %>%
  filter(INTERSECTS(nadBnd)) %>% #filter for records that are within the DPG boundary
  collect() #collect/download the data

st_write(ccb_nadinaBnd,
         "D:/Sync/BVRC/Fire program/Maps/Nad_ccb.gpkg",append=FALSE)
