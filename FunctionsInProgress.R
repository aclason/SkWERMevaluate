

bcdata::bcdc_describe_feature("3544ad91-0cf2-4926-a08a-bfe42d9a031d")%>%
  print(.,n=20)
tnad <- bcdc_query_geodata("3544ad91-0cf2-4926-a08a-bfe42d9a031d") %>%
  filter(INTERSECTS(nadBnd))

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
