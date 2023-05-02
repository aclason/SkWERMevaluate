## Add the output description and cleaning data here

ModelCodes <- fread("D:/Sync/BVRC/FSD program/SkWERM/SSAF_SiteSelection/SkWERM_output_descriptions.csv")

usethis::use_data(ModelCodes,overwrite = TRUE)
