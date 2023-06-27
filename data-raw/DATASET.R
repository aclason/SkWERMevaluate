## Add the output description and cleaning data here

ModelCodes <- fread("D:/Sync/BVRC/FSD program/SkWERM/SkWERMevaluate/SkWERM_output_descriptions.csv")

usethis::use_data(ModelCodes,overwrite = TRUE)
