library(dplyr)
library(stringr)
library(aws.s3)
library(purrr)

# A function to put data from local machine to S3
put_to_s3 <- function(from, to) {
  aws.s3::put_object(
    file = from,
    object = to,
    bucket = "fbedecarrats",
    region = "",
    multipart = TRUE)
}

# A function to iterate/vectorize copy
get_from_s3 <- function(from, to) {
  aws.s3::save_object(
    object = from,
    bucket = "projet-betsaka",
    file = to,
    overwrite = FALSE,
    region = "")
}

resources <- list(
  "accessibility_2000",
  "gfw_lossyear",
  "gfw_treecover",
  "nasa_srtm",
  "worldclim_max_temperature",
  "worldclim_min_temperature",
  "worldclim_precipitation",
  "worldpop"
)

get_from_s3_batch <- function(resource) {
 # resource <- "w"
  print(resource)
  root_local <- "data/raw/mapme/"
  root_s3 <- "diffusion/PA-impact-on-deforestation/mapme/"
  
  dir_local <- paste0(root_local, resource)
  dir_s3 <- paste0(root_s3, resource)
  
  dir.create(dir_local)
  
  my_files_s3 <- get_bucket_df(bucket = "projet-betsaka",
                               prefix = dir_s3,
                               region = "",
                               max = Inf) %>%
    pluck("Key")
  
  
  # To put files
  my_files_local <- list.files(dir_local, full.names = TRUE, recursive = TRUE)
  
  local_compare <- str_replace(my_files_local,root_local, root_s3)
  
  add_files <- my_files_s3[!my_files_s3 %in% local_compare]
  
  my_files_dest <- str_replace(add_files, root_s3, root_local)
  
  map2(add_files, my_files_dest, get_from_s3)
}

map(resources, get_from_s3_batch)

get_from_s3_batch("worldpop")


# A function to iterate/vectorize deletion
delete_on_s3 <- function(obj, to) {
  aws.s3::delete_object(
    object = obj,
    bucket = "projet-betsaka",
    region = "")
}


batch_delete_s3 <- function(resource, pattern) {
  root_s3 <- "diffusion/PA-impact-on-deforestation/mapme/"
  #resource <- "worldclim_precipitation"
  #pattern = "wc2\\.1_2\\.5"
  dir_s3 <- paste0(root_s3, resource)
  my_files_s3 <- get_bucket_df(bucket = "projet-betsaka",
                               prefix = root_s3,
                               region = "",
                               max = Inf) %>%
    pluck("Key")
  to_delete <- my_files_s3[str_detect(my_files_s3, pattern)]
  map(to_delete, delete_on_s3)
}
batch_delete_s3(resource = "worldclim_min_temperature",
                pattern = "wc2\\.1_2\\.5")
