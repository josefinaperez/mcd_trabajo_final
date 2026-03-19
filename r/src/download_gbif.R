# install.packages("rgbif")
# install.packages("dplyr")
# install.packages("readr")

library(rgbif)
library(dplyr)
library(readr)

download_gbif_fungi_species <- function(
    species_name,
    country_code = "AR",
    max_records = 20000,
    batch_size = 300,
    save_path = NULL,
    has_coordinate = TRUE
) {
  stopifnot(is.character(species_name), length(species_name) == 1)
  
  message("Searching GBIF for: ", species_name, " | country: ", country_code)
  
  offset <- 0
  all_results <- list()
  
  repeat {
    n_to_get <- min(batch_size, max_records - offset)
    if (n_to_get <= 0) break
    
    res <- rgbif::occ_search(
      scientificName = species_name,
      country = country_code,
      #kingdom = "Fungi",
      hasCoordinate = has_coordinate,
      limit = n_to_get,
      start = offset
    )
    
    dat <- res$data
    
    if (is.null(dat) || nrow(dat) == 0) {
      break
    }
    
    all_results[[length(all_results) + 1]] <- dat
    offset <- offset + nrow(dat)
    
    message("Downloaded ", offset, " records...")
    
    if (nrow(dat) < n_to_get) {
      break
    }
  }
  
  if (length(all_results) == 0) {
    warning("No records found.")
    return(data.frame())
  }
  
  out <- bind_rows(all_results) %>%
    distinct(key, .keep_all = TRUE)
  
  if (!is.null(save_path)) {
    readr::write_csv(out, save_path)
    message("Saved to: ", save_path)
  }
  
  out
}

download_gbif_fungi_species_batch <- function(
    species_list,
    country_code = "AR",
    max_records = 10000,
    out_dir = "data/ocurrences/raw",
    has_coordinate = TRUE
) {
  stopifnot(is.character(species_list), length(species_list) > 0)
  
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  results <- vector("list", length(species_list))
  names(results) <- species_list
  
  for (sp in species_list) {
    safe_name <- gsub("[^a-z0-9]+", "_", tolower(sp))
    safe_name <- gsub("^_|_$", "", safe_name)
    
    save_path <- file.path(out_dir, paste0("df_", safe_name, ".csv"))
    
    message("Downloading species: ", sp)
    
    df <- download_gbif_fungi_species(
      species_name = sp,
      country_code = country_code,
      max_records = max_records,
      save_path = save_path,
      has_coordinate = has_coordinate
    )
    
    results[[sp]] <- df
  }
  
  invisible(results)
}
