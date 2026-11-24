# Library -----------------------------------------------------------------

# library
library(soilDB)
library(terra)
library(sf)
library(dplyr)
library(purrr)
library(magick)
library(tidyterra)
library(ggplot2)


# Load data -----------------------------------------------------

# read coiid-mukey corr data
ssurgo_coiids <- read.csv("C:/Users/Nathan.Roe/Documents/ES_maps/sda_results/sda_nasis_ecosite_corr.csv")

ssurgo_coiids$recwlupdated <- as.Date(ssurgo_coiids$recwlupdated)

# make no corr more explicit
ssurgo_coiids$ecositeid <- ifelse(ssurgo_coiids$ecositeid == "", "Not assigned", ssurgo_coiids$ecositeid)
ssurgo_coiids$ecositeid <- ifelse(is.na(ssurgo_coiids$ecositeid), "Not assigned", ssurgo_coiids$ecositeid)

ssurgo_coiids |> dplyr::filter(ecositeid != "Not assigned" & 
                                 recwlupdated == "") |> nrow()
ssurgo_coiids |> dplyr::filter(recwlupdated == "") |> nrow()

ssurgo_coiids

# CONUS gSSURGO 2026 30m grid
r_mukey <- rast("D:/FY2026_gSSURGO_mukey_grid/MURASTER_30m.tif")

# Timesteps ---------------------------------------------------------------

# make date format
ssurgo_coiids$corr_date <- as.Date(ssurgo_coiids$recwlupdated, format = "%m/%d/%Y")

# create different timesteps
start_date <- as.Date("2010-01-01")
end_date <- Sys.Date()

date_seq <- seq.Date(
  from = start_date,
  to = end_date,
  by = "4 months"
)

# step all comps missing ecosite correlation to date in future to avoid NA
ssurgo_coiids$corr_date[ssurgo_coiids$ecositeid == "Not assigned"] <- "2050-01-01"

# Correct missing data ----------------------------------------------------

# coiids with ecosite correlation but no recwlupdated were populated before
# the table was added and are being assigned the earliest date. 

ssurgo_coiids$corr_date[ssurgo_coiids$ecositeid != "Not assigned" & 
                          ssurgo_coiids$recwlupdated == ""] <- "2010-01-01"

ssurgo_coiids$corr_date[ssurgo_coiids$ecositeid != "Not assigned" & 
                          is.na(ssurgo_coiids$recwlupdated)] <- "2010-01-01"

# Eval correlation @ timesteps --------------------------------------------------------

# create df of unique mukeys to join back to
final_df <- ssurgo_coiids |> 
  distinct(mukey)

# creates new col of correlation percent for each timestep
for(cutoff in date_seq){
  
  # date class gets stripped in for loop
  current_date <- as.Date(cutoff, origin = "1970-01-01")
  col_name <- as.character(current_date)
  
  # create temp df
  temp_stats <- ssurgo_coiids |>
    # column assessing correlation status at each timepoint
    mutate(before_cutoff = corr_date < current_date) |> 
    # group and summarise correlation status by mukey
    group_by(mukey) |> 
    summarise(
      perc = round(100 * sum(before_cutoff, na.rm = TRUE) / n()),
      .groups = "drop"
    ) |> 
    # name column with timestep
    rename(!!col_name := perc)
  
  # join to final df
  final_df <- left_join(final_df, temp_stats, by = "mukey")
}

# Perform raster replacement ----------------------------------------------

mukey <- final_df$mukey

for(j in seq_along(date_seq)){
  
  # iteration
  print(paste(j, "of 49"))
  
  # access percent values
  perc <- final_df[ , 1 + j]
  
  # function
  .f <- function(i){
    .idx <- match(i, mukey)
    .res <- perc[.idx]
    
    return(.res)
  }
  
  system.time(
    r <- app(
      r_mukey,
      fun = .f,
      filename = paste0("D:/es_maps/rasters/30m/", date_seq[j], ".tif"),
      overwrite = TRUE,
      wopt = list(datatype = 'INT1U')
    )
  )
  
}

# Aggregate ---------------------------------------------------------------


## mean --------------------------------------------------------------------



for(j in seq_along(date_seq)){
  
  r <- rast(paste0("D:/es_maps/rasters/30m/", date_seq[j], ".tif"))
  
  a <- aggregate(
      r,
      fact = 10,
      fun = 'mean', na.rm = FALSE,
      filename = paste0("D:/es_maps/gif/mean_agg/", date_seq[j], ".tif"),
      overwrite = TRUE,
      wopt = list(datatype = 'INT1U')
  )
}


## median ------------------------------------------------------------------

for(j in seq_along(date_seq)){
  
  r <- rast(paste0("D:/es_maps/rasters/30m/", date_seq[j], ".tif"))
  
  a <- aggregate(
    a <- aggregate(
      r,
      fact = 10,
      fun = 'median', na.rm = FALSE,
      filename = paste0("D:/es_maps/gif/median_agg/", date_seq[j], ".tif"),
      overwrite = TRUE,
      wopt = list(datatype = 'INT1U')
    )
  )
}


# Plot --------------------------------------------------------------------

g <- rast("D:/es_maps/gif/mean_agg/2010-05-01.tif")

my_ext <- ext(g)
map_width <- my_ext$xmax - my_ext$xmin
map_height <- my_ext$ymax - my_ext$ymin

asp_ratio <- map_height / map_width

my_width = 800
my_height = 800 * asp_ratio

my_rasts <- list.files("D:/es_maps/gif/mean_agg/")

for(i in seq_along(my_rasts)){
  
  # print(paste0("D:/es_maps/gif/",
  #              substr(my_rasts[i], start = 1, stop = nchar(my_rasts[i]) - 4),
  #              ".jpg"))
  
  # load raster
  t_rast <- rast(paste0("D:/es_maps/gif/mean_agg/", my_rasts[i]))
  
  # p <- ggplot() + geom_spatraster(data = t_rast) + 
  #   labs(Title = paste("Ecological Site Correlation -",
  #                      substr(my_rasts[i], start = 1, stop = 10)))
  
  # save jpg
  jpeg(filename = paste0("D:/es_maps/gif/plots/",
                         substr(my_rasts[i], 1, nchar(my_rasts[i]) - 4),
                         ".jpg"),
       width = my_width,
       height = my_height,
       units = "px",
       quality = 90
  )
  
  # # Save using ggsave
  # ggsave(
  #   filename = paste0(substr(my_rasts[i], 1, nchar(my_rasts[i]) - 4), ".jpg"),
  #   plot = p,
  #   path = "D:/es_maps/gif/plots/",
  #   width = my_width,
  #   height = my_height,
  #   units = "px",
  #   dpi = 300  # High resolution
  # )
  
  # plot
  

  terra::plot(t_rast, range = c(0, 100), main = paste("Ecological Site Correlation -",
                                                           substr(my_rasts[i], start = 1, stop = 10)),
              cex.main = 2.5,
              axes = F,
              mar = c(3.1, 3.1, 5.1, 7.1),
              loc.main = c(-46125, 3300000))

  dev.off()
  
}


# Make gif ----------------------------------------------------------------


# 1. Get a list of your .jpg files (ensure they are in the correct order)
img_files <- list.files(path = "D:/es_maps/gif/plots", full.names = TRUE)

# 2. Read images, join them, and animate
my_gif <- image_read(img_files) %>%
  image_join() %>%
  image_animate(fps = 4) # Adjust frames per second as needed

# 3. Write the final GIF to disk
image_write(my_gif, "D:/es_maps/ecological_site_correlation_mean_agg.gif")



