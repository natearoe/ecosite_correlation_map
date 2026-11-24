library(magick)


conus <- "D:/es_maps/gif/plots/"
ak <- "D:/es_maps/gif/plots_AK/"
hi <- "D:/es_maps/gif/plots_HI/"

conus_list <- list.files(conus)
ak_list <- list.files(ak)
hi_list <- list.files(hi)

start_date <- as.Date("2010-01-01")
end_date <- Sys.Date()

date_seq <- seq.Date(
  from = start_date,
  to = end_date,
  by = "4 months"
)

date_seq <- date_seq[2:length(date_seq)]

for(i in seq_along(date_seq)){
  
  my_name <- date_seq[i]
  
  conus_img <- paste0(conus, my_name, ".jpg")
  ak_img <- paste0(ak, my_name, ".jpg")
  hi_img <- paste0(hi, my_name, ".jpg")
  
  img1 <- image_read(hi_img) |>  image_transparent(color = "white", fuzz = 10)
  img2 <- image_read(ak_img) |> image_transparent(color = "white", fuzz = 10)
  img3 <- image_read(conus_img) |> image_transparent(color = "white", fuzz = 10)
  
  canvas <- image_blank(width = 800,
                        height = 800, color = "white")
  
  test <- canvas |> 
    image_composite(img3) |> 
    image_composite(img1, offset = "+500+500") |> 
    image_composite(img2, offset = "+0+450") 
  
  image_write(test, paste0("D:/es_maps/gif/plot_compiled/", my_name, ".jpg"))
  
}


# 1. Get a list of your .jpg files (ensure they are in the correct order)
img_files <- list.files(path = "D:/es_maps/gif/plot_compiled", full.names = TRUE)

# 2. Read images, join them, and animate
my_gif <- image_read(img_files) %>%
  image_join() %>%
  image_animate(fps = 4,
                loop = 2) # Adjust frames per second as needed

# 3. Write the final GIF to disk
image_write_video(my_gif, "D:/es_maps/ecological_site_correlation_mean_agg_compiled_movie.mp4",
                  framerate = 5)





# 4. View or Save
print(final_plot)
