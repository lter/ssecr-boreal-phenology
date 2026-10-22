library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0
library(glue) # Interpreted String Literals CRAN v1.8.1
library(pals) # Color Palettes, Colormaps, and Tools to Evaluate Them CRAN v1.10
library(googledrive) # An Interface to Google Drive CRAN v2.1.2


#> This script reads the FLUXNET data stored as zip files in our shared Google Drive.
#> It locally download and extract the just the csv file containing the hourly (HR) or 
#> half-hourly (HH) fluxes for each site and combine it into 1 full dataset adding 
#> a column with the site ID. Additionally, it checks if the data is HR or HH and if
#> variables like CO2 and CH4 fluxes among others are available.


# Authenticate with your Google Drive (if not already done)
drive_auth()

# Find the files on Google Drive FLUXNET_data folder
zip_files <- 
  drive_ls(path = "https://drive.google.com/drive/u/1/folders/1koXWV0942E_x2xhgzF8JHYdnsmQG6mS6",
           pattern = "*.zip"
           # q = "name = 'AMF_CA-ARB_FLUXNET_2011-2024_v1.3_r1.zip'"
  )

# Loop to uncompress and read the data

FLUXNET_metadata <- list()
temp_dir <- tempdir()

for (i in seq_len(nrow(zip_files))) {
  
  zip_to_uncompress <- 
    zip_files |> 
    slice(i)
  
  # Create a temporary file path in R's volatile RAM/temp space 
  # (Note: tempfile() creates a placeholder path that OS handles automatically and deletes)
  temp_zip <- tempfile(fileext = ".zip")
  
  # Download it to the temporary session path
  drive_download(zip_to_uncompress, path = temp_zip, overwrite = TRUE)
  
  # List the contents inside the zip file without extracting everything permanently
  zip_contents <- 
    unzip(temp_zip, list = TRUE)
  
  
  file_metadata <-
    # Get info from zip files, based on
    # https://fluxnet.org/data/fluxnet2015-dataset/
    zip_contents |> 
    filter(str_detect(Name, "FLUXMET") & str_detect(Name, ".csv")) |> 
    mutate(site_id = str_extract(Name, "[:alpha:]{2}-[:alnum:]{3}"),
           avg_time = str_extract(Name, "_HH_|_HR_"),
           avg_time = str_remove_all(avg_time, "_"),
           period_time = str_extract(Name, "\\d{4}-\\d{4}")) |> 
    filter(!is.na(avg_time)) |> 
    distinct(site_id, avg_time, period_time)
    
  # Read a specific file from the zip archive directly into R (e.g., a CSV file)
  target_file_name <-
    zip_contents |> 
    ## HH means for Half-Hourly and HR for Hourly
    filter(str_detect(Name, "FLUXMET_(HH|HR)")) |> 
    pull(Name)
  
  unzip(temp_zip, files = target_file_name, exdir = temp_dir)
  
  # Load the data into R
  file_path <- file.path(temp_dir, target_file_name)
  # fread function is really faster!
  flux_data <- data.table::fread(file_path, na.strings = "-9999")
  
  # get more info about the contents of the csv
  
  FLUXNET_metadata[[i]] <- 
    file_metadata |> 
    mutate(has_CO2_flux = sum(str_detect(names(flux_data), "NEE")) != 0,
           has_CH4_flux = sum(str_detect(names(flux_data), "CH4")) != 0,
           has_precipitation = sum(str_detect(names(flux_data), "^P$")) != 0,
           has_temperature = sum(str_detect(names(flux_data), "^TA_F$")) != 0)
  
  flux_data <- 
    flux_data |> 
    as_tibble() |> 
    mutate(SITE_ID = str_extract(target_file_name, "[:alpha:]{2}-[:alnum:]{3}")) |> 
    relocate(SITE_ID)
  
  # (Optional) Clean up the temporary file immediately
  unlink(temp_zip)
  
  
  # data used for plot below
  data_to_plot <-
    flux_data |> 
    mutate(TIMESTAMP = strptime(TIMESTAMP_START, "%Y%m%d%H%M"),
           TIMESTAMP = as.POSIXct(TIMESTAMP),
           HOUR = hour(TIMESTAMP),# + minute(TIMESTAMP)/60,
           DOY = yday(TIMESTAMP),
           YEAR = year(TIMESTAMP)) |> 
    filter(YEAR %in% 2011:2025) |> 
    relocate(TIMESTAMP, .after = SITE_ID) |> 
    select(SITE_ID, TIMESTAMP, YEAR, DOY, HOUR, NEE_CUT_REF, P_F)
  
  SITE_ID_LABEL <- unique(data_to_plot$SITE_ID)
  
  
  # Fingerprint plot of temperature, CO2 and CH4
  co2_flux_fingerprint <- 
    ggplot(data = data_to_plot,
           aes(x = HOUR,
               y = DOY,
               z = NEE_CUT_REF)) +
    facet_wrap(~YEAR,
               ncol = 5) +
    stat_summary_2d(binwidth = c(1, 1)) +
    labs(title = parse(text = glue("'Site ID:'~bold({SITE_ID_LABEL})"))) +
    scale_x_continuous(name = "Hour (Local lime)",
                       breaks = seq(0, 24, 6),
                       labels = \(x) str_sub(hms::as_hms(x*60*60), 1, 5),
                       limits = c(0, 23),
                       expand = c(0,0)) +
    scale_y_continuous(name = "Month",
                       breaks = cumsum(days_in_month(1:12)) - 30,
                       labels = month.abb,
                       expand = c(0,0)) +
    # geom_contour(breaks = 0,
    #              lwd = 0.2,
    #              color = "black") +
    scale_fill_gradientn(name = bquote(atop('NEE filled', (mu*mol~m^-2~s^-1))),
                         colours = pals::ocean.curl(5),
                         values = scales::rescale(with(data_to_plot, c(min(NEE_CUT_REF, na.rm = TRUE), 0, max(NEE_CUT_REF, na.rm = TRUE)))),
                         limits = with(data_to_plot, c(min(NEE_CUT_REF, na.rm = TRUE), max(NEE_CUT_REF, na.rm = TRUE)))
    ) +
    theme_bw() +
    theme(legend.position = "top",
          legend.key.width = unit(3, "line"))
  
  ggsave(filename = glue("{SITE_ID_LABEL}-fingerprint.png"),
         path = "output/figures",
         width = 9,
         height = 6,
         create.dir = TRUE)
  
}


#> Metadata of all the sites is appended and saved as a csv file in
#> output/data

FLUXNET_metadata |> 
  list_rbind() |> 
  write_csv(file = "output/data/FLUXNET-sites-metadata.csv")




