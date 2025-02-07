library(googlesheets4)
library(googledrive)
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(httr)  # For downloading images
library(stringr)

# Read the spreadsheet
drive_auth()

d <- read_sheet("https://docs.google.com/spreadsheets/d/1t8X7Hp2ff-bX3AgNp0Tc8J8r9wfeC6RbYScvNZk7JLc/edit?usp=sharing")

# Function to clean and format filenames
sanitize_filename <- function(name, surname) {
  filename <- paste0("staff_", name, "_", surname, ".jpg")
  filename <- str_replace_all(filename, "[^A-Za-z0-9_.]", "_")  # Replace special characters
  return(filename)
}

# Function to extract Google Drive File ID
extract_drive_id <- function(url) {
  if (is.na(url) || url == "") {
    return(NA)
  }
  
  match <- str_match(url, "file/d/([^/]+)/|open\\?id=([^&]+)")
  file_id <- coalesce(match[,2], match[,3])  # Get the first non-NA match
  
  return(file_id)
}

# Function to download/update images only if they have changed
download_image <- function(url, name, surname, image_folder) {
  file_id <- extract_drive_id(url)
  filename <- sanitize_filename(name, surname)
  image_path <- file.path(image_folder, filename)
  
  if (!is.na(file_id)) {
    if (file.exists(image_path)) {
      local_mod_time <- file.info(image_path)$mtime
      drive_metadata <- drive_get(as_id(file_id))
      remote_mod_time <- drive_metadata$drive_resource[[1]]$modifiedTime
      remote_mod_time <- as.POSIXct(remote_mod_time, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
      
      if (remote_mod_time > local_mod_time) {
        message("Updating image: ", filename)
        drive_download(as_id(file_id), path = image_path, overwrite = TRUE)
      } else {
        message("Image is up to date: ", filename)
      }
      return(image_path)
    } else {
      message("Downloading new image: ", filename)
      drive_download(as_id(file_id), path = image_path, overwrite = TRUE)
      return(image_path)
    }
  } else {
    return(NA)
  }
}

# Function to generate the HTML for each person
generate_staff_card <- function(name, surname, bio, img_path, homepage_url) {
  paste0(
    '<div style="display: flex; align-items: flex-start; margin-bottom: 20px; border: 1px solid #ddd; padding: 10px; border-radius: 10px;">',
    '<div style="flex: 1; max-width: 150px;">',
    '<img src="', img_path, '" alt="', name, ' ', surname, '" style="width: 100%; border-radius: 10px;">',
    '</div>',
    '<div style="flex: 3; margin-left: 20px;">',
    '<h3 style="margin: 5px 0;">', name, ' ', surname, '</h3>',
    '<p style="margin: 5px 0;">', bio, '</p>',
    '<a href="', homepage_url, '" target="_blank">', homepage_url, '</a>',
    '</div>',
    '</div>'
  )
}

CreateTeamSet <- function(status) {
  staff <- d %>% filter(Status == status)
  
  image_folder <- "staff_images"
  if (!dir.exists(image_folder)) {
    dir.create(image_folder)
  }
  
  if (nrow(staff) > 0) {
    staff <- staff %>% group_by(Name, `Surname(s)`) %>% slice_tail(n = 1) %>% ungroup()  # Keep only the last entry per person
    
    staff$image_path <- mapply(download_image, 
                               staff$`Upload your pic (Mb max, square format)`, 
                               staff$Name, 
                               staff$`Surname(s)`,
                               MoreArgs = list(image_folder = image_folder))
    
    for (i in 1:nrow(staff)) {
      if (!is.na(staff$image_path[i]) && !is.na(staff$Name[i])) {
        cat(generate_staff_card(
          name = staff$Name[i],
          surname = staff$`Surname(s)`[i],
          bio = staff$`Short Bio`[i],
          img_path = ifelse(!is.na(staff$image_path[i]), staff$image_path[i], "default.jpg"),
          homepage_url = staff$`url home page`[i]
        ))
      }
    }
  } else {
    message("No staff found. Skipping rendering.")
  }
}