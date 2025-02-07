library(revtools)
library(dplyr)

library(tidyr)
library(rvest)


library(httr)
library(jsonlite)
# Define the cache file


# Load existing DOI cache if available
load_doi_cache <- function() {
  if (file.exists(cache_file)) {
    return(fromJSON(cache_file))
  }
  return(data.frame(Title = character(), DOI = character(), stringsAsFactors = FALSE))
}

doi_cache <- load_doi_cache()

# Function to save DOI cache
save_doi_cache <- function(cache) {
write(toJSON(cache, pretty = TRUE), cache_file)
}

# Function to obtain the correct DOI given a paper reference
get_correct_doi <- function(title, author, year) {
  # Check if DOI is already cached
  cached_doi <- doi_cache$DOI[doi_cache$Title == title]
  if (length(cached_doi) > 0) {
    return(cached_doi[1])
  }
  
  query <- paste(title, author, year, sep = " ")
  query_url <- paste0("https://api.crossref.org/works?query=", URLencode(query))
  
  response <- tryCatch(GET(query_url), error = function(e) return(NULL))
  
  if (!is.null(response) && status_code(response) == 200) {
    content_text <- content(response, as = "text", encoding = "UTF-8")
    content_json <- tryCatch(fromJSON(content_text, flatten = TRUE), error = function(e) return(NULL))
    
    if (!is.null(content_json) && "message" %in% names(content_json) &&
        "items" %in% names(content_json$message) && length(content_json$message$items) > 0) {
      
      first_item <- content_json$message$items[1,]  # Get the first item
      
      if ("DOI" %in% names(first_item) && !is.null(first_item$DOI)) {
        doi_cache <<- rbind(doi_cache, data.frame(Title = title, DOI = first_item$DOI, stringsAsFactors = FALSE))
        save_doi_cache(doi_cache)
        return(first_item$DOI)
      }
    }
  }
  
  return(NA)  # Return NA if no DOI is found
}

paperstohtmlnodiv <- function(ris_url) {
  # Download the RIS file
  ris_path <- "publications.ris"
  GET(ris_url, write_disk(ris_path, overwrite = TRUE))
  
  # Read the RIS file with encoding fix
  ris_lines <- readLines(ris_path, encoding = "UTF-8", warn = FALSE)
  ris_lines <- iconv(ris_lines, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  writeLines(ris_lines, ris_path)
  
  # Read the cleaned RIS file using revtools
  ris_data <- read_bibliography(file = ris_path)
  
  # Select relevant fields and clean the data
  pubs_df <- ris_data %>%
    select(any_of(c("author", "title", "year", "journal", "volume", "issue", "url"))) %>%
    rename(
      Authors = author,
      Title = title,
      Year = year,
      Journal = journal,
      Volume = volume,
      Issue = issue,
      DOI = url
    ) %>%
    mutate(
      across(everything(), ~ replace_na(.x, "")),
      Authors = gsub("[`´~’]", "", Authors)
    )
  
  # Retrieve correct DOIs if missing or malformed
  pubs_df$DOI <- sapply(1:nrow(pubs_df), function(i) {
    if (pubs_df$DOI[i] == "" || !grepl("^10\\.", pubs_df$DOI[i])) {
      correct_doi <- get_correct_doi(pubs_df$Title[i], pubs_df$Authors[i], pubs_df$Year[i])
      return(ifelse(is.na(correct_doi), "", correct_doi))
    }
    return(pubs_df$DOI[i])
  })
  
  pubs_df$DOI <- ifelse(pubs_df$DOI != "", paste0("<a href='https://doi.org/", pubs_df$DOI, "' target='_blank'>[DOI]</a>"), "")
  
  # Convert year to numeric and replace NA values
  pubs_df$Year <- as.numeric(pubs_df$Year)
  pubs_df$Year[is.na(pubs_df$Year)] <- 0
  
  # Sort by Year (newest first)
  pubs_df <- pubs_df %>% arrange(desc(Year))
  
  # Generate HTML output
  if (nrow(pubs_df) > 0) {
    current_year <- ""
    counter <- 1  # Continuous numbering across years
    
    for (i in 1:nrow(pubs_df)) {
      if (!is.na(pubs_df$Year[i]) && pubs_df$Year[i] != current_year) {
        cat(paste0("<h2>", ifelse(pubs_df$Year[i] == 0, "Unknown Year", pubs_df$Year[i]), "</h2>"))
        current_year <- pubs_df$Year[i]
      }
      
      cat(paste0("<p><strong>", counter, ". ", pubs_df$Title[i], "</strong><br>"))
      cat(paste0(pubs_df$Authors[i], "<br>"))
      cat(paste0("*", pubs_df$Journal[i], "*", ", Vol. ", pubs_df$Volume[i], ", Issue ", pubs_df$Issue[i], " (", ifelse(pubs_df$Year[i] == 0, "Unknown", pubs_df$Year[i]), ")<br>"))
      
      if (!is.na(pubs_df$DOI[i]) && pubs_df$DOI[i] != "") {
        cat(paste0( pubs_df$DOI[i] ))
#        cat(paste0(" <a href='https://doi.org/", pubs_df$DOI[i], "' target='_blank'>[DOI]</a>"))
        
        }
      
      cat("</p>")
      
      counter <- counter + 1
    }
  } else {
    cat("<p>No valid publications available.</p>")
  }
}



# 
# 
# paperstohtml <- function(ris_url) {
#   # Download the RIS file
#   ris_path <- "publications.ris"
#   GET(ris_url, write_disk(ris_path, overwrite = TRUE))
#   
#   # Read the RIS file with encoding fix
#   ris_lines <- readLines(ris_path, encoding = "UTF-8", warn = FALSE)
#   ris_lines <- iconv(ris_lines, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
#   writeLines(ris_lines, ris_path)
#   
#   # Read the cleaned RIS file using revtools
#   ris_data <- read_bibliography(file = ris_path)
#   
#   # Select relevant fields and clean the data
#   pubs_df <- ris_data %>%
#     select(any_of(c("author", "title", "year", "journal", "volume", "issue", "url"))) %>%
#     rename(
#       Authors = author,
#       Title = title,
#       Year = year,
#       Journal = journal,
#       Volume = volume,
#       Issue = issue,
#       DOI = url
#     ) %>%
#     mutate(
#       across(everything(), ~ replace_na(.x, "")),
#       Authors = gsub("[`´~’]", "", Authors)
#     )
#   
#   # Retrieve correct DOIs if missing or malformed
#   pubs_df$DOI <- sapply(1:nrow(pubs_df), function(i) {
#     if (pubs_df$DOI[i] == "" || !grepl("^10\\.", pubs_df$DOI[i])) {
#       correct_doi <- get_correct_doi(pubs_df$Title[i], pubs_df$Authors[i], pubs_df$Year[i])
#       return(ifelse(is.na(correct_doi), "", correct_doi))
#     }
#     return(pubs_df$DOI[i])
#   })
#   
#   pubs_df$DOI <- ifelse(pubs_df$DOI != "", paste0("<a href='https://doi.org/", pubs_df$DOI, "' target='_blank'>[DOI]</a>"), "")
#   
#   # Convert year to numeric and replace NA values
#   pubs_df$Year <- as.numeric(pubs_df$Year)
#   pubs_df$Year[is.na(pubs_df$Year)] <- 0
#   
#   # Sort by Year (newest first)
#   pubs_df <- pubs_df %>% arrange(desc(Year))
#   
#   # Generate HTML output
#   if (nrow(pubs_df) > 0) {
#     current_year <- ""
#     counter <- 1  # Continuous numbering across years
#     
#     for (i in 1:nrow(pubs_df)) {
#       if (!is.na(pubs_df$Year[i]) && pubs_df$Year[i] != current_year) {
#         cat(paste0("<h2>", ifelse(pubs_df$Year[i] == 0, "Unknown Year", pubs_df$Year[i]), "</h2>"))
# #        cat(paste0("## ", ifelse(pubs_df$Year[i] == 0, "Unknown Year", pubs_df$Year[i]), ""))
#         
#                 current_year <- pubs_df$Year[i]
#       }
#       
#       cat(paste0("<p><strong>", counter, ". ", pubs_df$Title[i], "</strong><br>"))
#       cat(paste0(pubs_df$Authors[i], "<br>"))
#       cat(paste0("*", pubs_df$Journal[i], "*", ", Vol. ", pubs_df$Volume[i], ", Issue ", pubs_df$Issue[i], " (", ifelse(pubs_df$Year[i] == 0, "Unknown", pubs_df$Year[i]), ")<br>"))
#       
#       if (pubs_df$DOI[i] != "") {
#         cat(paste0(pubs_df$DOI[i]))
#       }
#       
#       cat("</p>")
#       
#       counter <- counter + 1
#     }
#   } else {
#     cat("<p>No valid publications available.</p>")
#   }
# }
# 

# Function to scrape Google Scholar profile publications
#get_scholar_publications <- function(profile_url) {
  # Read the Scholar profile page
  page <- read_html(profile_url)
  
  # Extract publication titles
  titles <- page %>%
    html_nodes(".gsc_a_t a") %>%
    html_text()
  
  # Extract authors and publication details
  authors <- page %>%
    html_nodes(".gsc_a_t .gs_gray") %>%
    html_text()
  
  # Extract years
  years <- page %>%
    html_nodes(".gsc_a_y span") %>%
    html_text()
  
  # Extract links to the publications
  links <- page %>%
    html_nodes(".gsc_a_t a") %>%
    html_attr("href")
  links <- paste0("https://scholar.google.com", links)
  
  # Combine into a data frame
  publications_df <- data.frame(
    Title = titles,
    Authors = authors,
    Year = years,
    URL = links,
    stringsAsFactors = FALSE
  )
  
  return(publications_df)
}


paperstohtml <- function(ris_url) {
  # Download the RIS file
  ris_path <- "publications.ris"
  GET(ris_url, write_disk(ris_path, overwrite = TRUE))
  
  # Read the RIS file with encoding fix
  ris_lines <- readLines(ris_path, encoding = "UTF-8", warn = FALSE)
  ris_lines <- iconv(ris_lines, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  writeLines(ris_lines, ris_path)
  
  # Read the cleaned RIS file using revtools
  ris_data <- read_bibliography(file = ris_path)
  
  # Select relevant fields and clean the data
  pubs_df <- ris_data %>%
    select(any_of(c("author", "title", "year", "journal", "volume", "issue", "url"))) %>%
    rename(
      Authors = author,
      Title = title,
      Year = year,
      Journal = journal,
      Volume = volume,
      Issue = issue,
      DOI = url
    ) %>%
    mutate(
      across(everything(), ~ replace_na(.x, "")),
      Authors = gsub("[`´~’]", "", Authors)
    )
  
  # Retrieve correct DOIs if missing or malformed
  pubs_df$DOI <- sapply(1:nrow(pubs_df), function(i) {
    if (pubs_df$DOI[i] == "" || !grepl("^10\\.", pubs_df$DOI[i])) {
      correct_doi <- get_correct_doi(pubs_df$Title[i], pubs_df$Authors[i], pubs_df$Year[i])
      return(ifelse(is.na(correct_doi), "", correct_doi))
    }
    return(pubs_df$DOI[i])
  })
  
  pubs_df$DOI <- ifelse(pubs_df$DOI != "", paste0("<a href='https://doi.org/", pubs_df$DOI, "' target='_blank'>[DOI]</a>"), "")
  
  # Convert year to numeric and replace NA values
  pubs_df$Year <- as.numeric(pubs_df$Year)
  pubs_df$Year[is.na(pubs_df$Year)] <- 0
  
  # Sort by Year (newest first)
  pubs_df <- pubs_df %>% arrange(desc(Year))
  
  # Generate HTML output and store it in a variable
  html_output <- ""
  
  if (nrow(pubs_df) > 0) {
    current_year <- ""
    counter <- 1  # Continuous numbering across years
    
    for (i in 1:nrow(pubs_df)) {
      if (!is.na(pubs_df$Year[i]) && pubs_df$Year[i] != current_year) {
        html_output <- paste0(html_output, "<h2>", ifelse(pubs_df$Year[i] == 0, "Unknown Year", pubs_df$Year[i]), "</h2>")
        current_year <- pubs_df$Year[i]
      }
      
      html_output <- paste0(
        html_output,
        "<p><strong>", counter, ". ", pubs_df$Title[i], "</strong><br>",
        pubs_df$Authors[i], "<br>",
        "<em>", pubs_df$Journal[i], "</em>", ", Vol. ", pubs_df$Volume[i], ", Issue ", pubs_df$Issue[i], 
        " (", ifelse(pubs_df$Year[i] == 0, "Unknown", pubs_df$Year[i]), ")<br>"
      )
      
      if (pubs_df$DOI[i] != "") {
        html_output <- paste0(html_output, pubs_df$DOI[i])
      }
      
      html_output <- paste0(html_output, "</p>")
      counter <- counter + 1
    }
  } else {
    html_output <- "<p>No valid publications available.</p>"
  }
  
  knitr::asis_output(html_output)
}


# Function to scrape Google Scholar profile publications
get_scholar_publications <- function(profile_url) {
  # Read the Scholar profile page
  page <- read_html(profile_url)
  
  # Extract publication entries
  entries <- page %>% html_nodes(".gsc_a_tr")
  
  # Extract publication titles
  titles <- entries %>%
    html_nodes(".gsc_a_t a") %>%
    html_text() %>%
    unique()
  
  # Extract authors and publication details
  authors <- entries %>%
    html_nodes(".gsc_a_t .gs_gray") %>%
    html_text() %>%
    unique()
  
  # Extract years
  years <- entries %>%
    html_nodes(".gsc_a_y span") %>%
    html_text() %>%
    unique()
  
  # Extract links to the publications
  links <- entries %>%
    html_nodes(".gsc_a_t a") %>%
    html_attr("href") %>%
    unique()
  links <- paste0("https://scholar.google.com", links)
  
  # Ensure consistent length by trimming to minimum vector size
  min_length <- min(length(titles), length(authors), length(years), length(links))
  
  publications_df <- data.frame(
    Title = titles[1:min_length],
    Authors = authors[1:min_length],
    Year = years[1:min_length],
    URL = links[1:min_length],
    stringsAsFactors = FALSE
  )
  
  return(publications_df)
}