library(revtools)
library(dplyr)
library(tidyr)
library(rvest)
library(httr)
library(jsonlite)
library(scholar)
library(stringr)

# Function to process RIS file
process_ris_file <- function(ris_url) {
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
  return(pubs_df)
}




# Function to save DOI cache
save_doi_cache <- function(cache) {
  write(toJSON(cache, pretty = TRUE), cache_file)
}



# Load existing DOI cache if available
load_doi_cache <- function() {
  if (file.exists(cache_file)) {
    return(fromJSON(cache_file))
  }
  return(data.frame(Title = character(), DOI = character(), stringsAsFactors = FALSE))
}

doi_cache <- load_doi_cache()



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


# Fix Authors column if it contains journal names instead of actual authors
clean_google_scholar_authors <- function(authors) {
  cleaned_authors <- sapply(authors, function(x) {
    if (grepl("\\d{4}", x) | grepl("Nephrology|Transplantation|Supplement", x)) {
      return("")
    }
    return(x)
  }, USE.NAMES = FALSE)
  return(cleaned_authors)
}

# Function to fetch publications from Google Scholar or Futur UPC
get_publications <- function(profile_url) {
  if (grepl("scholar.google.com", profile_url)) {
    return(get_scholar_publications(profile_url))
  } else if (grepl("futur.upc.edu", profile_url)) {
    return(get_futur_publications(profile_url))
  } else {
    stop("Unsupported URL format")
  }
}

# Function to scrape Google Scholar profile publications
# get_scholar_publications <- function(profile_url) {
#   page <- read_html(profile_url)
#   entries <- page %>% html_nodes(".gsc_a_tr")
#   
#   titles <- entries %>% html_nodes(".gsc_a_t a") %>% html_text()
#   authors <- entries %>% html_nodes(".gsc_a_t .gs_gray") %>% html_text()
#   years <- entries %>% html_nodes(".gsc_a_y span") %>% html_text()
#   links <- entries %>% html_nodes(".gsc_a_t a") %>% html_attr("href")
#   links <- paste0("https://scholar.google.com", links)
#   
#   min_length <- min(length(titles), length(authors), length(years), length(links))
#   
#   ris_data <- data.frame(
#     title = titles[1:min_length],
#     authors = authors[1:min_length],
#     year = years[1:min_length],
#     url = links[1:min_length],
#     stringsAsFactors = FALSE
#   )
#   
#   pubs_df <- ris_data %>%
#     select(any_of(c("authors", "title", "year", "journal", "volume", "issue", "url"))) %>%
#     rename(
#       Authors = authors,
#       Title = title,
#       Year = year,
#       Journal = journal,
#       Volume = volume,
#       Issue = issue,
#       DOI = url
#     ) %>%
#     mutate(
#       across(everything(), ~ replace_na(.x, "")),
#       Authors = gsub("[`´~’']", "", Authors)
#     )
#   return(pubs_df)
#   
# }

# Function to scrape Google Scholar profile publications
# Function to extract Google Scholar ID from profile URL
extract_scholar_id <- function(profile_url) {
  scholar_id <- str_extract(profile_url, "(?<=user=)[a-zA-Z0-9_-]+")
  if (is.na(scholar_id) || scholar_id == "") {
    stop("Invalid Google Scholar URL. Please provide a valid profile URL.")
  }
  return(scholar_id)
}

# Function to fetch publications using the scholar package
get_scholar_publications <- function(profile_url) {
  # Extract the Scholar ID from the given URL
  scholar_id <- extract_scholar_id(profile_url)
  
  # Fetch all publications
  pubs_df <- scholar::get_publications(scholar_id)
  
  # Ensure the correct structure
  pubs_df <- pubs_df %>%
    select(title, author, journal,year, number,pubid) %>%
    rename(
      Title = title,
      Authors = author,
      Year = year,
      URL = pubid  # Placeholder before constructing the full URL
    ) %>%
    mutate(
      URL = paste0("https://scholar.google.com/citations?view_op=view_citation&hl=en&citation_for_view=", scholar_id, ":", URL),
      Journal = journal,
      Volume = number,
      Issue = ""
    )
  
 
  
  return(pubs_df)
}


get_futur_publications <- function(ris_url) {
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
  return(pubs_df)
}


# Function to display publications in HTML format
paperstohtmlnodiv <- function(input_url) {
  if (grepl("\\.ris$", input_url)) {
    pubs_df <- process_ris_file(input_url)
  } else if (grepl("scholar.google.com|futur.upc.edu", input_url)) {
    pubs_df <- get_publications(input_url)
  } else {
    stop("Unsupported format: Provide a valid Google Scholar, Futur UPC profile link, or RIS file URL.")
  }
  
  # Convert year to numeric and handle NAs
  pubs_df$Year <- as.numeric(pubs_df$Year)
  pubs_df$Year[is.na(pubs_df$Year)] <- 0
  
  # Retrieve correct DOIs if missing or malformed
  # pubs_df$DOI <- sapply(1:nrow(pubs_df), function(i) {
  #   if (pubs_df$DOI[i] == "" || !grepl("^10\\.", pubs_df$DOI[i])) {
  #     correct_doi <- get_correct_doi(pubs_df$Title[i], pubs_df$Authors[i], pubs_df$Year[i])
  #     return(ifelse(is.na(correct_doi), "", correct_doi))
  #   }
  #   return(pubs_df$DOI[i])
  # })
  pubs_df$DOI <- sapply(1:nrow(pubs_df), function(i) {
    doi_value <- pubs_df$DOI[i]
    # Ensure DOI is not NULL, NA, or empty string before checking its format
    if (is.null(doi_value) || is.na(doi_value) || doi_value == "" || !grepl("^10\\.", doi_value)) {
      correct_doi <- get_correct_doi(pubs_df$Title[i], pubs_df$Authors[i], pubs_df$Year[i])
      return(ifelse(is.null(correct_doi) || is.na(correct_doi), "", correct_doi))
    }
    
    
    return(doi_value)
  }, USE.NAMES = FALSE)
  
  
  # Fix Authors column for Google Scholar data
  if ("Authors" %in% colnames(pubs_df)) {
    pubs_df$Authors <- clean_google_scholar_authors(pubs_df$Authors)
  }
  
  # Ensure Journal, Volume, and Issue exist (especially for Google Scholar)
  if (!"Journal" %in% colnames(pubs_df)) pubs_df$Journal <- ""
  if (!"Volume" %in% colnames(pubs_df)) pubs_df$Volume <- ""
  if (!"Issue" %in% colnames(pubs_df)) pubs_df$Issue <- ""
  
  # Convert DOIs to clickable links
  pubs_df$DOI <- ifelse(pubs_df$DOI != "", paste0("<a href='https://doi.org/", pubs_df$DOI, "' target='_blank'>[DOI]</a>"), "")
  
  # Sort by Year (newest first)
  pubs_df <- pubs_df %>% arrange(desc(Year))
  
  # Generate HTML output
  if (nrow(pubs_df) > 0) {
    current_year <- ""
    counter <- 1  # Continuous numbering across years
    
    for (i in seq_len(nrow(pubs_df))) {
      if (!is.na(pubs_df$Year[i]) && pubs_df$Year[i] != current_year) {
        cat(paste0("<h2>", ifelse(pubs_df$Year[i] == 0, "Unknown Year", pubs_df$Year[i]), "</h2>"))
        current_year <- pubs_df$Year[i]
      }
      
      cat(paste0("<p><strong>", counter, ". ", pubs_df$Title[i], "</strong><br>"))
      cat(paste0(pubs_df$Authors[i], "<br>"))
      cat(paste0("*", pubs_df$Journal[i], "*", ", Vol. ", pubs_df$Volume[i], ", Issue ", pubs_df$Issue[i], 
                 " (", ifelse(pubs_df$Year[i] == 0, "Unknown", pubs_df$Year[i]), ")<br>"))
      
      if (!is.na(pubs_df$DOI[i]) && pubs_df$DOI[i] != "") {
        cat(paste0(pubs_df$DOI[i]))
      }
      cat("</p>")
      
      counter <- counter + 1
    }
  } else {
    cat("<p>No valid publications available.</p>")
  }
}


# Function for long lists: no pagination, better collapsible sections
paperstohtmlnodivlong <- function(input_url) {
  if (grepl("\\.ris$", input_url)) {
    pubs_df <- process_ris_file(input_url)
  } else if (grepl("scholar.google.com|futur.upc.edu", input_url)) {
    pubs_df <- get_publications(input_url)
  } else {
    stop("Unsupported format: Provide a valid Google Scholar, Futur UPC profile link, or RIS file URL.")
  }
  
  # Convert year to numeric and handle NAs
  pubs_df$Year <- as.numeric(pubs_df$Year)
  pubs_df$Year[is.na(pubs_df$Year)] <- 0
  
  # Retrieve correct DOIs if missing or malformed
  # pubs_df$DOI <- sapply(1:nrow(pubs_df), function(i) {
  #   if (pubs_df$DOI[i] == "" || !grepl("^10\\.", pubs_df$DOI[i])) {
  #     correct_doi <- get_correct_doi(pubs_df$Title[i], pubs_df$Authors[i], pubs_df$Year[i])
  #     return(ifelse(is.na(correct_doi), "", correct_doi))
  #   }
  #   return(pubs_df$DOI[i])
  # })
  pubs_df$DOI <- sapply(1:nrow(pubs_df), function(i) {
    doi_value <- pubs_df$DOI[i]
    
    # Ensure DOI is not NULL, NA, or empty string before checking its format
    if (is.null(doi_value) || is.na(doi_value) || doi_value == "" || !grepl("^10\\.", doi_value)) {
      correct_doi <- get_correct_doi(pubs_df$Title[i], pubs_df$Authors[i], pubs_df$Year[i])
      return(ifelse(is.null(correct_doi) || is.na(correct_doi), "", correct_doi))
    }
    
    return(doi_value)
  }, USE.NAMES = FALSE)
  
  
  # Ensure all necessary columns exist
  if (!"Journal" %in% colnames(pubs_df)) pubs_df$Journal <- ""
  if (!"Volume" %in% colnames(pubs_df)) pubs_df$Volume <- ""
  if (!"Issue" %in% colnames(pubs_df)) pubs_df$Issue <- ""
  if (!"Authors" %in% colnames(pubs_df)) pubs_df$Authors <- ""
  
  # Clean Authors column for Google Scholar data
  pubs_df$Authors <- clean_google_scholar_authors(pubs_df$Authors)
  
  # Convert DOIs to clickable links
  pubs_df$DOI <- ifelse(pubs_df$DOI != "", paste0("<a href='https://doi.org/", pubs_df$DOI, "' target='_blank'>[DOI]</a>"), "")
  
  # Sort by Year (newest first)
  pubs_df <- pubs_df %>% arrange(desc(Year))
  
  # Unique years for collapsible sections
  unique_years <- sort(unique(pubs_df$Year), decreasing = TRUE)
  latest_years <- unique_years[1:min(3, length(unique_years))]  # First 3 years remain open
  
  # Start HTML output
  cat("<html><head>")
  cat("<script>
          function toggleYear(year) {
            var elem = document.getElementById('year_' + year);
            if (elem.style.display === 'none') {
              elem.style.display = 'block';
            } else {
              elem.style.display = 'none';
            }
          }
        </script>")
  cat("<style>
        body { font-family: Arial, sans-serif; margin: 20px; line-height: 1.5; }
        .year-header { cursor: pointer; color: #0044cc; font-size: 20px; margin-top: 15px; font-weight: bold; }
        .year-header:hover { text-decoration: underline; }
        .publication-list { margin-left: 20px; padding: 10px; border-left: 3px solid #0044cc; }
        .publication { margin-bottom: 10px; padding-bottom: 5px; border-bottom: 1px solid #ccc; }
        .hidden { display: none; }
      </style>")
  cat("</head><body>")
  
  # Generate collapsible sections per year
  for (year in unique_years) {
    if (year == 0) { year_display <- "Unknown Year" } else { year_display <- as.character(year) }
    
    # Determine if the section should be open or closed
    display_style <- ifelse(year %in% latest_years, "block", "none")
    
    # Header
    cat(paste0("<h2 class='year-header' onclick='toggleYear(", year, ")'>", year_display, " ▼</h2>"))
    cat(paste0("<div id='year_", year, "' class='publication-list' style='display:", display_style, ";'>"))
    
    # Filter publications of the current year
    year_pubs <- pubs_df %>% filter(Year == year)
    pubs_df$DOI <- ifelse(pubs_df$DOI != "", paste0( pubs_df$DOI), "")
    
    for (i in seq_len(nrow(year_pubs))) {
      if (!is.na(year_pubs$Title[i]) && year_pubs$Title[i] != "") {
        cat("<div class='publication'>")
        cat(paste0("<p><strong>", i, ". ", year_pubs$Title[i], "</strong><br>"))
        cat(paste0(year_pubs$Authors[i], "<br>"))
        cat(paste0("*", year_pubs$Journal[i], "*", ", Vol. ", year_pubs$Volume[i], ", Issue ", year_pubs$Issue[i], " (", year_display, ")<br>"))
        if (!is.na(year_pubs$DOI[i]) && year_pubs$DOI[i] != "") {
          cat(paste0(year_pubs$DOI[i]))
        }
        cat("</p></div>")
      }
    }
    
    cat("</div>")  # Close year div
  }
  
  cat("</body></html>")
}