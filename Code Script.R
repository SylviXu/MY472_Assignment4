# This is a code script for assignment 4.

# Load packages
library(rvest)
library(xml2)
library(dplyr)
library(spotifyr)
library(jsonlite)
library(httr)
library(tidyverse)

## Step 1: scrape the 100 greates artists data from Rolling Stone webpage.======

# Scrape the first page's 50 artists.
# Navigate to the webpage.
website <- "https://www.rollingstone.com/music/music-lists/100-greatest-artists-147446/"

# Read the page's html.
res <- read_html(website)

# Get the 51-100 artists' data.
rank_51_100 <- res %>%
  html_nodes("h2") %>%
  html_text() %>%
  as.data.frame() %>%
  head(50)
colnames(rank_51_100) <- "Name" #Rename the column.
rank_51_100$Rank <- 100:51 #Add ranking numbers.

# Navigate to the next page which contains 1-50 artists' data.
website2 <- "https://www.rollingstone.com/music/music-lists/100-greatest-artists-147446/the-band-2-88489/"
res2 <- read_html(website2)
rank_1_50 <- res2 %>%
  html_nodes("h2") %>%
  html_text() %>%
  as.data.frame() %>%
  head(50)
colnames(rank_1_50) <- "Name" #Rename the column.
rank_1_50$Rank <- 50:1 #Add ranking numbers.

# Combine two data frames into one single table.
greatest_100_artists <- bind_rows(rank_51_100, rank_1_50) %>%
  arrange(Rank)

## Step 2: retrieve more details for analysis from Spotify API.================

# Set up my Spotify client id and secret by reading from a local ".env" file.
readRenviron("E:/文件/LSE/MY472/spotify id&secret.env")
id <- Sys.getenv("id")
secret <- Sys.getenv("secret")
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)

# Set the access token.
access_token <- get_spotify_access_token()

# Create a function to retrieve an artist's spotify id from api.
get_spotify_id <- function(artist_name) {
  # To avoid timeout issues, I create a retry mechanism system here, which contains a loop combined with tryCatch and Sys.sleep to make repeated attempts at the API call.
  attempt <- 1 # Set the minimum number of retry attempts.
  max_attempts <- 5 # Set the maximum number of retry attempts.
  # Write a loop to retrieve data while avoiding timeout errors.
  while (attempt <= max_attempts) {
    try({
      # Set up the api endpoint.
      search_url <- paste0('https://api.spotify.com/v1/search?q=', URLencode(artist_name), '&type=artist&limit=1')
      # Get response.
      response <- GET(search_url, timeout(30), add_headers(Authorization = paste('Bearer', access_token)))
      if (status_code(response) == 200) {
        search_results <- content(response, as = "parsed", type = "application/json")
        # Here I add an if sentence to deal with potential NA values.
        if (length(search_results$artists$items) > 0) {
          return(search_results$artists$items[[1]]$id)
        } else {
          return(NA)
        }
      }
    }, silent = TRUE)
    attempt <- attempt + 1
    Sys.sleep(5)  # pause for 5 seconds before retrying
  }
  return(NA)
}

# Improve the function into another function that retrieve a list of artists' spotify id.
get_ids <- function(rank) {
  for (i in 1:nrow(rank)) {
    name <- rank$Name[i]
    rank$id[i] <- get_spotify_id(name)
  }
  return(rank)
}

# Get all the artists' spotify ids and add them into my dataframe.
greatest_100_artists <- get_ids(greatest_100_artists)

# Retrieve followers and popularity index.====================================================

# Function to get an artist's followers and popularity index.
get_artist_details <- function(id, token) {
  # Set the url endpoint.
  url <- paste0("https://api.spotify.com/v1/artists/?ids=", id)
  
  # To avoid timeout issues mentioned before, here I create a retry mechanism as well.
  attempt <- 1 # Set the minimum number of retry attempts.
  max_attempts <- 5  # Set the maximum number of retry attempts.
  while (attempt <= max_attempts) {
    response <- tryCatch({
      # Get API response.
      GET(url, timeout(30), add_headers(`Authorization` = paste("Bearer", token)))
    }, error = function(e) { NULL })
    
    # Check if the request was successful
    if (!is.null(response) && status_code(response) == 200) {
      # Retrieve text details from a json file we got.
      details <- fromJSON(content(response, "text", encoding = "UTF-8"))
      # Extract the followers and popularity data.
      followers <- details$artists$followers$total
      popularity <- details$artists$popularity
      
      # Create a new data frame to storage the retrieved data.
      return(data.frame(followers = followers, popularity = popularity))
    } else {
      message(sprintf("Attempt %d failed. Retrying in %d seconds...", attempt, attempt))
      Sys.sleep(attempt)  # Exponential back-off
      attempt <- attempt + 1
    }
  }
  # If all attempts failed, output warning message and return NA values.
  warning("All attempts failed.")
  return(data.frame(followers = NA, popularity = NA))
}

# Mapping the artists dataframe to add followers and popularity details.
for (i in 1:nrow(greatest_100_artists)) {
  id <- greatest_100_artists$id[i]
  df <- get_artist_details(id, access_token)
  greatest_100_artists$followers[i] <- df$followers[1]
  greatest_100_artists$popularity[i] <- df$popularity[1]
}

# Review the first few rows of the updated dataframe.
head(greatest_100_artists)

# Write this data into my local storage for future usage.
write.csv(greatest_100_artists, "artists_spotify_data", row.names = FALSE)
