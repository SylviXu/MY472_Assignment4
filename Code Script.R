# This is a code script for assignment 4.
# Install and load packages.
#install.packages('spotifyr')
library(spotifyr)
library(tidyverse)
library(knitr)
library(httr)
library(RSelenium)
library(rvest)
library(xml2)
library(jsonlite)
#set_config(use_proxy(url = "Proxy_Add_Here", port = 8090))

## Scrape the 100 greatest artists ranked by Rolling Stone Magazine.==============================

# Set up the driver and navigate to Rolling Stone's ranking page.
rD <- rsDriver(browser=c("firefox"), verbose = F, port = 4567L, chromever = NULL) 
driver <- rD[["client"]]
website <- "https://www.rollingstone.com/music/music-lists/100-greatest-artists-147446/"
driver$navigate(website)

# Create a scraping function to collect all the rankings and names of the artists presented on the page.
rank_scrape <- function() {
  src <- driver$getPageSource() # Get the source code of the page.
  
  result_html <- read_html(src[[1]]) # Get the html.
  
  rank_table <- data.frame() # Create a blank list to storage final data.
  
  # Get the rank data from the page.
  rank <- src %>%
    html_nodes("span.c-gallery-vertical-album__number") %>%
    html_text() %>%
    as.numeric()
  
  # Get the names of the artists.
  names <- src %>%
    html_nodes("h2.c-gallery-vertical-album__title") %>%
    html_text()
  
  # Create new rows in the final table.
  rank_table$Rank <- rank
  rank_table$Name <- names
  
  # Return the result of the function.
  return(rank_table)
}

# Since there are only 50 artists on one single page, we need to scrape the 1-50 and 51-100 artists separately and then combine them into one table.
# First, scrape the 51-100 artists' data.
rank_51_100 <- rank_scrape()
# Then find the "load more" button and click, navigating to next page.
load_button <- driver$findElement(using = "xpath", value = "/html/body/div[4]/main/div[2]/div[1]/div/article/div[3]/div[2]/div[2]")
load_button$clickElement()
# Scrape the 1-50 artists' data.
rank_1_50 <- rank_scrape()

## Get artist details from Spotify API. ================================================
# Set up my client id and secret from a local file.
readRenviron("E:/文件/LSE/MY472/spotify id&secret.env")
id <- Sys.getenv("id")
secret <- Sys.getenv("secret")
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
access_token <- get_spotify_access_token()

# Function to get all artists' Spotify ids.
get_spotify_artist_ids <- function(artist_names) {
  artist_ids <- sapply(artist_names, function(name) {
    artist <- spotifyr::get_artist(name)
    if (nrow(artist) > 0) {
      return(artist$id[1])
    } else {
      return(NA)
    }
  })
  return(artist_ids)
}

# Function to Get Multiple Artists' Details.
get_multiple_artists_details <- function(artist_ids, token) {
  valid_ids <- paste(na.omit(artist_ids), collapse = ",")
  url <- paste0("https://api.spotify.com/v1/artists/?ids=", valid_ids)
  response <- httr::GET(url, add_headers(`Authorization` = paste("Bearer", token)))
  details <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  return(details$artists)
}

# Write a function to update my data.
update_data <- function(rank){
  rank$spotify_id <- get_spotify_artist_ids(rank$Name)
  # Fetch the details
  artist_details <- get_multiple_artists_details(rank$spotify_id, access_token)
  
  # Assuming artist_details_1_50 is a list of artist details
  # Create a data frame from the details
  details_df <- data.frame(
    spotify_id = sapply(artist_details, function(x) x$id),
    followers = sapply(artist_details, function(x) x$followers$total),
    popularity = sapply(artist_details, function(x) x$popularity)
  )
  # Merge the new details with the original data frame
  rank_update <- merge(rank, details_df, by = "spotify_id")
  return(rank_update)
}

rank_1_50 <- update_data(rank_1_50)
rank_51_100 <- update_data(rank_51_100)

# Finally, combine them into one table.
greatest_100_artists <- bind_rows(rank_1_50, rank_51_100)

print(rank_51_100)
print(rank_1_50)
print(greatest_100_artists)
# ========================================================================================

