# This is a code script for assignment 4.

# Install and load packages.
install.packages('spotifyr')
library(spotifyr)
library(tidyverse)
library(knitr)
library(httr)
library(RSelenium)
library(rvest)
library(xml2)
#set_config(use_proxy(url = "Proxy_Add_Here", port = 8090))

## Scrape the 100 greatest artists ranked by Rolling Stone Magazine.

# Set up the driver and navigate to Rolling Stone's ranking page.
rD <- rsDriver(browser=c("firefox"), verbose = F, port = 4567L, chromever = NULL) 
driver <- rD[["client"]]
url <- 'https://www.rollingstone.com/music/music-lists/100-greatest-artists-147446/'
driver$navigate(url)

# Create a scraping function to collect all the rankings and names of the artists presented on the page.
rank_scrape <- function() {
  src <- driver$getPageSource() # Get the source code of the page.
  
  result_html <- read_html(src[[1]]) # Get the html.
  
  rank_table <- list() # Create a blank list to storage final data.
  
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
# Finally, combine them into one table.
greatest_100_artists <- bind_rows(rank_1_50, rank_51_100)


# Set up my client id and secret from a local file.
readRenviron("E:/文件/LSE/MY472/spotify id&secret.env")
id <- Sys.getenv("id")
secret <- Sys.getenv("secret")
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
access_token <- get_spotify_access_token()

