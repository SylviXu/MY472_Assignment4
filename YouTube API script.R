library(httr)
library(jsonlite)

# Set up my YouTube data API key.
readRenviron("E:/文件/LSE/MY472/youtube api.env")
youtube_key <- Sys.getenv("KEY")

# Create a function to fetch YouTube channel ID
search_youtube_channel <- function(artist_name, api_key) {
  base_url <- "https://www.googleapis.com/youtube/v3/search"
  query <- list(part = "snippet", q = artist_name, type = "channel", key = api_key)
  
  response <- GET(url = base_url, query = query)
  data <- content(response, "parsed")
  
  if (length(data$items) > 0) {
    for (item in data$items) {
      channel_title <- item$snippet$title
      
      # Enhanced title matching logic
      if (tolower(channel_title) == tolower(artist_name) || 
          grepl(paste0("^", artist_name, "$"), channel_title, ignore.case = TRUE)) {
        # Exclude common non-official channel keywords
        if (!grepl("topic|vevo", channel_title, ignore.case = TRUE)) {
          return(item$snippet$channelId)
        }
      }
    }
  }
  
  return(NA)  # Return NA if no official channel is found
}

# Get the channel ids by searching artists' names.
channel_ids2 <- lapply(data$Name, search_youtube_channel, youtube_key)
ids2 <- unlist(channel_ids2)
yt_channel2 <- data.frame(names = data$Name, ids = ids2)
# Data washing.
yt_channel2 <- yt_channel2 %>%
  filter(!(yt_channel2$names %in% c("James Brown", "Bo Diddley", "Jerry Lee Lewis", "Fats Domino", "The Who", "Jogn Lennon", "Simon and Garfunkel", "Sly and the Faminly Stone", "Public Enemy", "The Byrds", "Run-DMC", "Queen", "The Sex Pistols", "Phil Spector", "CREAM", "Jackie Wilson", "Beastie Boys", "The Stooges", "The Four Tops", "Eminen", "Gram Parsons", "The Yard Birds", "Carlos Santana", "Tom Petty", "Guns N' Roses", "Diana Ross and the Supremes", "Carl Perkins")))%>%
  na.omit()
# Non-return sampling.
sampled_rows <- sample(nrow(yt_channel2), 30, replace = FALSE)
sampled_channel <- yt_channel2[sampled_rows, ]

write.csv(sampled_channel, "sampled_channel", row.names = FALSE)
# sampled_channel <- read.csv("sampled_channel")

# Function to get video statistics
get_video_statistics <- function(video_id, api_key) {
  stats_base_url <- "https://www.googleapis.com/youtube/v3/videos"
  stats_params <- list(
    part = "statistics",
    id = video_id,
    key = api_key
  )
  stats_response <- GET(url = stats_base_url, query = stats_params)
  stats_content <- fromJSON(rawToChar(stats_response$content), flatten = TRUE)
  
  if (length(stats_content$items) == 0) {
    return(list(
      ViewCount = NA,
      CommentCount = NA,
      LikeCount = NA,
      FavoriteCount = NA
    ))
  }
  
  # Ensure that each statistic is available, else NA
  view_count <- ifelse(!is.null(stats_content$items$statistics.viewCount), stats_content$items$statistics.viewCount, NA)
  comment_count <- ifelse(!is.null(stats_content$items$statistics.commentCount), stats_content$items$statistics.commentCount, NA)
  like_count <- ifelse(!is.null(stats_content$items$statistics.likeCount), stats_content$items$statistics.likeCount, NA)
  fav_count <- ifelse(!is.null(stats_content$items$statistics.favoriteCount), stats_content$items$statistics.favoriteCount, NA)
  
  video_stats <- data.frame(Like_Count = like_count,
                            View_Count = view_count,
                            Comment_Count = comment_count,
                            Fav_Count = fav_count)
  
  return(video_stats)
}

# Function to get the latest 5 videos and their statistics
get_latest_videos <- function(channel_id, api_key) {
  base_url <- "https://www.googleapis.com/youtube/v3/search"
  params <- list(
    part = "snippet",
    channelId = channel_id,
    maxResults = 1,
    order = "date",
    type = "video",
    key = api_key
  )
  response <- GET(url = base_url, query = params)
  content <- fromJSON(rawToChar(response$content), flatten = TRUE)
  
  if (!"items" %in% names(content)) {
    stop("No items found in API response.")
  }
  
  video_details_list <- list()
  for (i in 1:1) {
    video_id <- content$items$id.videoId
    video_title <- content$items$snippet.title
    statistics <- get_video_statistics(video_id[i], api_key)
    
    video_details <- data.frame(
      Title = video_title,
      ViewCount = statistics$View_Count,
      CommentCount = statistics$Comment_Count,
      LikeCount = statistics$Like_Count,
      FavoriteCount = statistics$Fav_Count,
      stringsAsFactors = FALSE
    )
    
    video_details_list[[i]] <- video_details
  }
  
  return(do.call(rbind, video_details))
}

video_data <- data.frame()
# Fetch the latest 5 videos for each artist
for (i in 1:nrow(sampled_channel)) {
  channel_id <- sampled_channel$ids[i]
  videos <- get_latest_videos(channel_id, youtube_key)
  # Process the videos data as needed
  video_data <- rbind(video_data, videos)
}