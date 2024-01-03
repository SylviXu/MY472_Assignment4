# Load the packages.
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
  # Handle the NA values.
  if (length(data$items) > 0) {
    return(data$items[[1]]$snippet$channelId)
  } else {
    return(NA)
  }
}

# Get the channel ids by searching artists' names.
channel_ids <- lapply(data$Name, search_youtube_channel, youtube_key)
ids <- unlist(channel_ids)
yt_channel <- data.frame(names = data$Name, ids = ids)

# Non-return sampling.
# Generate a sampling index (start at 1 and draw every 4).
sampling_indices <- seq(1, nrow(yt_channel), by=5)
# Use the index to select from the sample.
selected_samples <- yt_channel[sampling_indices, ]

# Store the sampled table as a local file. Because the YouTube API has a ration for tokens everyday.
write.csv(selected_samples, "selected_samples", row.names = FALSE)
sampled_channel <- read.csv("selected_samples")

# Write a function to get video statistics.
get_video_statistics <- function(video_id, api_key) {
  stats_base_url <- "https://www.googleapis.com/youtube/v3/videos"
  stats_params <- list(
    part = "statistics",
    id = video_id,
    key = api_key
  )
  stats_response <- GET(url = stats_base_url, query = stats_params)
  stats_content <- fromJSON(rawToChar(stats_response$content), flatten = TRUE)
  # Handle potential NA values.
  if (length(stats_content$items) == 0) {
    return(list(
      ViewCount = NA,
      CommentCount = NA,
      LikeCount = NA,
      FavoriteCount = NA
    ))
  }
  
  # Ensure that each statistic is available, else NA. And collect the view counts, comment counts, like counts and favourite counts.
  view_count <- ifelse(!is.null(stats_content$items$statistics.viewCount), stats_content$items$statistics.viewCount, NA)
  comment_count <- ifelse(!is.null(stats_content$items$statistics.commentCount), stats_content$items$statistics.commentCount, NA)
  like_count <- ifelse(!is.null(stats_content$items$statistics.likeCount), stats_content$items$statistics.likeCount, NA)
  fav_count <- ifelse(!is.null(stats_content$items$statistics.favoriteCount), stats_content$items$statistics.favoriteCount, NA)
  
  # Structure the retrieved data as a data frame.
  video_stats <- data.frame(Like_Count = like_count,
                            View_Count = view_count,
                            Comment_Count = comment_count,
                            Fav_Count = fav_count)
  
  return(video_stats)
}

# Write a function to get the latest videos and their statistics.
get_latest_videos <- function(channel_id, api_key) {
  base_url <- "https://www.googleapis.com/youtube/v3/search"
  # Set the parameters.
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
  
  # Handle potential errors.
  if (!"items" %in% names(content)) {
    stop("No items found in API response.")
  }
  
  # Get the video's id, title, and statistics.
  video_id <- content$items$id.videoId
  video_title <- content$items$snippet.title
  statistics <- get_video_statistics(video_id[1], api_key)
  
  # Create a new data frame as final output.
  video_details <- data.frame(
    Name = sampled_channel$names[which(sampled_channel$ids == channel_id)],
    VideoTitle = video_title,
    VideoID = video_id,
    ViewCount = statistics$View_Count,
    CommentCount = statistics$Comment_Count,
    LikeCount = statistics$Like_Count,
    FavoriteCount = statistics$Fav_Count,
    stringsAsFactors = FALSE
    )
  
  return(do.call(cbind, video_details))
}

# Create a blank data frame for storage.
video_data2 <- data.frame()
# Fetch the latest video for each artist
for (i in 1:nrow(sampled_channel)) {
  channel_id <- sampled_channel$ids[i]
  videos2 <- get_latest_videos(channel_id, youtube_key)
  # Process the videos data.
  video_data2 <- rbind(video_data2, videos2)
}

# Create a function to get the first ten comments of each video.
get_comments <- function(video_id, api_key) {
  url <- paste0("https://www.googleapis.com/youtube/v3/commentThreads?key=", api_key,
                "&textFormat=plainText&part=snippet&videoId=", video_id,
                "&maxResults=20")  # Request for the top 20 comments.
  response <- GET(url)
  content <- fromJSON(rawToChar(response$content))
  
  # Extract the comments.
  comments <- content$items$snippet$topLevelComment$snippet$textDisplay
  combined_comments <- paste(comments, collapse = "\n")
  return(combined_comments)
}

# Create a list for storage.
Comments <- list()
# Get the top few comments for each video.
for (i in 1:nrow(video_data2)) {
  comments <- get_comments(video_data2$VideoID[i], youtube_key)
  Comments[i] <- comments
}
video_data2$Comments <- Comments

# View the final data.
print(video_data2)

#====================================================
# Create a list for storage.
Comments2 <- list()
# Get the top few comments for each video.
for (i in 1:nrow(video_data2)) {
  comments2 <- get_comments(video_data2$VideoID[i], youtube_key)
  Comments2[i] <- comments2
}
video_data2$Comments <- Comments

#install.packages("tm")
#install.packages("wordcloud")
library(tm)
library(wordcloud)
# Prepare the Text Data
comments_text <- paste(Comments2, collapse=" ")
# Create a Text Corpus and Clean the Data
corpus <- Corpus(VectorSource(comments_text))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english")) # adjust language as necessary

wordcloud(corpus, scale=c(5,0.6), max.words=80, random.order=FALSE, rot.per=0, colors=brewer.pal(8, "Set1"))
# =============================================================
install.packages("highcharter")
library(highcharter)
transformed_data <- video_data2 %>%
  pivot_longer(cols = c(ViewCount, LikeCount, CommentCount),
               names_to = "Statistics")
transformed_data$value <- as.numeric(transformed_data$value)
hchart(transformed_data, "bar", hcaes(x = Name, y = value, group = Statistics))
