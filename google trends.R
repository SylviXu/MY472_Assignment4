library(gtrendsR)
library(ggplot2)
library(dplyr)

# Assuming data$Name is your vector of 100 keywords
total_keywords <- length(data$Name)
batch_size <- 5
all_trends <- data.frame()  # Empty data frame to store all results

# Loop through keywords in batches
for (i in seq(1, total_keywords, by = batch_size)) {
  batch <- data$Name[i:min(i + batch_size - 1, total_keywords)]
  
  repeat {
    tryCatch({
      trends_data <- gtrends(batch, time = "today 12-m")
      break
    }, error = function(e) {
      if (grepl("429", e)) {
        Sys.sleep(30)  # Wait for 30 seconds before retry
      } else {
        stop(e)  # Handle other errors differently
      }
    })
  }
  
  interest_over_time <- trends_data$interest_over_time
  all_trends <- rbind(all_trends, interest_over_time)
  
  Sys.sleep(5)  # Delay between each batch
}



# 试验：用gtrends收集的interest_over_time数据表绘图
all_trends$hits <- as.numeric(all_trends$hits)
all_trends[is.na(all_trends)] <- 0
  
all_trends %>%
  ggplot()+
  geom_line(aes(x = date, y = hits, group = keyword, color = keyword))+
  scale_y_continuous(
    breaks = seq(0, 110, by = 25),
  )+
  labs(title = "Google Trends Interest Over Time", x = "Date", y = "Search hits") +
  theme_light()

