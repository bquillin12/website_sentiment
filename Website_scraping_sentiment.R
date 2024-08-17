###############################################################################
####################### Webscraping and Sentiment Analysis ####################
###############################################################################

# Load necessary libraries
library(rvest)
library(httr)
library(tidyverse)
library(tidytext)
library(textdata)

# Function to get all links
get_all_links <- function(base_url) {
  page <- read_html(base_url)
  links <- page %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    na.omit() %>%
    unique()
  links <- ifelse(startsWith(links, "/"), paste0(base_url, links), links)
  return(links)
}

# Base URL of the website to scrape
base_url <- ""  # Add the base URL here

# Extract all URLs from the base URL
all_links <- get_all_links(base_url)

# Function to get page content
get_page_content <- function(url) {
  tryCatch({
    page <- read_html(url)
    page_text <- page %>% html_text(trim = TRUE)
    return(page_text)
  }, error = function(e) {
    message(paste("Error in URL:", url, " - ", e$message))
    return(NA)
  })
}

# Fetch content from all URLs
website_data <- data.frame(
  URL = all_links,
  Content = sapply(all_links, get_page_content),
  stringsAsFactors = FALSE
)

# Filter out rows with missing content
website_data <- website_data %>% filter(!is.na(Content))

# Clean the text content
clean_text <- function(text) {
  text <- gsub("<.*?>", "", text)  # Remove HTML tags
  text <- gsub("[^[:alnum:][:space:]]", "", text)  # Remove special characters
  text <- tolower(text)  # Convert to lowercase
  return(text)
}

website_data <- website_data %>%
  mutate(Cleaned_Content = clean_text(Content))

# Tokenize the text
tokens <- website_data %>%
  unnest_tokens(word, Cleaned_Content)

# Perform tone analysis using the NRC lexicon
tone_analysis <- tokens %>%
  inner_join(get_sentiments("nrc")) %>%
  count(URL, sentiment) %>%
  spread(sentiment, n, fill = 0)

# Merge tone analysis results back to website_data
website_data <- website_data %>%
  left_join(tone_analysis, by = "URL")


#############

# Ensure correct total URL count
total_urls <- nrow(website_data)
print(paste("Total number of URLs:", total_urls))

# Verify tone counts
tone_counts <- website_data %>%
  select(URL, joy:trust) %>%
  gather(tone, count, joy:trust) %>%
  group_by(tone) %>%
  summarize(total_count = sum(count, na.rm = TRUE))

# Calculate the sum of all tone counts
total_tone_counts <- sum(tone_counts$total_count)

# Calculate percentages based on the total tone counts
tone_data <- tone_counts %>%
  mutate(percentage = (total_count / total_tone_counts) * 100)

# Print to verify calculations
print(tone_data)

# Reorder 'tone' based on 'percentage' in descending order
tone_data <- tone_data %>%
  mutate(tone = factor(tone, levels = tone[order(-percentage)]))

# Plotting with tones in descending order of percentage
sentiment_plot <- ggplot(tone_data, aes(x = tone, y = percentage, fill = tone)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Total Tone Counts by Tone Type", x = "Tone", y = "Percentage of Total Tone Counts") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 1)) +  # Format y-axis as percentages
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()
coord_flip()  # Flip coordinates for better readability if needed

print(sentiment_plot)


