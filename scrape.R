install.packages("rvest")
install.packages("dplyr")
library(rvest)
library(dplyr)

# Vector of event slugs
event_slugs <- c("rome-1960", "tokyo-1964", "tel-aviv-1968", 
                 "heidelberg-1972", "toronto-1976", "arnhem-1980",
                 "stoke-mandeville-new-york-1984", "seoul-1988", 
                 "barcelona-1992","atlanta-1996","sydney-2000",
                 "athens-2004","beijing-2008","london-2012",
                 "rio-2016","tokyo-2020")

# Base URL components
base_url <- "https://www.paralympic.org/"
suffix <- "/medalstandings"

# Initialize an empty list to store the results
results_urls <- list()

# Loop through each event
for(event in event_slugs) {
  
  # Construct the main page URL
  event_url <- paste0(base_url, event, "/results")
  
  # Read the page content
  page <- read_html(event_url)
  
  # Extract sports overview section
  sports_section <- page %>%
    html_node("div.sports") %>%
    html_nodes("div.sport-element")
  
  # Extract the href attribute from each sport-element and build full URL
  sport_urls <- sports_section %>%
    html_node("a") %>%
    html_attr("href") %>%
    na.omit() %>%
    paste0(base_url, ., suffix)
  
  # Store the URLs in the list, named by event
  results_urls[[event]] <- sport_urls
}

# Convert list to a data frame for easier viewing
results_df <- bind_rows(lapply(names(results_urls), function(event) {
  data.frame(Event = event, URL = results_urls[[event]], stringsAsFactors = FALSE)
}), .id = "ID")

### Manually take out all volleyball, wheelchair tennis, and cycling events

results_df <- results_df %>%
  filter(!str_detect(URL, "volleyball") &
           !str_detect(URL, "cycling") &
           !str_detect(URL, "wheelchair-tennis") &
           !str_detect(URL, "wheelchair-rugby")  &
           !str_detect(URL, "canoeing")  &
           !str_detect(URL, "sailing"))


# Remove the specific row from results_df
#results_df <- results_df %>%
#  filter(URL != "https://www.paralympic.org//toronto-1976/results/volleyball/medalstandings" & 
#           URL != "https://www.paralympic.org//arnhem-1980/results/volleyball/medalstandings")



all_data <- data.frame(Event = character(),
                       Sport = character(),
                       Rank = character(),
                       Country = character(),
                       NPC = character(),
                       Gold = character(),
                       Silver = character(),
                       Bronze = character(),
                       Total = character(),
                       stringsAsFactors = FALSE)

# Loop through each row in the results_df
for(i in 1:nrow(results_df)) {
  
  # Get the event name and the sport URL
  event_name <- results_df$Event[i]
  sport_url <- results_df$URL[i]
  
  # Read the sport's medal standings page
  sport_page <- tryCatch({
    read_html(sport_url)
  }, error = function(e) {
    warning(paste("Failed to read URL:", sport_url))
    next
  })
  
  # Check if the "sports" div exists
  sports_div <- sport_page %>% html_node("div.sports")
  
  # If the "sports" div is missing, skip to the next iteration
  if (is.null(sports_div)) {
    warning(paste("No sports data found for URL:", sport_url))
    next
  }
  
  # Extract the table element within the "sports" div
  table_element <- sports_div %>% html_node("table")
  
  # If the table element is missing, skip to the next iteration
  if (is.null(table_element)) {
    warning(paste("No table element found in sports div for URL:", sport_url))
    next
  }
  
  # Extract the table header (column names)
  table_header <- table_element %>%
    html_node("thead") %>%
    html_node("tr") %>%
    html_nodes("th") %>%
    html_text(trim = TRUE)
  
  # Ensure there are no missing or empty column names
  table_header <- ifelse(table_header == "", NA, table_header)
  
  # Extract the table body (medal standings data)
  table_body <- table_element %>%
    html_node("tbody") %>%
    html_nodes("tr")
  
  # If the table body is empty or missing, skip to the next iteration
  if (length(table_body) == 0) {
    warning(paste("No table data found for URL:", sport_url))
    next
  }
  
  # Extract data from each row in the table body
  table_data <- table_body %>%
    html_nodes("td") %>%
    html_text(trim = TRUE)
  
  # Number of columns in the table
  num_columns <- length(table_header)
  
  # Reshape table_data to a matrix format with appropriate number of columns
  table_matrix <- matrix(table_data, ncol = num_columns, byrow = TRUE)
  
  # Convert matrix to data frame
  sport_df <- as.data.frame(table_matrix, stringsAsFactors = FALSE)
  
  # Rename columns based on the header
  if(any(is.na(table_header))) {
    table_header[is.na(table_header)] <- paste0("Column", seq_along(which(is.na(table_header))))
  }
  
  colnames(sport_df) <- table_header
  
  # Extract the second-to-last segment from the sport_url
  sport_name <- basename(dirname(sport_url))
  
  # Add the event and sport information
  sport_df <- sport_df %>%
    mutate(Event = event_name,
           Sport = sport_name) %>%
    select(Event, Sport, everything())
  
  # Append the sport_df to all_data
  all_data <- bind_rows(all_data, sport_df)
}