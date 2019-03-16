################################
# Scrape English Profile Data  #
################################

# To a large extent, I just adapted
# a script from this tutorial:
# https://www.datacamp.com/community/tutorials/r-web-scraping-rvest

library(tidyverse)  
library(rvest)    
library(stringr)

# Save the URL for the english profile site as a variable
# Looks like prepending the supplied username and
# password to this url worked just fine, see post:
# https://stackoverflow.com/questions/50894693/using-rvest-to-scrape-from-password-protected-website
url <- 'http://englishprofile:vocabulary@vocabulary.englishprofile.org/dictionary/search/us/?pageSize=100&wl=&q=&c=-1&c=-1&c=-1&c=-1&c=-1&c=-1&c=-1'

# Function to get the number of total pages
# That the user would have to click through
get_last_page <- function(html){
  
  pages_data <- html %>% 
    # The '.' indicates the class
    # Looks like if I just specify underline
    # Then it matches the whitespace in the class name
    html_nodes('.underline') %>% 
    # Extract the raw text as a list
    html_text()                   
  
  # The second to last of the buttons is the one
  pages_data[(length(pages_data)-1)] %>%            
    # Take the raw string
    unname() %>%                                     
    # Convert to number
    as.numeric()                                     
}

# Read in the html of the first page
first_page <- read_html(url)

# Use the first page to get the last page
latest_page_number <- get_last_page(first_page)

# Create a list of page urls that we are going to scrape
list_of_pages <- str_c(url, '&p=', 1:latest_page_number)

# Create a fucntion that scrapes the information we need from each page
get_entry <- function(html){
  html %>% 
    # The relevant tag
    html_nodes('.arl1') %>%
    map_df(~{
      # There is always a base so we don't need to test this
      base <- .x %>% html_nodes('.base') %>% html_text()
      pos <- .x %>% html_nodes('.pos') %>% html_text()
      gw <- .x %>% html_nodes('.gw') %>% html_text()
      # Here I'm using a more complicated CSS selector
      # That is, get get the span that starts with freq- because
      freq <- .x %>% html_nodes("span[class*='freq-']") %>% html_text()
      
      # If there is no pos, then set it's a lexical chunk
      # If you don't include the next two if statements, then the function doesn't return anything for this row
      if (is_empty(pos)) {
        pos <- "lexical_chunk"
      }
      
      # Sometimes there is no subsense so set this field to none
      if (is_empty(gw)) {
        gw <- "none"
      }
      
      # combine all of these fields into a dataframe
      data_frame(base, pos, gw, freq)
      })
  
}

# Create a helper function to run on each url
get_data_from_url <- function(url) {
  html <- read_html(url)
  get_entry(html)
}

# Apply helper function to each url save as final data frame
all <- list_of_pages %>%
  map(get_data_from_url) %>%
  bind_rows()

write_csv(all,"eprofile_lex.csv")
