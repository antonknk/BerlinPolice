# Scrape police reports

# SETUP ####
library(rvest)
library(tidyverse)

Baseurl <- 'https://www.berlin.de/polizei/polizeimeldungen/archiv/'


html_base <- read_html('https://www.berlin.de/polizei/polizeimeldungen/archiv/2023')

# retrieve number of pages to iterate over
get_all_pages <- function(base_page_url) {
  lastpage <- read_html(base_page_url) |> 
    html_element(
      "#layout-grid__area--maincontent > section > nav > ul > li.pager-item.last > a"
    ) |>
    html_attr('aria-label') |>
    str_remove('\\w*') |>
    str_squish()
  
  pages <- paste0(base_page_url, '/?page_at_1_0=', seq(1, as.numeric(lastpage), 1))
  
  return(pages)
}

get_all_pages('https://www.berlin.de/polizei/polizeimeldungen/archiv/2023')

# extract urls
get_article_urls <- function(base_page_url) {
  urls <- read_html(base_page_url) |>
    html_elements('#layout-grid__area--maincontent > section > ul > li > div:nth-child(2) > a') |>
    html_attr('href')
  return(urls)
}

example_article <- read_html('https://www.berlin.de/polizei/polizeimeldungen/pressemitteilung.767480.php')
html_element(example_article, '#layout-grid__area--herounit') |> 
  html_text2()

html_element(example_article, '#layout-grid__area--maincontent > p:nth-child(1)') |> 
  html_text2()

html_element(example_article, '#layout-grid__area--maincontent > section > div > div') |>  
  html_text2()




# Read Articles 
read_articles <- function(article_urls){
  article_html <- read_html(article_urls)
  
  title <- html_element(article_html, '#layout-grid__area--herounit') |> 
    html_text2()
  
  date <- html_element(article_html, '#layout-grid__area--maincontent > p:nth-child(1)') |> 
    html_text2()
  
  bezirk <- html_element(article_html, '#layout-grid__area--maincontent > p:nth-child(2)') |> 
    html_text2()
  
  number <- html_element(article_html, '#layout-grid__area--maincontent > section > div > div > p:first-child > strong') |> 
    html_text2()
  
  body_text <- html_element(article_html, '#layout-grid__area--maincontent > section > div > div') |> 
    html_text2()
  
  return(data.frame('title' = title, 
              'date' = date,
              'bezirk' = bezirk,
              'number' = number,
              'body_text' = body_text))
}




# Putting it all together 
years <- as.character(seq(2018,2024, 1))
archive_url = 'https://www.berlin.de/polizei/polizeimeldungen/archiv/'
baseurls <- paste0(archive_url, years)

counter = 1
all_pages <- sapply(baseurls, function(baseurl){
  all_pages <-  get_all_pages(base_page_url = baseurl)
  
  counter = counter +1
  if (counter %% 5 == 0){
    print('I will now sleep for 60 seconds')
    for (i  in 59:1) {
      Sys.sleep(1)
      print(paste('remaining seconds:', i))
    }
  }
  return(all_pages)
  })
  
all_pages <- unname(unlist(all_pages))

all_article_urls <- list()
for(page in 1:length(all_pages[1:10])){
  result[page] <- list(get_article_urls(t[page]))
  Sys.sleep(2)
  print('did a thing')
  
  if (page %% 5 == 0) {
    print('I will now sleep for 60 seconds')
    for (i  in 59:1) {
      Sys.sleep(1)
      print(paste('remaining seconds:', i))
    }
    
  }
}


 