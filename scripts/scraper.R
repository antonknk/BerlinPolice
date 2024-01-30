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
scrape_polizei <- function(archive_url = 'https://www.berlin.de/polizei/polizeimeldungen/archiv/',
                           years){
  # create baseurls per year
  baseurls <- paste0(archive, years)
  
  all_pages <- sapply(baseurls, get_all_pages, simplify = T, USE.NAMES = F) 
  
  all_article_urls <- sapply(unlist(all_pages), function(page){
    get_article_urls(page)
    
    Sys.sleep(2)
  })
  return(all_article_urls)
}

scrape_polizei()

t <- sapply(c('https://www.berlin.de/polizei/polizeimeldungen/archiv/2022', 
         'https://www.berlin.de/polizei/polizeimeldungen/archiv/2023'), get_all_pages)

sapply(unname(unlist(t))[1:4], print)

all_article_urls <- sapply(sample(unname(unlist(t)), 2), function(page){
  result <- get_article_urls(page)
  
  Sys.sleep(2)
  
  return(result)
})

as.list(all_article_urls)
