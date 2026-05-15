library(tidyverse)
library(rvest)

my_boxoffice <- function(my_date = Sys.Date()-7) {
  
  dates <- my_date %>%
    str_replace_all("-", "/")
  
  my_xml <- "tbody .data , .chart-desktop a"
  my_url <- glue::glue("https://www.the-numbers.com/box-office-chart/daily/{dates}")
  #my_url <- "https://www.the-numbers.com/box-office-chart/daily/2026/04/14"
  z <- read_html(httr::GET(my_url, httr::timeout(30)))
  z1 <- html_elements(z, my_xml)
  z2 <- html_text2(z1)
  movie <- z2[seq(3, 93, 10)]
  weekly <- z2[seq(4, 94, 10)] |> 
    str_remove_all("\\$") |> 
    str_remove_all(",") |> 
    as.numeric()
  gross <- z2[seq(9, 99, 10)] |> 
    str_remove_all("\\$") |> 
    str_remove_all(",") |> 
    as.numeric()
  tibble(movie, weekly, gross)
}