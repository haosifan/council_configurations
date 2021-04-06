
## -------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(readr)
library(diffdf)
library(rvest)


## ----f_wikitemplate-------------------------------------------------------------------------------------------------

f_wikicheck <- function(){
  url_wiki <- "https://en.wikipedia.org/wiki/Template:EU_governments"

  gov_name <- read_html(url_wiki) %>% 
    html_nodes(".hlist .flagicon+ a") %>% 
    html_text()

  link <- read_html(url_wiki) %>% 
    html_nodes(".hlist .flagicon+ a") %>% 
    html_attr("href") %>% paste0("https://en.wikipedia.org",.)

  cntry <- read_html(url_wiki) %>% 
    html_nodes(".hlist .thumbborder") %>% html_attr("alt")

  eu_govs <- bind_cols(cntry = cntry, gov_name = gov_name, link = link)

return(eu_govs)
}



## ----govcheck_wiki--------------------------------------------------------------------------------------------------
equal_govs <- all(f_wikicheck() == read_csv2("data/eu_governments_lv1_wiki.csv"))

if (equal_govs == FALSE){eu_govs <- read_csv2("data/eu_governments_lv1_wiki.csv")}


## ----ifelsedownloadwiki---------------------------------------------------------------------------------------------
if (equal_govs == TRUE) {
  message("No changes in European Governments")
} else {
  write_csv2(f_wikicheck(), file = "data/eu_governments_lv1_wiki.csv")
  message("Attention: Changes in European Governments - Downloaded new file")
}


## -------------------------------------------------------------------------------------------------------------------
if (equal_govs == TRUE) {
  message("No changes in European Governments")
} else {
  of <- eu_govs
  nf <- f_wikicheck()
  diffdf(of, nf, suppress_warnings = TRUE, file = paste0("log/log_", Sys.Date(),".txt"))
}

