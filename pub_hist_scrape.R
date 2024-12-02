#Script to scrape google scholar results for historical trends

#load packages
library(magrittr)
library(tidyverse)
library(RSelenium)
library(rvest)

#open session (needs two attemps, god knows why)
shell('docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.1')
remDr <- remoteDriver(remoteServerAddr = "localhost",
                      port = 4445L,
                      browserName = "firefox")


Sys.sleep(1)
remDr$open()
Sys.sleep(1)

#define search term
search_term <- "social media morality" #"what ever you want in quotation marks"

#intergroup relations, cognitive consistency, social identity, leadership, group decision making, reasoning, moral resoning

#create empty dataframe
no_articles <- tibble(
  topic = character(),
  year = double(),
  articles = character()
)

#go to google scholar and enter search term
remDr$navigate("https://scholar.google.com/")
#remDr$navigate("https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&as_ylo=1991&as_yhi=1991&q=political+polarization&btnG=")
#remDr$getCurrentUrl()
remDr$findElement("id", "gs_hdr_tsi")$sendKeysToElement(list(search_term))
remDr$findElement("id", "gs_hdr_tsb")$clickElement()
remDr$screenshot(display = TRUE)


#Start loop here

for (i in 1990:2024) {

#define custom range
remDr$findElement("id", "gs_res_sb_yyc")$clickElement()
  
Sys.sleep(runif(1, 2, 4))

lower <- as.character(i)

remDr$findElement("id", "gs_as_ylo")$sendKeysToElement(list(key = "control", "a"))  # Select all text
remDr$findElement("id", "gs_as_ylo")$sendKeysToElement(list(key = "delete"))        # Delete selected text
remDr$findElement("id", "gs_as_ylo")$sendKeysToElement(list(lower))

Sys.sleep(runif(1, 2, 4))

remDr$findElement("id", "gs_as_ylo")$sendKeysToElement(list(key = "tab"))

Sys.sleep(runif(1, 2, 4))

upper <- as.character(i)
remDr$sendKeysToActiveElement(list(upper))

Sys.sleep(runif(1, 2, 4))

remDr$findElement("id", "gs_as_ylo")$sendKeysToElement(list(key = "return"))

Sys.sleep(runif(1, 2, 4))

#scrape number of articles
current <- remDr$getPageSource()[[1]]

temp <- read_html(current) %>%
  html_nodes(".gs_ab_mdw") %>% 
  html_text()

no_articles %<>% add_row(topic = search_term, year = i, articles = temp[2])


Sys.sleep(runif(1, 2, 4))
}



#Tidy data and write to csv
no_articles %<>%
  mutate(
   articles = str_extract(articles, "(?<=Ungefähr|About).*(?=Ergebnisse|results)") %>%
     str_squish() %>%
     str_remove_all("\\.|\\s|’|,")  %>%
     as.numeric()
  )# %>% view()
  

write.csv(no_articles, paste0(search_term, ".csv"), row.names =  FALSE)

#close the server
remDr$close()
rD$server$stop()

