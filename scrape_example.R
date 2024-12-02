#First attempts to scrape LinkedIn

#load packages
library(rvest)
library(tidyverse)
library(RSelenium)
library(httr)
library(magrittr)
library(lubridate)

#open session
shell('docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.1')
remDr <- remoteDriver(remoteServerAddr = "localhost",
                      port = 4445L,
                      browserName = "firefox")
#shell('docker run -d -p 4445:4444 selenium/standalone-chrome')
#remDr <- remoteDriver(remoteServerAddr = "localhost",
#                      port = 4445L,
#                      browserName = "chrome")

Sys.sleep(2)
remDr$open()
Sys.sleep(2)


engine <- readline(prompt="Please enter a lowercase b for scraping bing and a lowercase g for scraping google: ")

links <- tibble(links = character()) 

if (engine == "b") {

#Bing -----------------------------

remDr$navigate("https://www.bing.com")
remDr$findElement("name", "q")$sendKeysToElement(list('site:linkedin.com/in/ AND "Data Scientist" AND "UK"', key = "enter"))

for (i in 1:5) {
current <- remDr$getPageSource()[[1]]
links %<>% add_row(
  links = read_html(current) %>%
    html_nodes("h2 a") %>%
    html_attr("href")
)
remDr$findElement("css", ".sb_pagN")$clickElement()
Sys.sleep(runif(1, 1, 3))
}

write.csv(links, "links_bing.csv", row.names = FALSE)
}

if (engine == "g") {

#google ---------------------------------

remDr$navigate("https://www.google.com/")
remDr$findElement("name", "q")$sendKeysToElement(list('site:linkedin.com/in/ AND "Data Scientist" AND "UK"', key = "enter"))
filtered_url <- as.character(remDr$getCurrentUrl())
unfiltered_url <- paste0(filtered_url, "&filter=0")
remDr$navigate(unfiltered_url)

for (i in 1:5) {
  current <- remDr$getPageSource()[[1]]
  links %<>% add_row(
    links = read_html(current) %>%
      html_nodes("div.r > a") %>%
      html_attr("href")
  )
  remDr$findElement("css", "#pnnext")$clickElement()
  Sys.sleep(runif(1, 1, 3))
}

write.csv(links, "links_google.csv", row.names = FALSE)
}

#LinkedIn ----------------

#Load links
if (engine == "b") {
  links <- read_csv("links_bing.csv")
   all_links <- as.character(links$links)
}

if (engine == "g") {
  links <- read_csv("links_google.csv")
  all_links <- as.character(links$links)
}

#function to scrape individual linkedin page

scrape_in <- function(current) {
  tibble(
    name_raw = read_html(current) %>%
      html_nodes(".pv-top-card--list.inline-flex.align-items-center") %>% 
      html_text(),
    info_raw = read_html(current) %>%
      html_nodes(".pv-top-card--list.pv-top-card--list-bullet.mt1") %>% 
      html_text(),
    #followers = read_html(current) %>%
    #  html_nodes(".align-self-center.pv-recent-activity-section__follower-count") %>% 
    #  html_text(),
    jobs_raw = read_html(current) %>% 
      html_nodes(".pv-profile-section__card-item-v2.pv-profile-section.pv-position-entity.ember-view") %>%
      html_text() %>%
      paste(collapse = "-section-section-section"),
    schools_raw = read_html(current) %>% 
      html_nodes(".pv-profile-section__list-item.pv-education-entity.pv-profile-section__card-item.ember-view") %>%
      html_text() %>%
      paste(collapse = "-section-section-section"),
    skills = read_html(current) %>% 
      as.character() %>% 
      str_extract_all('\\w+[\\s\\w]*(?=\\\"\\},\\\"\\$recipeTypes\\\":\\[\\\"com\\.linkedin\\.voyager\\.dash\\.deco\\.identity\\.profile\\.FullProfileSkill)') %>%
      paste()
  ) 
}

profiles <- tibble(
  name_raw = character(),
  info_raw = character(),
  followers = character(),
  jobs_raw = character(),
  schools_raw = character(),
  skills = character()
)

#login to linkedin and loop through links
remDr$navigate("https://www.linkedin.com/login")
remDr$findElement("id", "username")$sendKeysToElement(list("user_email"))
remDr$findElement("id", "password")$sendKeysToElement(list("user_password"))
remDr$findElement("css", ".btn__primary--large.from__button--floating")$clickElement()

for (i in 1:length(all_links)) {
  remDr$navigate(all_links[i])
  Sys.sleep(sample(seq(1, 2, by = 0.01), 1))
  while (remDr$findElements("css", ".pv-profile-section__see-more-inline.pv-profile-section__text-truncate-toggle") %>% length(.)>0) {
    y <- remDr$findElement("css", ".pv-profile-section__see-more-inline.pv-profile-section__text-truncate-toggle")
    remDr$executeScript("arguments[0].click();", list(y))
  }
  x <- remDr$getPageSource()[[1]]
  profiles <- rbind(profiles, scrape_in(x))
  Sys.sleep(sample(seq(1, 3, by = 0.01), 1))
}

write.csv(profiles, "profiles_raw_R.csv")

#close session 
remDr$close()
shell('docker ps')
temp <- enframe(shell('docker ps', intern = TRUE))[[2,2]]
shell(paste('docker stop', word(temp,-1), sep = " "))
shell('docker ps')

#Tidy data ----------------
#tidy raw html scraped from LinkedIn

#load data
profiles_raw <- read.csv("profiles_raw_R.csv", stringsAsFactors = FALSE)

profiles_raw %<>%
  select(-X) %>%
  distinct(.keep_all = TRUE) 

profiles_raw %<>%
  transmute(
    name = str_squish(str_extract(name_raw, "(?<=\\n).*(?=\\n)")),
    current_location = ifelse(str_detect(info_raw, "London, United Kingdom"), "London",
                              ifelse(str_detect(info_raw, "United Kingdom"), "UK",
                                     "Other")),
    jobs_raw = str_squish(jobs_raw),
    schools_raw = str_squish(schools_raw),
    skills = skills
  ) %>% 
  separate(jobs_raw, 
           into = c("company1", "company2", "company3", "company4", "company5", 
                    "company6", "company7", "company8", "company9", "company10", 
                    "company11", "company12", "company13", "company14", "company15"), 
           sep = "-section-section-section")

profiles_raw_long <- profiles_raw %>% 
  pivot_longer(c("company1", "company2", "company3", "company4", "company5", 
                 "company6", "company7", "company8", "company9", "company10", 
                 "company11", "company12", "company13", "company14", "company15"),
               names_to = "company",
               values_to = "raw") %>% 
  drop_na(raw)

profiles_raw_long %<>% 
  separate(raw,
           into = c("joba", "jobb", "jobc", "jobd", "jobe", "jobf", "jobg", "jobh", "jobi", "jobj"),
           sep = "\\s(?=Title)") %>% 
  mutate(
    joba = str_squish(joba),
    jobb = ifelse(str_detect(joba, "^Company Name") & !is.na(jobb), paste(joba, jobb), jobb),
    jobc = ifelse(str_detect(joba, "^Company Name") & !is.na(jobc), paste(joba, jobc), jobc),
    jobd = ifelse(str_detect(joba, "^Company Name") & !is.na(jobd), paste(joba, jobd), jobd),
    jobe = ifelse(str_detect(joba, "^Company Name") & !is.na(jobe), paste(joba, jobe), jobe),
    jobf = ifelse(str_detect(joba, "^Company Name") & !is.na(jobf), paste(joba, jobf), jobf),
    jobg = ifelse(str_detect(joba, "^Company Name") & !is.na(jobg), paste(joba, jobg), jobg),
    jobh = ifelse(str_detect(joba, "^Company Name") & !is.na(jobh), paste(joba, jobh), jobh),
    jobi = ifelse(str_detect(joba, "^Company Name") & !is.na(jobi), paste(joba, jobi), jobi),
    jobj = ifelse(str_detect(joba, "^Company Name") & !is.na(jobj), paste(joba, jobj), jobj),
    joba = ifelse(str_detect(joba, "^Company Name"), NA, joba)
  ) %>% 
  pivot_longer(cols = c("joba", "jobb", "jobc", "jobd", "jobe", "jobf", "jobg", "jobh", "jobi", "jobj"),
               names_to = "job",
               values_to = "raw") %>% 
  drop_na(raw) %>%
  mutate(company_job = paste(company, job, sep = "_")) %>%
  select(-job, -company) %>% 
  group_by(name, skills) %>% 
  arrange(company_job) %>% 
  mutate(
    raw = str_squish(raw),
    job = paste0("job", row_number())) %>%
  ungroup()

profiles_raw_long %<>%
  mutate(
    company = str_squish(
      ifelse(str_detect(raw, "^Company Name"), 
             str_extract(raw, "(?<=Company Name).*(?=Total Duration)"), 
             str_extract(raw, "(?<=Company Name).*(?=Dates Employed)"))
    ),
    job_type = ifelse(str_detect(company, "Internship$|Contract$|Full-time$|Part-time$|Freelance$"),
                      str_extract(company, "Internship|Contract|Full-time|Part-time|Freelance"),
                      NA),
    company = str_squish(
      ifelse(str_detect(company, "Internship$|Contract$|Full-time$|Part-time$|Freelance$"),
             str_replace(company, "Internship$|Contract$|Full-time$|Part-time$|Freelance$", ""),
             company)
    ),
    title = str_squish(
      ifelse(str_detect(raw, "^Company Name"), 
             str_extract(raw, "(?<=Title).*(?=Dates Employed)"), 
             str_extract(raw, ".*(?=Company Name)"))
    ), 
    job_type = ifelse(str_detect(title, "Internship$|Contract$|Full-time$|Part-time$|Freelance$"),
                      str_extract(title, "Internship|Contract|Full-time|Part-time|Freelance"),
                      job_type),
    date_raw = str_squish(str_extract(raw, "(?<=Dates Employed).*(?=Employment Duration)")),
    start = myd(str_extract(date_raw, ".*(?=–)"), truncated = 2),
    current = ifelse(str_detect(date_raw, "Present"), "Yes", "No"),
    end = as_date(ifelse(str_detect(date_raw, "Present"), Sys.Date(), myd(str_extract(date_raw, "(?<=–).*"), truncated = 2))),
    tenure = month(as.period(interval(start, end), unit = "month")) + 1,
    duration_company = (str_squish(ifelse(str_detect(raw, "^Company Name"), str_extract(raw, "(?<=Total Duration).*(?=Title)"), NA)))
  ) %>% 
  #separate(schools_raw,
  #        into = c("school1", "school2", "school3", "school4", "school5", "school6", "school7", "school8", "school9", "school10"),
  #       sep = "-section-section-section") %>%
  select(-company_job, -date_raw)

profiles_raw_wide <- profiles_raw_long %>% 
  pivot_wider(names_from = job, values_from = c(raw, job, company, job_type, title, start, current, end, tenure, duration_company)) 

write_csv(profiles_raw_wide, "profiles_tidy.csv")









