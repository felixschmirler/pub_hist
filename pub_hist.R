#Extracting information about research trends from Google Scholar

#Load packages
library(magrittr)
library(tidyverse)
library(scholar) #only works for authors
library(reticulate)

#attempt to use python package scholarly via reticulate but no success, search functions don't seem to exist
#install python package scholarly
#py_install("scholarly")
#scholarly <-import("scholarly")
#scholarly$
#topic <- "Morality"
#search_moral <- scholarly$publication_parser(topic)
#scholarly$scholarly(topic)

