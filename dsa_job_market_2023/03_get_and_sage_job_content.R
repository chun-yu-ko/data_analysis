# Environment setting -----------------------------------------------------

options(scipen = 999)
Sys.setlocale("LC_TIME", "C")

# Import library ----------------------------------------------------------

library(httr)
library(dplyr)
library(data.table)

# Vars --------------------------------------------------------------------

var_curl = "curl 'https://{{domain}}/job/ajax/content/ID' -H 'Accept: application/json, text/plain, */*' -H 'Referer: https://{{domain}}/job/ID'"

# Import jobs search list -------------------------------------------------

data_jobs_search_list = "data/jobs_search_list.RData"%>%
  readRDS()

list_curl = data_jobs_search_list%>%
  pull(job_link)%>%
  stringr::str_remove_all(., "\\?.*")%>%stringr::str_remove_all(., ".*\\/")%>%
  unique%>%
  sort%>%
  data.table(id = .)%>%
  mutate(curl = stringr::str_replace_all(var_curl, "ID", id))%>%
  pull(curl)

list_result = lapply(1:length(list_curl), function(x){
  if(x%%1000 == 0 ) Sys.sleep(60)
  system(command = list_curl[x], intern = TRUE)
})%>%
  lapply(., jsonlite::fromJSON)

saveRDS(list_result, file = "data/job_content/job_content.RData")


list_result[[1]]
