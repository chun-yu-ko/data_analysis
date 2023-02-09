# Environment setting -----------------------------------------------------

setwd("./dsa_job_market_2023")
options(scipen = 999)
Sys.setlocale("LC_TIME", "C")

# Import library ----------------------------------------------------------

library(httr)
library(dplyr)
library(data.table)
library(tidyr)

# Function ----------------------------------------------------------------

fill_null = function(x){
  if(is.null(x)){
    NA
  } else x
}

# Extract -----------------------------------------------------------------

list_file = list.files("data/jobs_search_list/", full.names = T)

data_jobs_search_list = list_file%>%
  lapply(., function(i){
    print(i)
    i = i %>% readRDS
    tryCatch({
      lapply(i, function(j){
        j = j %>% content()
        j_list = j$data$list

        lapply(j_list, function(k){
          c(k[["jobType"]]%>%fill_null, k[["jobNo"]]%>%fill_null, k[["jobName"]]%>%fill_null, k[["custNo"]]%>%fill_null, k[["custName"]]%>%fill_null, k[["link"]][["job"]]%>%fill_null)%>%
            return()
        })%>%
          do.call(rbind, .)%>%
          data.table()%>%
          setnames(c("job_type", "job_no", "job_name", "cust_no", "cust_name", "job_link"))%>%
          data.table()%>%
          mutate(total_page = j$data$totalPage,
                 total_count = j$data$totalCount,
                 page_no = j$data$pageNo,
                 keyword = j$data$queryDesc$keyword,
                 area = j$data$queryDesc$area)%>%
          return()
      })%>%
        do.call(rbind, .)%>%
        return()
    }, error = function(e) return(NULL))
  })%>%
  do.call(rbind, .)%>%
  data.table

saveRDS(data_jobs_search_list, file = "data/jobs_search_list.RData")

