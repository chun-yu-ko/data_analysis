# Environment setting -----------------------------------------------------

options(scipen = 999)
Sys.setlocale("LC_TIME", "C")

# Import library ----------------------------------------------------------

library(httr)
library(dplyr)
library(data.table)

# Import other source -----------------------------------------------------

"jobs_search_list/get_jobs_search_list.R"%>%source()

"jobs_search_list/cookie_header.R"%>%source()

keywords = "jobs_search_list/keyword.txt"%>%scan(., character())

area_no = "jobs_search_list/area_no.txt"%>%scan(., character())

# For loop ask for result -------------------------------------------------

for (i in 1:length(keywords)){

  for(j in 1:length(area_no)){

    list_result = list()

    page_no = 1

    print(paste("keywords = ", keywords[i], ", area_no = ", area_no[j], sep = ""))

    result = get_job_search_list(cookies = cookie, headers = header, keyword = keywords[i], area_no = area_no[j], page_no = page_no)

    list_result = append(list_result, list(result))

    while (page_no < content(result)$data$totalPage) {

      page_no = page_no + 1

      result = get_job_search_list(cookies = cookie, headers = header, keyword = keywords[i], area_no = area_no[j], page_no = page_no)

      list_result = append(list_result, list(result))
    }

    saveRDS(list_result, file = paste("data/jobs_search_list/",i,"_",j,".RData",sep = ""))
  }

}
