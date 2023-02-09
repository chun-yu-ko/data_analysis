# Import library ----------------------------------------------------------

library(httr)
library(dplyr)
library(tidyr)
library(data.table)
library(stringr)
library(jiebaR)

Sys.setlocale("LC_TIME", "C")

# Function ----------------------------------------------------------------

fill_null <- function(x) {
  if (is.null(x)) {
    NA
  } else {
    x
  }
}

# Import job content ------------------------------------------------------

list_job_content_2022 <- "dsa job market/past/list_search_result_overall_detail_group.rdata" %>%
  readRDS() %>%
  unlist() %>%
  split(., 1:length(.)) %>%
  lapply(., jsonlite::fromJSON)

list_job_content_2023 <- "dsa_job_market_2023/data/job_content/job_content.RData" %>%
  readRDS()

list_job_content <- append(list_job_content_2022, list_job_content_2023)

rm(list_job_content_2022, list_job_content_2023)

gc()

# Collect jobs ------------------------------------------------------------

data_jobs <- lapply(1:length(list_job_content), function(num) {
  x <- list_job_content[[num]]$data
  data.table(
    employees = x$employees %>% fill_null(),
    industry = x$industry %>% fill_null(),
    industry_no = x$industryNo %>% fill_null(),
    cust_name = x$header$custName %>% fill_null(),
    cust_no = x$custNo %>% fill_null(),
    job_name = x$header$jobName %>% fill_null(),
    id = x$header$analysisUrl %>% str_remove_all(., ".*/") %>% fill_null(),
    appear_date = x$header$appearDate %>% fill_null(),
    content_hr_name = x$contact$hrName %>% fill_null(),
    content_info = x$contact$other %>% fill_null(),
    env_pic_desc = x$environmentPic$environmentPic$description %>% fill_null() %>% unique() %>% na.omit() %>% paste(., collapse = " "),
    accept_role = x$condition$acceptRole$role$description %>% list(),
    work_exp = x$condition$workExp %>% fill_null(),
    edu = x$condition$edu %>% fill_null(),
    major = x$condition$major %>% list(),
    lang = x$condition$language %>% list(),
    specialty = x$condition$specialty$description %>% list(),
    skill = x$condition$skill$description %>% list(),
    contition_desc = x$condition$other %>% fill_null(),
    welfare_tag = x$welfare$tag %>% list(),
    welfare = x$welfare$welfare %>% fill_null(),
    welfare_legal_tag = x$welfare$legalTag %>% list(),
    job_desc = x$jobDetail$jobDescription %>% fill_null(),
    job_category = x$jobDetail$jobCategory$description %>% list(),
    salary = x$jobDetail$salary %>% fill_null(),
    salary_min = x$jobDetail$salaryMin %>% fill_null(),
    salary_max = x$jobDetail$salaryMax %>% fill_null(),
    salary_type = x$jobDetail$salaryType %>% fill_null(),
    job_type = x$jobDetail$jobType %>% fill_null(),
    add_region = x$jobDetail$addressRegion %>% fill_null(),
    add_detail = x$jobDetail$addressDetail %>% fill_null(),
    manage = x$jobDetail$manageResp %>% fill_null(),
    working_hour = x$jobDetail$workPeriod %>% fill_null(),
    vacation_policy = x$jobDetail$vacationPolicy %>% fill_null(),
    need_emp = x$jobDetail$needEmp %>% fill_null(),
    remote = x$jobDetail$remoteWork %>% fill_null()
  )
}) %>%
  do.call(rbind, .) %>%
  data.table()

# Remove duplicate --------------------------------------------------------

var_removable_text = scan("dsa_job_market_2023/data/removable_text.txt", character())%>%paste(., collapse = "|")

data_jobs <- data_jobs %>%
  filter(!is.na(id)) %>%
  arrange(id, desc(appear_date)) %>%
  group_by(id) %>%
  mutate(appear_id = row_number()) %>%
  filter(appear_id == 1) %>%
  select(-appear_id)%>%
  mutate_at(.vars = c("job_name", "job_desc", "contition_desc"), ~str_remove_all(., var_removable_text))%>%
  mutate_at(.vars = c("job_name", "job_desc", "contition_desc"), ~tolower(.))%>%
  data.table

# Segment -----------------------------------------------------------------

jieba_worker = worker(user = "dsa_job_market_2023/data/dict.csv", stop_word = "dsa_job_market_2023/data/stop_word.txt")

list_segment = lapply(1:nrow(data_jobs), function(x){
  tryCatch({
    data.table(id = data_jobs$id[x],
               job_name = data_jobs$job_name[x],
               job_desc = data_jobs$job_desc[x],
               contition_desc = data_jobs$contition_desc[x])%>%
      mutate_at(.vars = c(2, 3, 4), ~segment(., jieba_worker)%>%list)%>%
      return()
  },error=function(e){
    return(NULL)
  })
})%>%
  data.table()%>%
  unnest(c("."))%>%
  setnames(c("id", "text_job_name", "text_job_desc", "text_contition_desc"))%>%
  unnest(c("id"))

data_jobs = data_jobs%>%
  merge(x =., y = list_segment, by = "id", all = T)

rm(list_segment)

gc()

data_jobs%>%
  saveRDS("dsa_job_market_2023/data/job_contents.RData")
