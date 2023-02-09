
get_job_search_list <- function(cookies, headers, keyword, area_no, page_no) {

  params <- list(
    `ro` = "0",
    `kwop` = "7",
    `keyword` = keyword,
    `area` = area_no,
    `order` = "15",
    `asc` = "0",
    `page` = page_no,
    `mode` = "s",
    `jobsource` = "2018indexpoc",
    `langFlag` = "0",
    `langStatus` = "0",
    `recommendJob` = "1",
    `hotJob` = "1"
  )

  res <- httr::GET(url = "https://www.104.com.tw/jobs/search/list",
                   httr::add_headers(.headers = headers),
                   query = params, httr::set_cookies(.cookies = cookies))

  res %>%
    return()
}
