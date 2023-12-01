source("__cookies.R")

get_data2023 <- function(.day) {
  
  f <- paste0("Input/", stringr::str_pad(as.character(.day), 2, "left", "0"))
  
  write.table(
    readr::read_lines(
      httr::content(
        httr::GET(
          paste0("https://adventofcode.com/2023/day/", .day, "/input"),
          httr::set_cookies(session = co_session) # 53616...
        ), encoding = "UTF-8"
      )
    ),
    f, row.names = FALSE, col.names = FALSE, quote = FALSE)
}
