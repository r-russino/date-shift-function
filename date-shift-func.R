library(dplyr)
library(lubridate)


# Date Shift Function -----------------------------------------------------

date_shift <- function(base_date, y = 0, m = 0, w = 0, d = 0, pos = "", aggr = "") {
  
  y_period <- period(y, unit = "years")
  m_period <- period(m, unit = "months")
  w_period <- period(w, unit = "weeks")
  d_period <- period(d, unit = "days")
  
  aggr_enquo <- rlang::sym(aggr)
  
  if (pos == "floor") {
    base_date <- floor_date(base_date, aggr_enquo)
    base_date <- base_date - y_period - m_period - w_period - d_period
    
  } else if (pos == "rollback") {
    base_date <- rollback(base_date - y_period - m_period - w_period - d_period)
    
  } else {
    base_date <- base_date - y_period - m_period - w_period - d_period
    
  }
}


# Frequently used ---------------------------------------------------------

paste("Today:", today())
paste("1 year  ago today:", date_shift(today(), y = 1))
paste("2 years ago today:", date_shift(today(), y = 2))

paste("Yesterday", date_shift(today(), d = 1))
paste("yesterday 1 year  ago:", date_shift(today(), y = 1, d = 1))
paste("yesterday 2 years ago:", date_shift(today(), y = 2, d = 1))

paste("First day of the current month in the current year:", date_shift(today(), pos = "floor", aggr = "month"))
paste("First day of the current month 1 year  ago:", date_shift(today(), y = 1, pos = "floor", aggr = "month"))
paste("First day of the current month 2 years ago:", date_shift(today(), y = 2, pos = "floor", aggr = "month"))

paste("Last day of last month:", date_shift(today(), m = 0, pos = "rollback"))
paste("Last day of last month 1 year  ago:", date_shift(today(), y = 1, m = 0, pos = "rollback"))
paste("Last day of last month 2 years ago:", date_shift(today(), y = 2, m = 0, pos = "rollback"))

paste("Last day of the month  1 month ago:", date_shift(today(), m = 0, pos = "rollback"))
paste("Last day of the month  2 months ago:", date_shift(today(), m = 1, pos = "rollback"))
paste("Last day of the month  3 months ago:", date_shift(today(), m = 2, pos = "rollback"))
paste("Last day of the month  4 months ago:", date_shift(today(), m = 3, pos = "rollback"))
paste("Last day of the month  5 months ago:", date_shift(today(), m = 4, pos = "rollback"))
paste("Last day of the month  6 months ago:", date_shift(today(), m = 5, pos = "rollback"))
paste("Last day of the month  7 months ago:", date_shift(today(), m = 6, pos = "rollback"))
paste("Last day of the month  8 months ago:", date_shift(today(), m = 7, pos = "rollback"))
paste("Last day of the month  9 months ago:", date_shift(today(), m = 8, pos = "rollback"))
paste("Last day of the month 10 months ago:", date_shift(today(), m = 9, pos = "rollback"))
paste("Last day of the month 11 months ago:", date_shift(today(), m = 10, pos = "rollback"))

paste("Last day 12 months ago:", date_shift(today(), m = 11, pos = "rollback"))
paste("Last day 24 months ago:", date_shift(today(), m = 23, pos = "rollback"))
paste("Last day 36 months ago:", date_shift(today(), m = 35, pos = "rollback"))

paste("Last day of the month 12 months ago:", date_shift(today(), m = 12, pos = "rollback"))
paste("Last day of the month 12 months ago:", date_shift(today(), y = 1, pos = "rollback"))
        
paste("New Year's Eve 1 year  ago:", date_shift(today(), y = 1, pos = "rollback"))
paste("New Year's Eve 2 years ago:", date_shift(today(), y = 2, pos = "rollback"))
paste("New Year's Eve 3 years ago:", date_shift(today(), y = 3, pos = "rollback"))

paste("This year:", format(year(today())))
paste("Last year:", format(year(date_shift(today(), y = 1))))
paste("Two years ago:", format(year(date_shift(today(), y = 2))))

paste("This month:", format(month(today())))
paste("Last month:", format(month(date_shift(today(), m = 1))))
paste("Two months ago:", format(month(date_shift(today(), m = 2))))
paste("Three months ago:", format(month(date_shift(today(), m = 3))))

paste("New year current year:", date_shift(today(), pos = "floor", aggr = "year"))
paste("New Year 1 year  ago:", date_shift(today(), y = 1, pos = "floor", aggr = "year"))
paste("New Year 2 years ago:", date_shift(today(), y = 2, pos = "floor", aggr = "year"))


# Documentation -----------------------------------------------------------

# y = [0-9] | m = [0-9] | w = [0-9] | d = [0-9] | pos = "floor", aggr = "year" | "rollback"
# date_shift(today, y = 1, pos = "floor", aggr = "week")
