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

paste("Heute ist der", today())
paste("Heute vor 1 Jahr", date_shift(today(), y = 1))
paste("Heute vor 2 Jahren", date_shift(today(), y = 2))

paste("Gestern war der", date_shift(today(), d = 1))
paste("Gestern vor 1 Jahr", date_shift(today(), y = 1, d = 1))
paste("Gestern vor 2 Jahren", date_shift(today(), y = 2, d = 1))

paste("Erster Tag des aktuellen Monats im aktuellen Jahr:", date_shift(today(), pos = "floor", aggr = "month"))
paste("Erster Tag des aktuellen Monats vor 1 Jahr:", date_shift(today(), y = 1, pos = "floor", aggr = "month"))
paste("Erster Tag des aktuellen Monats vor 2 Jahren:", date_shift(today(), y = 2, pos = "floor", aggr = "month"))

paste("Letzter Tag des letzten Monats:", date_shift(today(), m = 0, pos = "rollback"))
paste("Letzter Tag des letzten Monats vor 1 Jahr:", date_shift(today(), y = 1, m = 0, pos = "rollback"))
paste("Letzter Tag des letzten Monats vor 2 Jahren:", date_shift(today(), y = 2, m = 0, pos = "rollback"))

paste("Letzter Tag vor  1 Monat:", date_shift(today(), m = 0, pos = "rollback"))
paste("Letzter Tag vor  2 Monaten:", date_shift(today(), m = 1, pos = "rollback"))
paste("Letzter Tag vor  3 Monaten:", date_shift(today(), m = 2, pos = "rollback"))
paste("Letzter Tag vor  4 Monaten:", date_shift(today(), m = 3, pos = "rollback"))
paste("Letzter Tag vor  5 Monaten:", date_shift(today(), m = 4, pos = "rollback"))
paste("Letzter Tag vor  6 Monaten:", date_shift(today(), m = 5, pos = "rollback"))
paste("Letzter Tag vor  7 Monaten:", date_shift(today(), m = 6, pos = "rollback"))
paste("Letzter Tag vor  8 Monaten:", date_shift(today(), m = 7, pos = "rollback"))
paste("Letzter Tag vor  9 Monaten:", date_shift(today(), m = 8, pos = "rollback"))
paste("Letzter Tag vor 10 Monaten:", date_shift(today(), m = 9, pos = "rollback"))
paste("Letzter Tag vor 11 Monaten:", date_shift(today(), m = 10, pos = "rollback"))

paste("Letzter Tag vor 12 Monaten:", date_shift(today(), m = 11, pos = "rollback"))
paste("Letzter Tag vor 24 Monaten:", date_shift(today(), m = 23, pos = "rollback"))
paste("Letzter Tag vor 36 Monaten:", date_shift(today(), m = 35, pos = "rollback"))

paste("Letzter Tag vor 12 Monaten:", date_shift(today(), m = 12, pos = "rollback"))
paste("Letzter Tag vor 12 Monaten:", date_shift(today(), y = 1, pos = "rollback"))
        
paste("Silvester vor 1 Jahr:", date_shift(today(), y = 1, pos = "rollback"))
paste("Silvester vor 2 Jahren:", date_shift(today(), y = 2, pos = "rollback"))
paste("Silvester vor 2 Jahren:", date_shift(today(), y = 3, pos = "rollback"))

paste("Dieses Jahr:", format(year(today())))
paste("Letztes Jahr:", format(year(date_shift(today(), y = 1))))
paste("Vorletztes Jahr:", format(year(date_shift(today(), y = 2))))

paste("Dieser Monat:", format(month(today())))
paste("Letzter Monat:", format(month(date_shift(today(), m = 1))))
paste("Vorletzter Monat:", format(month(date_shift(today(), m = 2))))
paste("Vorvorletzter Monat:", format(month(date_shift(today(), m = 3))))

paste("Neujahr aktuelles Jahr:", date_shift(today(), pos = "floor", aggr = "year"))
paste("Neujahr vor 1 Jahr:", date_shift(today(), y = 1, pos = "floor", aggr = "year"))
paste("Neujahr vor 2 Jahren:", date_shift(today(), y = 2, pos = "floor", aggr = "year"))


# Documentation -----------------------------------------------------------

# y = [0-9] | m = [0-9] | w = [0-9] | d = [0-9] | pos = "floor", aggr = "year" | "rollback"
# date_shift(today, y = 1, pos = "floor", aggr = "week")
