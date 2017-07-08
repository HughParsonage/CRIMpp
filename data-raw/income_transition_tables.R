library(dtplyr)
library(dplyr)
library(data.table)

fread_mark_filename <- function(filename) {
  age <- as.integer(gsub("^.*([0-9]{2}).[0-9]{2}.*$", "\\1", filename))
  out <- 
    fread(filename, header = TRUE) %>%
    melt.data.table(id.vars = "Percentile_pre_2009", variable.name = "Percentile_post_2009") %>%
    .[, value := value + min(value[value > 0]), by = Percentile_pre_2009] %>%
    .[, cumvalue := cumsum(value), by = Percentile_pre_2009] %>%
    .[, Age := age]
  out
}

list.files(path = file.path("~/../Dropbox (Grattan Institute)", 
                            "Retirement incomes",
                            "Data and analysis",
                            "Cameo model", 
                            "income-model-through-hilda",
                            "age-transition-matrices/"), 
           pattern = "^[0-9].*\\.csv$", 
           full.names = TRUE) %>% 
  lapply(fread_mark_filename) %>%
  rbindlist(use.names = TRUE, fill = TRUE)


