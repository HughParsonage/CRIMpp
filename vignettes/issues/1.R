library(data.table)
library(taxstats)
library(magrittr)

sample_file_1516 <- fread("~/ozTaxData/data-raw/2016_sample_file.csv")
lapply(list("2015-16" = sample_file_1516, 
            "2014-15" = SampleFile1415::sample_file_1415, 
            "2013-14" = sample_file_1314),
       function(x) {
         sample_file_1516 %>%
           setkey(age_range) %>%
           age_range_decoder[., on = "age_range"] %>%
           .[Sw_amt > 0] %>%
           .[, ageWise_salary_percentile := dplyr::ntile(Sw_amt, 100), keyby = "age_range_description"] %>%
           .[, .(AverageWage = mean(Sw_amt)), 
             keyby = .(age_range_description, ageWise_salary_percentile)]
            }) %>%
  rbindlist(idcol = "fy.year", use.names = TRUE, fill = TRUE) %>%
  .[, AverageWage := round(AverageWage, 2)] %>%
  fwrite("inst/extdata/AverageWage-vs-age-percentile.csv")





