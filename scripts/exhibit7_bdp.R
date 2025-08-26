
#min_year <- year_set_func()
#last_year <- min_year+ 10

ca_jobs <- ca_demand %>% select(SOC, Description, names(ca_demand)[str_detect(names(ca_demand), pattern = "^(20[0-9]{2} Jobs)")], `Avg. Annual Openings`)

ca_jobs_bdp <- ca_jobs[,c(-3,-4)]
names(ca_jobs_bdp) <- names(ca_jobs_bdp) %>% str_remove_all(" Jobs|\\...\\d{2}")

exhibit_7 <- pivot_longer(ca_jobs_bdp, cols = -c(1,2),
             names_to = "year",
             values_to = "jobs") %>%
  mutate(year = as.numeric(year)) %>%
  mutate(SOC = "California",
         Description = NULL) %>%
  rename(region = SOC) %>%
  #filter(between(year, min_year, last_year)) %>%
  filter(between(year, year_set_func(), year_set_func()+10)) %>%
  group_by(region, year) %>%
  reframe(jobs = round(sum(jobs))) %>%
  pivot_wider(
    names_from = year,
    values_from = jobs
  ) %>%
  suppressWarnings()

exhibit_excel_fix(exhibit_7, "7_Exhibit.xlsx")
exhibit_word_doc("7_Exhibit.xlsx", "Process7")



