
cvml_job_growth_v3 <- f_job_growth_v3(cvml_demand)
cvml_job_growth_v3$region <- "CVML"

ca_job_growth_v3 <- f_job_growth_v3(ca_demand)
ca_job_growth_v3$region <- "California"

demand_projections_v3 <- cvml_job_growth_v3 %>%  bind_rows(ca_job_growth_v3) #%>%  bind_rows(cvml_job_growth)
#min_year_ex9 <- year_set_func()-10

#min_year_9 <-
e9 <- demand_projections_v3 %>%
  mutate(Year = as.numeric(as.character(Year))) %>%  # Convert Year to numeric
  #filter(if (trig) !is.na(SOC) else is.na(SOC)) %>%
  filter(between(Year, year_set_func()-10, year_set_func())) %>%
  select(Year, perc_growth, region) %>%
  mutate(perc_growth = round(perc_growth, 1))

e9_1 <- e9 %>%
  mutate(perc_growth = round(perc_growth/100,2) #,
         #region = factor(region, levels = c("NCV/NML", "SCV/SML", "CA (All Occupations)"), ordered = T)
         ) %>%
  pivot_wider(names_from = Year,
              values_from = perc_growth) %>%
  arrange(region)

exhibit_excel_fix(e9_1, "9_Exhibit.xlsx")
exhibit_word_doc("9_Exhibit.xlsx", "Process9")


##### Document officeR_011regional_analysis_1_bdp.R
max_growth_perc_index <- which.max(cvml_job_growth_v3$perc_growth)

ex_9_max_growth_perc <- pull(cvml_job_growth_v3, "perc_growth")[max_growth_perc_index]
ex_9_max_growth_count <- round(pull(cvml_job_growth_v3, "Jobs_n")[max_growth_perc_index])
ex_9_max_growth_year <- pull(cvml_job_growth_v3, "Year")[max_growth_perc_index]


