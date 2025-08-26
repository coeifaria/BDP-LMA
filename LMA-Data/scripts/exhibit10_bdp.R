
exhibit_10_func <- function(ca_or_cvml) {
  #  ca_or_cvml <- ca_job_growth
  e8_10 <- ca_or_cvml %>%
    mutate(region = NULL) %>%
    filter(
      Year == as.numeric(op_date_range_min) |
        Year == as.numeric(op_date_range_max)
    ) %>%
    #filter(if (trig) !is.na(SOC) else is.na(SOC)) %>%
    filter(!is.na(SOC)) %>%
    #group_by(SOC, Description, Year) %>%
    #reframe(perc_growth = )
    #select(region, Year, Jobs_n, Annual_Openings) %>%
    mutate(Year = paste0(Year, " Jobs")) %>%
    pivot_wider(
      names_from = Year,
      values_from = c(Jobs_n, perc_growth)
    )

  e8_10_1 <- e8_10 %>%  select(starts_with("Jobs"))
  e8_10_2 <- pull(e8_10_1, 2) - pull(e8_10_1, 1)
  col_name_e8_10 <- paste0(op_date_range_min, " - ", op_date_range_max, " Change")
  e8_10[[col_name_e8_10]] <- round(e8_10_2,0)

  e8_10_3 <- e8_10 %>%   select(ends_with("Change"))
  col_name_e8_10_2 <- paste0(op_date_range_min, " - ", op_date_range_max, " % Change")
  e8_10[[col_name_e8_10_2]] <- paste0(round(e8_10_2/pull(e8_10_1,1),2)*100, "%")


  names(e8_10)[which(str_detect(names(e8_10), "\\d{4}"))[1:2]] <- paste0(c(op_date_range_min, op_date_range_max), " Jobs")
  names(e8_10)[which(str_detect(names(e8_10), "Change"))]
  e8_10_1 <- e8_10[,c(which(str_detect(names(e8_10), "Description")), which(str_detect(names(e8_10), "\\d{4}"))[1:2], which(str_detect(names(e8_10), "Change")),which(str_detect(names(e8_10), "Annual")))]
  names(e8_10_1)[which(str_detect(names(e8_10_1), "Description"))] <- "Occupation"
  names(e8_10_1)[which(str_detect(names(e8_10_1), "Annual"))] <- "Annual Openings"

  e8_10_1[,2] <- round(e8_10_1[,2],0)
  e8_10_1[,3] <- round(e8_10_1[,3],0)
  e8_10_1[,6] <- round(e8_10_1[,6],0)

  e8_10_total_row <- tibble(
    Occupation = "Total",
    min_jobs = sum(e8_10_1[,2]),
    max_jobs = sum(e8_10_1[,3]),
    change_jobs = 0,
    perc_change_jobs = 0,
    openings = sum(e8_10_1[,6])
  )

  e8_10_total_row <- e8_10_total_row %>%
    mutate(change_jobs = max_jobs-min_jobs,
           perc_change_jobs = paste0(round(100*change_jobs/min_jobs), "%"))

  names(e8_10_total_row) <- names(e8_10_1)

  e8_10_2 <- bind_rows(e8_10_1, e8_10_total_row)

    return(e8_10_2)
}

cvml_annual_openings <- tail(pull(exhibit_10_func(f_job_growth(cvml_demand)), `Annual Openings`), 1)
