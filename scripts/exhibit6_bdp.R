for(jp_file in advertised_salaries) {
  job_postings <-  read_excel(jp_file, sheet = "Parameters") %>% suppressMessages()

  hs_preferred_filter <- length(which(str_detect(job_postings$Parameters, "High school")))
  associate_preferred_filter <- length(which(str_detect(job_postings$Parameters, "Associate's")))
  bachelors_preferred_filter <- length(which(str_detect(job_postings$Parameters, "Bachelor's")))
  mastersplus_preferred_filter <- length(which(str_detect(job_postings$Parameters, "Master's")))

  if(hs_preferred_filter==1) {
    hs_advertised_salaries <- jp_file
  }
  else if (associate_preferred_filter==1) {
    as_advertised_salaries <- jp_file
  }
  else if (bachelors_preferred_filter==1){
    bs_advertised_salaries <- jp_file
  }
  else if (mastersplus_preferred_filter==1){
    ms_advertised_salaries <- jp_file
  } else {
    "error"
  }
}

advertised_salaries_files <- c(hs_advertised_salaries, as_advertised_salaries, bs_advertised_salaries, ms_advertised_salaries)
advertised_salaries_list <- list()

weighted_avg_salary_column_func <- function(tab_x, column, nrow_length){
  #value <- tab_x[[names(tab_x)[which(str_detect(names(tab_x), column))]]][nrow_length]
  #tab_x <- tab
  value <- tab_x[[names(tab_x)[which(str_detect(names(tab_x), column))]]]
  v_length <- length(na.omit(value))
  ads <- tab_x[[names(tab_x)[which(str_detect(names(tab_x), "Advertised Salary Observations"))]]]
  ads <- ads[1:v_length]
  output <- sum((ads/sum(ads))*value[1:v_length])
  return(output)
}
#t1 <- read_excel(advertised_salaries_files[3], sheet = "Job Postings Occ Table")

#weighted_avg_salary_column_func(t1,
#                                "Pct. 25 Hourly",
#                                nrow(t1[!is.na(t1$SOC),])) %>% round() %>% format_num_func("$W", F, F)

advertised_salary_func <- function(table_num = T){
for (file in advertised_salaries_files){
  education <- c("High school diploma or GED", "Associate degree", "Bachelor's degree", "Master's degree or above")[which(advertised_salaries_files %in% file)]
  tab  <- read_excel(file, sheet = "Job Postings Occ Table") %>% suppressMessages()
  ad_table_length <- nrow(tab[!is.na(tab$SOC),])

  postings <- sum(pull(tab, names(tab)[which(str_detect(names(tab), "Advertised Salary Observations"))])[1:ad_table_length]) %>% prettyNum(big.mark = ",")
  salary <- weighted_avg_salary_column_func(tab, "Pct. 25 Annual", ad_table_length) %>% round() %>% format_num_func("$S", F, F)
  hourly <- weighted_avg_salary_column_func(tab, "Pct. 25 Hourly", ad_table_length) %>% round(2) %>% format_num_func("$W", T, F)
if(table_num){
  ad_table <- tibble(
    "Education" = education,
    "Job Postings\nwith Advertised\nSalary" = postings,
    "Advertised\nEntry-Level\nWage" = paste0(salary,
                                           "\n",
                                           hourly
    )
  )

  advertised_salaries_list[[file]] <-  ad_table
} else {
  ad_table <- tibble(
    "Education" = education,
    "Job Postings with Advertised Salary" = postings,
    "Advertised Salary" = weighted_avg_salary_column_func(tab, "Pct. 25 Annual", ad_table_length) %>% round(2),
    "Advertised Wage" = weighted_avg_salary_column_func(tab, "Pct. 25 Hourly", ad_table_length) %>% round(2)
    )
  advertised_salaries_list[[file]] <-  ad_table
}
  advertised_salaries_table <- do.call(bind_rows, advertised_salaries_list)}
  return(advertised_salaries_table)
}

advertised_salaries_table <- advertised_salary_func()

exhibit6_ft <- flextable(advertised_salaries_table) %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 12, part = "all") %>%
  font(fontname = "Segoe UI", part = "all") %>%
  bold(part = "header") %>%
  color(color = table_header_fontcolor, part = "header") %>%
  bg(bg = table1_header_color, part = "header") %>%

  bold(j = 1, part = "body") %>%
  color(j = 1, color = table_header_fontcolor, part = "body") %>%
  bg(i = str_detect(advertised_salaries_table$Education, "Bachelor's"), bg = "#D3D4D3", part = "body") %>%
  bg(j = 1, bg = table1_header_color, part = "body") %>%
  border_remove() %>%
  border_inner_h(border = border_settings, part = "body") %>%
  border_inner_v(border = border_settings, part = "body") %>%
  border_outer(border = border_settings, part = "all") %>%
  #hline_top(border = border_settings, part = "body") %>%
  #hline_bottom(border = border_settings, part = "body") %>%
  #fontsize(size = 12, part = "body") %>%
  #font(fontname = "Segoe UI", part = "body") %>%
  bold(i = str_detect(advertised_salaries_table$Education, "Bachelor's"), part = "body") %>%
  autofit()


exhibit_doc_func(exhibit6_ft)


###### Document officeR_008advertised_salary_bdp

jp_ned <- sum(as.numeric(pull(advertised_salaries_table, 2)))

advertised_salaries_table_numbers <- advertised_salary_func(F)

ast_wage <- function(degree) {
  index <- which(str_detect(advertised_salaries_table_numbers$Education,degree))
  return(pull(advertised_salaries_table_numbers, "Advertised Wage")[index])
}

b_a_difference <- ast_wage("Bachelor") - ast_wage("Associate")

format_num_func(b_a_difference, "$W")
