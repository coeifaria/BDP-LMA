

op_date_range_min <- year_set_func()
op_date_range_max <- year_set_func() + 10

# Custom formatting functions
exhibit_8_10_func <- function(ca_or_cvml) {
  #e8_10 <- f_job_growth(ca_demand) %>%
  e8_10 <- ca_or_cvml %>%
    mutate(region = NULL) %>%
    filter(
      Year == as.numeric(year_set_func()) |
        Year == as.numeric(year_set_func()+10)
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
  col_name_e8_10 <- paste0(year_set_func(), " - ", op_date_range_max, " Change")
  e8_10[[col_name_e8_10]] <- round(e8_10_2,0)

  e8_10_3 <- e8_10 %>%   select(ends_with("Change"))
  col_name_e8_10_2 <- paste0(year_set_func(), " - ", op_date_range_max, " % Change")
  e8_10[[col_name_e8_10_2]] <- paste0(round(e8_10_2/pull(e8_10_1,1),2)*100, "%")


  names(e8_10)[which(str_detect(names(e8_10), "\\d{4}"))[1:2]] <- paste0(c(year_set_func(), op_date_range_max), " Jobs")
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


  # Create flextable with dynamic column logic
  return(e8_10_2)
}


exhibit_8_10_flex_func <- function(e8_10_2) {
  col_change <- names(e8_10_2)[4]
  col_pct_change <- names(e8_10_2)[5]


  # Store original numeric values for color logic
  ._change_raw <- e8_10_2[[col_change]]
  ._pct_raw <- e8_10_2[[col_pct_change]]
  e_10_1_maker <- nrow(e8_10_2)-1
  e_10_1_col_change <- as.numeric(str_remove_all(e8_10_2[[col_change]], ","))[1:e_10_1_maker]
  e_10_1_col_pct_change <- as.numeric(str_remove(e8_10_2[[col_pct_change]], "\\%"))[1:e_10_1_maker]/100
  # Format the values (without altering raw columns)
  e8_10_2_2 <- format_num_func(e_10_1_col_change, "#",F,T)
  e8_10_2[[col_change]][1:length(e8_10_2_2)] <- e8_10_2_2

  e8_10_2_3 <- format_num_func(e_10_1_col_pct_change, "%",F,T)
  e8_10_2[[col_pct_change]][1:length(e8_10_2_3)] <- e8_10_2_3

  as.numeric(._change_raw)
  as.numeric(str_remove(._pct_raw, "\\%"))

  p_ind1 <- which(str_detect(pull(e8_10_2, 5), "%"))
  p_ind2 <- which(!str_detect(pull(e8_10_2, 5), "%"))
  p_ind_good <- c(pull(e8_10_2, 5)[p_ind1], paste0(pull(e8_10_2, 5)[p_ind2], "%"))
  e8_10_2[,5] <- p_ind_good[sort(c(p_ind1, p_ind2))]

  e8_10_2[[col_change]][1:length(e8_10_2_2)] <- e8_10_2_2
  ._change_raw <- e8_10_2[[col_change]]
  ._pct_raw <- e8_10_2[[col_pct_change]]

  e8_10_2_ft <- flextable(e8_10_2) %>%
    # Bold last row
    #bold(i = nrow(e8_10_1), bold = TRUE) %>%

    # Red color for negative values
    color(
      i = which(as.numeric(str_remove_all(._change_raw, ",")) < 0),
      j = col_change,
      color = "red"
    ) %>%
    color(
      i = which(as.numeric(str_remove(._pct_raw, "\\%")) < 0),
      j = col_pct_change,
      color = "red"
    ) %>%

    # Center align all content
    align(align = "center", part = "all") %>%

    # Set header labels with line breaks
    set_header_labels(.labels = setNames(
      c(gsub(" ", "\n", col_change, fixed = TRUE),
        gsub(" ", "\n", col_pct_change, fixed = TRUE)),
      c(col_change, col_pct_change)
    )) %>%

    # Style header
    fontsize(size = 12, part = "header") %>%
    font(fontname = "Segoe UI", part = "header") %>%
    bold(part = "header") %>%
    color(color = "white", part = "header") %>%
    bg(bg = table1_header_color, part = "header") %>%

    # Style body
    fontsize(size = 12, part = "body") %>%
    font(fontname = "Segoe UI", part = "body") %>%

    # Style footer (last row as footer proxy)
    bg(i = seq(ifelse(nrow(e8_10_2)==1, 1, 2), nrow(e8_10_2), 2), bg = table_banding_odd, part = "body") %>%
    bg(i = seq(1, nrow(e8_10_2), 2), bg = table_banding_even, part = "body") %>%
    fontsize(i = nrow(e8_10_2), size = 12, part = "body") %>%
    font(i = nrow(e8_10_2), fontname = "Segoe UI", part = "body") %>%
    bg(nrow(e8_10_2), bg = total_bar_color, part = "body") %>%
    bold(nrow(e8_10_2), part = "body") %>%
    border_remove() %>%
    border_inner_h(border = border_settings, part = "body") %>%
    hline_top(border = border_settings, part = "body") %>%
    hline_bottom(border = border_settings, part = "body") %>%
    set_table_properties(layout = "autofit", width = 1) # Replaces autofit()
    # Autofit column widths
    #autofit()

  return(e8_10_2_ft)
}

#ca_job_growth <- f_job_growth_v3(ca_demand)
ca_job_growth <- f_job_growth(ca_demand)
cvml_job_growth <- f_job_growth(cvml_demand)


exhibit8 <- exhibit_8_10_func(f_job_growth(ca_demand))
exhibit10 <- exhibit_8_10_func(f_job_growth(cvml_demand))

exhibit8_ft <-  exhibit_8_10_flex_func(exhibit8)
exhibit10_ft <- exhibit_8_10_flex_func(exhibit10)

d1_num <- ca_job_growth %>%
  mutate(region = NULL) %>%
  filter(
    Year == as.numeric(op_date_range_min) |
    Year == as.numeric(op_date_range_max)
  )

growth_perc_func <- function(ca_or_cvml){

    d_num <- ca_or_cvml %>%
    mutate(region = NULL) %>%
    filter(
      Year == as.numeric(op_date_range_min) |
        Year == as.numeric(op_date_range_max)
    )
  output <- paste0(round((pull(d_num, Jobs_n)[2]-pull(d_num, Jobs_n)[1])/pull(d_num, Jobs_n)[1],2)*100, "%")
  return(output)
}

statewide_growth <- growth_perc_func(ca_job_growth)
cvml_growth <- growth_perc_func(cvml_job_growth)

d1_num_2 <- cvml_job_growth %>%
  mutate(region = NULL) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(between(Year,as.numeric(year_set_func()), as.numeric(year_set_func()+10)))

d1_num_2_2 <- cvml_job_growth %>%
  mutate(region = NULL) %>%
  filter(
    Year == as.numeric(op_date_range_min) |
      Year == as.numeric(op_date_range_max)
  )

d1_cvml_highest_growth_ind <- which.max(d1_num_2$perc_growth)
d1_cvml_highest_growth_perc <- paste0(round(pull(d1_num_2, perc_growth)[d1_cvml_highest_growth_ind]), "%")
d1_cvml_highest_growth_year <- pull(d1_num_2, Year)[d1_cvml_highest_growth_ind]

d2_ca_annual_openings <- prettyNum(round(pull(d1_num, Annual_Openings)[1]), big.mark = ",")
d2_cvml_annual_openings <- prettyNum(round(pull(d1_num_2_2, Annual_Openings)[1]), big.mark = ",")

