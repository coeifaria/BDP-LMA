#```{r exhibit9, chunk_progress=TRUE}

exhibit2 <- EducationalAttainment_filtered %>%
  select(c(0:9)) %>%
  pivot_longer(
    cols = c(3:9),
    names_to = "lev",
    values_to = "per")


#table(exhibit9_1$lev)

ed_levels <- c(
  "Less than high school diploma",
  "High school diploma or equivalent",
  "Some college, no degree",
  "Associate's degree",
  "Bachelor's degree",
  "Master's degree",
  "Doctoral or professional degree")

ed_levels_abrev <- c("High School Diploma or Less",
                     "Some College or Associate Degree",
                     "Bachelor's Degree",
                     "Graduate or Professional Degree")

ed_levels_abrev_v2 <- c("High School Diploma or Less",
                     "Some college, no degree",
                     "Associate's degree",
                     "Bachelor's Degree",
                     "Graduate or Professional Degree")

exhibit2_1 <- exhibit2 %>%
  mutate(ed_levels = case_when(
    lev %in% c("Less than high school diploma", "High school diploma or equivalent") ~ "High School Diploma or Less",
    lev %in% c("Some college, no degree", "Associate's degree") ~ "Some College or Associate Degree",
    lev == "Bachelor's degree" ~ "Bachelor's Degree",
    lev %in% c("Master's degree", "Doctoral or professional degree") ~ "Graduate or Professional Degree",
    TRUE ~ lev  # Keep the level unchanged if it doesn't match any case
#  ),
#  `2023 National Employment Matrix title` =
#    str_wrap(`2023 National Employment Matrix title`, width = 20)
  )) %>%
  rename(field = `2023 National Employment Matrix title`) %>%
  mutate(ed_levels = factor(ed_levels, levels = rev(ed_levels_abrev), order=T)) %>%
  group_by(field, ed_levels) %>%
  summarize(per = round(sum(per),0)) %>%
  arrange(field, ed_levels) %>%
  ungroup() %>%
  suppressMessages()
#mutate(per = paste0(round(per,0), "%")) %>%
#mutate(field = str_replace_all(str_wrap(field, width = 30), "\n", "<br>")) %>%


exhibit2_2 <- exhibit2 %>%
  mutate(ed_levels = case_when(
    lev %in% c("Less than high school diploma", "High school diploma or equivalent") ~ "High School Diploma or Less",
    lev %in% c("Some college, no degree", "Associate's degree") ~ "Some College or Associate Degree",
    lev == "Bachelor's degree" ~ "Bachelor's Degree",
    lev %in% c("Master's degree", "Doctoral or professional degree") ~ "Graduate or Professional Degree",
    TRUE ~ lev
  ),
  field = str_wrap(`2023 National Employment Matrix title`, width = 20)) %>%
  mutate(ed_levels = factor(ed_levels, levels = rev(ed_levels_abrev), order=T)) %>%
  group_by(field, ed_levels) %>%
  summarize(per = round(sum(per),0)) %>%
  arrange(field, ed_levels) %>%
  ungroup() %>%
  group_by(field) %>%
  mutate(cum_per = cumsum(per),
         label_position = case_when(
           ed_levels == "High School Diploma or Less" ~ 5,  # Far left
           ed_levels == "Graduate or Professional Degree" ~ 100,  # Far right
           TRUE ~ lag(abs(cum_per-100), 0)  # Others lag by 1
         )) %>%
  ungroup() %>%
  suppressMessages()


exhibit2_3 <- exhibit2_2 %>%
  rename(cumulative_per = cum_per) %>%
  select(-label_position, -cumulative_per) %>%
  mutate(field = str_replace_all(field, pattern = "\n", replacement = " "),
         per = round(per/100, 2)) %>%
  pivot_wider(names_from = ed_levels,
              values_from = per) %>%
  select(1, 5,4,3,2) #%>%  arrange(desc(`Some College or Associate Degree` ))

exhibit_excel_fix(exhibit2_3, "2_Exhibit.xlsx")
exhibit_word_doc("2_Exhibit.xlsx", "Process2")


exhibit2_4 <- exhibit2 %>%
  mutate(ed_levels = case_when(
    lev %in% c("Less than high school diploma", "High school diploma or equivalent") ~ "High School Diploma or Less",
    #lev %in% c("Some college, no degree", "Associate's degree") ~ "Some College or Associate Degree",
    lev == "Bachelor's degree" ~ "Bachelor's Degree",
    lev %in% c("Master's degree", "Doctoral or professional degree") ~ "Graduate or Professional Degree",
    TRUE ~ lev
  ),
  field = str_wrap(`2023 National Employment Matrix title`, width = 20)) %>%
  mutate(ed_levels = factor(ed_levels, levels = rev(ed_levels_abrev_v2), order=T)) %>%
  group_by(field, ed_levels) %>%
  summarize(per = round(sum(per),0)) %>%
  arrange(field, ed_levels) %>%
  ungroup() %>%
  group_by(field) %>%
  mutate(cum_per = cumsum(per),
         label_position = case_when(
           ed_levels == "High School Diploma or Less" ~ 5,  # Far left
           ed_levels == "Graduate or Professional Degree" ~ 100,  # Far right
           TRUE ~ lag(abs(cum_per-100), 0)  # Others lag by 1
         )) %>%
  ungroup() %>%
  rename(cumulative_per = cum_per) %>%
  select(-label_position, -cumulative_per) %>%
  mutate(field = str_replace_all(field, pattern = "\n", replacement = " "),
         per = round(per/100, 2)) %>%
  pivot_wider(names_from = ed_levels,
              values_from = per) %>%
suppressMessages()



bachelor_range <- pull(exhibit2_4, names(exhibit2_4)[str_detect(names(exhibit2_4), "Bachelor")]) %>% range() %>% unique()
#bachelor_range_test <- c(.01, .23, .15, .45, .85, .22) %>% range()


bachelor_range_f(bachelor_range, 2)
