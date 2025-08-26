

jp_sheets <- readxl::excel_sheets(bp_job_postings)
jp_edu_index <- jp_sheets %>% str_detect("Edu and Experience")
jp_edu_sheets <- jp_sheets[jp_edu_index]

for (edu_sheet in jp_edu_sheets) {
  jp_edu <- read_excel(bp_job_postings,
                       sheet = edu_sheet,
                       skip = 0) %>% suppressMessages()
  if(str_detect(names(jp_edu)[1], "Minimum Education")){
    jp_min_edu_sheet <- edu_sheet
  }
}

jp_min_edu_required <-  read_excel(bp_job_postings,
                                   sheet = jp_min_edu_sheet,
                                   skip = 1)

ex3 <- jp_min_edu_required %>%
  rename(`Education Level` = `Minimum Education Level`, `Job Postings` = `Unique Postings (minimum)`) %>%
  mutate(order = 1)  %>%
  select(`Education Level`, `Job Postings`, order) %>%
  mutate(
    `Education Level` = ifelse(`Education Level` %in% c("Master's degree", "Ph.D. or professional degree"), "Master's degree or higher", a_degree_fix(`Education Level`))
  ) %>%
  group_by(`Education Level`) %>%
  reframe(`Job Postings` = sum(`Job Postings`))


ex3_order <- c("High school or GED", "Associate degree", "Bachelor's degree", "Master's degree or higher")
ex3_order <- factor(ex3_order, ordered = T)
jb_nb <- sum(ex3$`Job Postings`)
ex3_1 <- ex3 %>% mutate(`Percentage of Job Postings` = paste0(100*round(`Job Postings`/jb_nb,2), "%"))

ex3_1 <- ex3_1 %>%
  mutate(`Education Level` = factor(`Education Level`, levels = ex3_order, order = T)) %>%
  arrange(`Education Level`)

total_job_postings <- prettyNum(sum(ex3$`Job Postings`), big.mark = ",", scientific = FALSE)

exhibit_345_func <- function(exhibit){

  exhibit_ft <- flextable(exhibit) %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 12, part = "header") %>%
  font(fontname = "Segoe UI", part = "header") %>%
  bold(part = "header") %>%
  color(color = table_header_fontcolor, part = "header") %>%
  bg(bg = table1_header_color, part = "header") %>%
  border_remove() %>%
  border_inner_h(border = border_settings, part = "body") %>%
  hline_top(border = border_settings, part = "body") %>%
  hline_bottom(border = border_settings, part = "body") %>%
  fontsize(size = 12, part = "body") %>%
  font(fontname = "Segoe UI", part = "body") %>%
  bg(i = seq(ifelse(nrow(exhibit)==1, 1, 2), nrow(exhibit), 2), bg = table_banding_even, part = "body") %>%
  bg(i = seq(1, nrow(exhibit), 2), bg = table_banding_odd, part = "body") %>%
  fontsize(i = nrow(exhibit), size = 12, part = "body") %>%
  font(i = nrow(exhibit), fontname = "Segoe UI", part = "body") %>%
  autofit()
return(exhibit_ft)
}


e_3_ft <- exhibit_345_func(ex3_1) %>%
  bold(which(str_detect(ex3_1$`Education Level`, "Bachelor")))

exhibit_doc_func(e_3_ft)

#e_3_ft
