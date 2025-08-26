

bp_jp_min_exp_required <-  jp_func(bp_job_postings, "Experience Breakdown")

ex5 <- bp_jp_min_exp_required %>%
  rename(`Years of Experience` = `Minimum Experience`, `Job Postings` = `Unique Postings`, `Percentage of Job Postings` = `% of Total`) %>%
  mutate(`Percentage of Job Postings` = paste0(round(`Percentage of Job Postings`, 2)*100, "%")) %>%
  slice(-1)

e_5_ft <- exhibit_345_func(ex5)
exhibit_doc_func(e_5_ft)

jb_bachelor <- pull(jb_all_count[str_detect(jb_all_count$`Minimum Education Level`, "Bachelor"),], 2)

###### Document officeR_007jobpostings_workexp_bdp.R
jp_ad_count_sheet <- jp_sheets == ("Job Postings Top Occs")
jp_ad_count <- read_excel(job_postings_all,
                          sheet = jp_sheets[jp_ad_count_sheet],
                          skip = 2) %>% suppressMessages()

unique_postings <- sum(pull(jp_ad_count,which(str_detect(names(jp_ad_count), "Unique"))))

jp_all_sheets <- readxl::excel_sheets(job_postings_all)
jb_all_count <- jp_func(job_postings_all, "Minimum Education Breakdown")

jb_all <- sum(pull(jb_all_count, 2))


#jb_nb <- sum(jp_min_exp_required$`Unique Postings`)
exhibit_5_narrative_func <- function(jp_experience_sheet) {
  ex5_narrative <- jp_experience_sheet %>%
    rename(`Years of Experience` = `Minimum Experience`, `Job Postings` = `Unique Postings`, `Percentage of Job Postings` = `% of Total`) %>%
    mutate(`Percentage of Job Postings` = paste0(round(`Percentage of Job Postings`, 2)*100, "%"))

  ex5_narrative <- head(ex5_narrative[order(ex5_narrative$`Job Postings`, decreasing =T),])
  ex5_narrative$`Years of Experience` <- if_else(ex5_narrative$`Years of Experience`== "No Experience Listed", " no years ", str_to_lower(ex5_narrative$`Years of Experience`))

  ex5_narrative <- ex5_narrative %>%
    mutate(perc_and_count = paste0(`Percentage of Job Postings`, " (", prettyNum(`Job Postings`, big.mark = ","), ")"))
  return(ex5_narrative)
  }

jp_min_exp_required_all <-  jp_func(job_postings_all, "Experience Breakdown")
ex5_v2 <- exhibit_5_narrative_func(jp_min_exp_required_all)
typical_entry_level_work_experience_for_all <- pull(ex5_v2, 1)[1]
typical_entry_level_work_experience_for_all <- ifelse(typical_entry_level_work_experience_for_all == " no years ", paste0(typical_entry_level_work_experience_for_all, ' - "none" -'), typical_entry_level_work_experience_for_all)

jp_min_exp_required <-  jp_func(job_postings_all, "Experience Breakdown")

ex5_v2_bp <- exhibit_5_narrative_func(jp_min_exp_required)
typical_entry_level_work_experience_for_bachelor_preferance <- c(pull(ex5_v2_bp, 4)[1], pull(ex5_v2_bp, 1)[1])
typical_entry_level_work_experience_for_bachelor_preferance_followed_by <- c(pull(ex5_v2_bp, 4)[2], pull(ex5_v2_bp, 1)[2])

#jp_func(bp_job_postings, "Minimum Education Breakdown")
#jp_func(bp_job_postings, "Experience Breakdown")

degree_alignment_figures <- jp_func(bp_job_postings, "Education Breakdown")

daf_index <- which(str_detect(degree_alignment_figures$`Education Level`, "Bachelor"))

perc_req_bach <- sum(pull(degree_alignment_figures,2)[daf_index])/sum(pull(degree_alignment_figures,2))

