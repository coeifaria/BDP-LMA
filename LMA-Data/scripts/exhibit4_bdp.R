jp_sheets <- readxl::excel_sheets(bp_job_postings)
jp_edu_index <- jp_sheets %>% str_detect("Edu and Experience")
jp_edu_sheets <- jp_sheets[jp_edu_index]

jp_func <- function(job_postings, sheet_needed){
  jp_sheet_output <- "no match"
  for (sheet in jp_edu_sheets) {
    jp_sheet_iteration <- read_excel(job_postings,
                                     sheet = sheet,
                                     skip = 0) %>%
      suppressMessages()
    #names(jp_sheet_iteration) <- slice(jp_sheet_iteration, 2)
    if(names(jp_sheet_iteration)[1] == sheet_needed){
      names(jp_sheet_iteration) <- slice(jp_sheet_iteration, 2)
      jp_sheet_output <- slice(jp_sheet_iteration, -c(1,2))
      jp_sheet_output[[2]] <- as.numeric(jp_sheet_output[[2]])
      jp_sheet_output[[3]] <- as.numeric(jp_sheet_output[[3]])

    }
  }
  return(jp_sheet_output)
}


jp_top_qualifications <-  read_excel(bp_job_postings,
                                   sheet = "Top Qualifications (2)",
                                   skip = 1)

ex4 <- jp_top_qualifications %>%
  #rename(`Education Level` = `Minimum Education Level`, `Job Postings` = `Unique Postings (minimum)`) %>%
  rename(postings = `Postings with Qualification`) %>%
  mutate(`Percentage of Job Postings` = paste0(round(postings/jb_nb, 2)*100, "%")) %>%
  rename(`License or Certification` = Qualification, `Job Postings` = postings) %>%
  head(10)

e_4_ft <- exhibit_345_func(ex4)

exhibit_doc_func(e_4_ft)


jp_top_qualifications_2 <- jp_top_qualifications %>% head(2)

###### Document officeR_007jobpostings_workexp_bdp.R
jp_sheets <- readxl::excel_sheets(bp_job_postings)
jp_ad_count_sheet <- jp_sheets == ("Job Postings Top Occs")
jp_ad_count <- read_excel(job_postings_all,
                          sheet = jp_sheets[jp_ad_count_sheet],
                          skip = 2) %>% suppressMessages()

unique_postings <- sum(pull(jp_ad_count,which(str_detect(names(jp_ad_count), "Unique"))))

jp_all_sheets <- readxl::excel_sheets(job_postings_all)
jb_all_count <- jp_func(job_postings_all, "Minimum Education Breakdown")

jb_all <- sum(pull(jb_all_count, 2))


lcc_title <- pull(jp_top_qualifications_2, 1)
lcc_perc <- paste0(round(pull(jp_top_qualifications_2, 2)/jb_all,2)*100, "%")


