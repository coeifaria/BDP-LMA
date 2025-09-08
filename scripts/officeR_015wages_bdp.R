wage_compare_formatted_func <- function(soc_codes){
  wage_list <- list()
  x_wages <- prepare_data_ex13(test_demand_data(SOC))
  x_wages$region <- "CVML"
  x_wages_ex13 <- exhibit13_func(x_wages)

  wages_above_infant_ind <- which(c(wage_compare(adult_or_infant = "Adult_Infant", compare_wage = x_wages_ex13$`Entry-Level Hourly Earnings`))>0)
  wages_above_infant_jobs <- pull(x_wages_ex13, Description)[wages_above_infant_ind]
  wages_above_adult_ind <- which(c(wage_compare(adult_or_infant = "Adult", compare_wage = x_wages_ex13$`Entry-Level Hourly Earnings`))>0)
  intersected_adult_infant <- (wages_above_adult_ind %in% wages_above_infant_ind)

  if(all(wages_above_adult_ind==wages_above_infant_ind)) {
    intersected_adult_infant <- wages_above_adult_ind
  } else {
  intersected_adult_infant <- !(wages_above_adult_ind %in% wages_above_infant_ind)
  }
  wages_above_adult_jobs <- pull(x_wages_ex13, Description)[intersected_adult_infant]

  wage_list[["Adult_Infant"]] <- wages_above_infant_jobs
  wage_list[["Adult"]] <- wages_above_adult_jobs
  wage_list[["Adult_count"]] <- length(wages_above_adult_ind)
  wage_list[["infant_count"]] <- length(wages_above_infant_ind)
  wage_list[["data_step1"]] <- test_demand_data(soc_codes)
  wage_list[["data_step2"]] <- x_wages
  wage_list[["data_step3"]] <- x_wages_ex13
  wage_list[["below_lw"]] <- nrow(x_wages_ex13)-length(wages_above_adult_ind)
  return(wage_list)
}

wages_paragraph_0 <- paste0(
  "This report considers the entry-level hourly wages for ",
  jobs,
  related_occupations_or_no(),
  " as they relate to the region’s living wage. These figures represent the high and low range of regional living wages. Entry-level hourly earnings are represented by the 25th percentile, which indicates 25% of workers in that occupation earn less than that amount while 75% earn more. "
)

wages_paragraph_1 <- paste0(
  "Entry-level hourly wages for ",
  reg_lw_count(cvml_living_wage_adult_infant, number=F, all=T),
  " ",
  jobs,
  related_occupations_or_no(),

  #"Electrical and Electronic Engineering Technologists and Technicians, Engineering Technologists and Technicians, Except Drafters, All Other, Soil and Plant Scientists and Agricultural Technicians",
  " are all above the CVML region’s living wage for one adult ($16.48)",
  #but ",
  #"Soil and Plant Scientists and Agricultural Technicians",
  " are below the living wage for one adult with one infant-aged child ($33.49). Exhibit 13 shows the wage range for ",
  this_or_these(), " ",
  occupation_or_occupations(),
  " in the CVML region and its comparison to the region’s living wages. "
  #"Note: there is insufficient data for Agricultural Engineers."
  )

wages_t <- wage_compare_formatted_func(SOC)

wages_paragraph_2_00 <- paste0(
  "Entry-level hourly wages for ",
  reg_lw_count(cvml_living_wage_adult_infant, number=F, all=T),
  " ")



wages_paragraph_2_01_1 <- paste0(
  " are above the CVML region’s living wage for one adult ",
  format_num_func(average_living_wage(cvml_counties_string, "Adult", char=F), "$W", T,F)

)

wages_paragraph_2_01_2 <- paste0(
  " are above the living wage for one adult with one infant-aged child ",
  format_num_func(average_living_wage(cvml_counties_string, "Adult_Infant", char=F), "$W", T,F)
)



wages_paragraph_2_01_3 <- paste0(". Exhibit 13 shows the wage range for ",
                                 this_or_these(), " ",
                                 occupation_or_occupations(),
                                 " in the CVML region and its comparison to the region’s living wages. "
                                 #"Note: there is insufficient data for Agricultural Engineers."
)
wages_paragraph_2_01_2 <- paste0(wages_paragraph_2_01_2, wages_paragraph_2_01_3)
wages_paragraph_2_01_11 <- format_sentence(wages_paragraph_2_00, wages_paragraph_2_01_1, format_soc_titles(wages_t$Adult))
wages_paragraph_2_02_11 <- format_sentence(" and ", wages_paragraph_2_01_2, format_soc_titles(wages_t$Adult_Infant))


wage_list_test <- wage_compare_formatted_func(SOC)

if(wage_list_test$Adult_count==wage_list_test$infant_count) {
  wages_paragraph_2_03_11 <- format_paragraph(wages_paragraph_2_00, wages_paragraph_2_01_2, format_soc_titles(wages_t$Adult))
} else {
  wages_paragraph_2_03_11 <- format_sentence_to_paragraph(append(wages_paragraph_2_01_11, wages_paragraph_2_02_11))
}





wages_paragraph_1_00 <- format_paragraph("This report considers the entry-level hourly wages for ",
                 " as they relate to the region’s living wage. These figures represent the high and low range of regional living wages. Entry-level hourly earnings are represented by the 25th percentile, which indicates 25% of workers in that occupation earn less than that amount while 75% earn more. ")


#wages_paragraph_2_02 <- format_paragraph(wages_paragraph_2_00,wages_paragraph_2_01)


#doc15 <- read_docx("bdp_template.docx") %>%
doc15 <- read_docx() %>%
  body_add_fpar(fpar(ftext("Wages", H2), fp_p = fp_par(text.align = "left"))) %>%
  body_add_par("") %>%
  body_add_fpar(wages_paragraph_1_00) %>%
  body_add_par("") %>%
  body_add_fpar(fpar(ftext("CVML Wages", H2), fp_p = fp_par(text.align = "left"))) %>%
  body_add_par("") %>%
  body_add_fpar(wages_paragraph_2_03_11) %>%
  body_add_par("") %>%
  body_add_fpar(
    fpar(
      ftext("Exhibit 13: Wages by Occupation in CVML ", H3),
      ftext(jobs, H3),
      ftext(related_occupations_or_no(), H3_italicized),
      fp_p = fp_par(text.align = "center"))
  ) %>%
  body_add_docx("ex13_linked.docx")

print(doc15, "officeR_015wages.docx")
