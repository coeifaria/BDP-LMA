

statewide_growth <- growth_perc_func(ca_job_growth)
cvml_growth <- growth_perc_func(cvml_job_growth)

cvml_college_award_counter <- function(num=T, cvml = T, count=F){
  if(cvml) {
    region_pick <- ccc_supply_cvml
  } else {
    region_pick <- ex12_1
  }
  college_award_count <- length(unique(pull(region_pick, 1)))
  if(count) {
    final_answer <- sum(pull(region_pick, `3-Year Award Average`))
  } else {
    if(length(college_award_count)==0) {
      final_answer <- "no"
    } else if (num) {
      final_answer <- college_award_count
    } else {
      final_answer <- xfun::n2w(college_award_count)
    }
  }
  return(final_answer)
}


# see exhibit8_bdp.R
d_b_1_2 <- paste0(
  is_or_are(),
  " projected to ",
  increase_or_decrease_func(statewide_growth),
  " by ",
  statewide_growth,
  " statewide through ",
  op_date_range_max,
  ". In the CVML region, employment in ",
  this_or_these(),
  " ",
  occupation_or_occupations(),
  " experienced the highest growth of ",
  d1_cvml_highest_growth_perc,
  " in ",
  d1_cvml_highest_growth_year,
  "."
)

d_b_1 <- fpar(
  ftext("Employment for ", prop = body_text_style),
  ftext(jobs, prop = body_text_style_italic),
  ftext(related_occupations_or_no(), prop = body_text_style_related_occupations),
  ftext(paste0(" ", related_occupations_helper("report"), " "), prop = body_text_style), # Space between jobs and is_or_are
  ftext(d_b_1_2, prop = body_text_style),
  fp_p = fp_par(text.align = "justify")
)

d_b_1 <- format_paragraph("Employment for ", paste0(" ", related_occupations_helper("report"), " ", d_b_1_2))
d_b_1

# see exhibit8_bdp.R
d_b_2_1 <- paste0(
  "Over the next ten years, California is projected to have approximately ",
  d2_ca_annual_openings,
  " annual job openings for ",
  this_or_these(),
  " ",
  occupation_or_occupations(),
  " with ",
  d2_cvml_annual_openings,
  " of those openings located in the CVML region."
)

d_b_2 <- fpar(
    ftext(d_b_2_1, prop = body_text_style),
    fp_p = fp_par(text.align = "justify")
  )




#entry_lev_wages
#wage_tester <- 17.1
#wage_to_dollar(wage_tester)
#format_num_func(wage_tester, "$W")
#wage_compare(T)
#increase_or_decrease_func()
#average_living_wage(cvml_counties_string,"Adult", F)
#average_living_wage(cvml_counties_string,"Adult_Infant", F)
#average_wage()
#cvml_living_wage_adult
#cvml_living_wage_adult_infant


cvml_wage_compare <- wage_compare(cvml_counties_string, "Adult", compare_wage = entry_lev_wages)

d_b_3_1 <- paste0(
 #reg_lw_count(cvml_living_wage_adult, all=T),
  " is ",
  format_num_func(entry_lev_wages, "$W"),
  " in the CVML region - ",
  format_num_func(cvml_wage_compare, "$W"),
 " than ",
 increase_or_decrease_func(cvml_wage_compare, 3),
 " ",
 increase_or_decrease_func(cvml_wage_compare, 3),
 " the CVML region's living wage."
)


d_b_3 <- format_paragraph("The entry-level hourly wage for ", d_b_3_1)


s_b_1_1 <- paste0(
  " there were ",
  cvml_college_award_counter(F, T),
  " educational providers in the region that conferred bachelor's degrees. However, institutions conferred ",
  cvml_college_award_counter(F, F, T),
  " post baccalaureate degrees in related programs."
)

s_b_1 <- fpar(
  ftext("Between ", prop = body_text_style),
  ftext(supply_years_min, prop = body_text_style),
  ftext(" and ", prop = body_text_style),
  ftext(supply_years_max, prop = body_text_style),
  ftext(s_b_1_1, prop = body_text_style)
  ,fp_p = fp_par(text.align = "justify")
)

cvml_annual_openings <- tail(pull(exhibit_10_func(f_job_growth(cvml_demand)), `Annual Openings`), 1)

g_b_1_1 <- paste0(
  "With no average annual bachelor's degrees issued in the region, ",
  cvml_college_award_counter(F, F, T),
  " post-baccalaureate degrees issued, and ",
  cvml_annual_openings,
  " projected annual job openings for "
)

cvml_supply_gap <- function(text=T){
  gap <- cvml_annual_openings-cvml_college_award_counter(F, F, T)
  if(text) {
  if(gap>0){
    final_offer <- "above"
  } else if (gap == 0) {
    final_offer <- "equal"
  } else if (gap <0 ){
    final_offer <- "below"
  }
  } else {
    final_offer <- gap
  }
  return(final_offer)
}

g_b_1_2 <- paste0(
  ", the potential supply gap is ",
  increase_or_decrease_func(cvml_annual_openings-cvml_college_award_counter(F, F, T),5),
  " the acceptable threshold by ",
  abs(cvml_supply_gap(F)),
  " jobs in the CVML region."
)

g_b_1 <- format_paragraph(g_b_1_1, g_b_1_2)

g_b_2_1 <- paste0(
  "In addition to ",
  this_or_these(),
  " ",
  occupation_or_occupations(),
  " ",
  "having a typical entry-level education of",
  a_degree_fix(pull(ca_demand, `Typical Entry Level Education`), T,T),
  ", ",
  bachelor_range_f(bachelor_range, 2),
  " of incumbent workers have completed a bachelor's degree as their highest level of education."
)

g_b_2 <- fpar(
  ftext(g_b_2_1, prop = body_text_style),
  fp_p = fp_par(text.align = "justify")
)

#### CONSTRUCT DOCUMENT

#doc1 <- read_docx("bdp_template.docx") %>%
doc1 <- read_docx() %>%
  body_add_fpar(fpar(ftext(text_date, H0_1), fp_p = fp_par(text.align = "right"))) %>% # This line is where the error occurs
  body_add_par("", style = "Normal") %>%
  body_add_fpar(fpar(ftext("KEY FINDINGS", H1), fp_p = fp_par(text.align = "left"))) %>%
  body_add_fpar(fpar(ftext("Demand:", body_text_style_bold))) %>%
  body_add_fpar(d_b_1, style = "List Paragraph") %>%
  body_add_fpar(d_b_2, style = "List Paragraph") %>%
  body_add_fpar(d_b_3, style = "List Paragraph") %>%
  body_add_fpar(fpar(ftext("Supply:", body_text_style_bold))) %>%
  body_add_fpar(s_b_1, style = "List Paragraph") %>%
  body_add_fpar(fpar(ftext("Gap Analysis:", body_text_style_bold))) %>%
  body_add_fpar(g_b_1, style = "List Paragraph") %>%
  body_add_fpar(g_b_2, style = "List Paragraph")

# Save the document
print(doc1, target = "officeR_001keyfindings_bdp.docx")
