

#soc_titles_formatted <- format_soc_titles(soc_code_titles_w_soc_code, addmore = T)

#soc_titles_w_codes_func(soc_tester_titles_w_codes)
statewide_analysis_paragraph_0_1 <- paste0("This section presents a statewide overview of ",
                                           "the"
                                           #this_or_these()
                                           , " ", occupation_or_occupations(),
                                           ifelse(two_or_more_soc>1, ": ", " ")
                                           )
statewide_analysis_paragraph_0_2 <- " focusing on the following key areas: entry-level education requirements, desired licenses and certifications, work experience expectations, salary differentials by education level, and long-term job projections. "

#statewide_analysis_paragraph_0 <- list(ftext("This section presents a statewide overview of the occupations: ", body_text_style)) %>%
#  append(soc_titles_formatted) %>%
#  append(
#      list(
#        ftext(statewide_analysis_paragraph_0_2, body_text_style),
#        fp_p = fp_par(text.align = "justify")
#          )
#        )

#statewide_analysis_paragraph <- do.call(fpar, statewide_analysis_paragraph_0)

statewide_analysis_paragraph <- format_paragraph(statewide_analysis_paragraph_0_1, statewide_analysis_paragraph_0_2)

summary_1_table_func <- function() {
  final_offer <- cvml_demand %>%
    rename(Demand = `Avg. Annual Openings`) %>%
    mutate(weight = round((Demand/sum(Demand))*100)) %>%
    select(SOC, Description, Demand, `Typical Entry Level Education`, weight) %>%
    left_join(
      exhibit2_4 %>%
        rename(Description = field), #%>%
      #  select(Description, `Bachelor's Degree`),
      by = c("Description")) %>%
    mutate(non_bach = 1-`Bachelor's Degree`)
  return(final_offer)
}


ed_att <- summary_1_table_func() %>%
  pivot_longer(
    cols = c(6:9),
    names_to = "grad_perc"
  ) %>%
  mutate(allocate = weight*value) %>%
  group_by(grad_perc) %>%
  summarize(
    most_grad = sum(allocate)
  ) %>%
  arrange(desc(most_grad)) %>%
  head(2)

bachelor_degrees <- pull(summary_1_table_func(), `Bachelor's Degree`)

if(length(bachelor_degrees)==1){
  bachelor_degrees_interval <- paste0(bachelor_degrees*100, "%")
} else {
  bachelor_degrees_interval <-  paste0(
    "between ",
    min(bachelor_degrees)*100,
    "% and ",
    max(bachelor_degrees)*100,
    "%"
    )

}

entry_level_education_requirements_paragraph <-
  paste0("The Bureau of Labor Statistics (BLS) lists",
         a_degree_fix(pull(ed_att, 1)[1], a = T, b = T), " and", a_degree_fix(pull(ed_att, 1)[2], a = T, b = T),
         " as the typical entry-level education for the ",
         occupation_or_occupations(),
         " of interest. National-level educational attainment data indicates that ",
         bachelor_degrees_interval,
         " of incumbent workers in ",
         this_or_these(),
         " ",
         occupation_or_occupations(),
         " have completed a bachelor’s degree as their highest level of education.  Exhibit 2 shows the educational attainment for ",
         this_or_these(),
         " ",
         occupation_or_occupations(),
         "."
)




doc4 <- read_docx("bdp_template.docx") %>%
  body_add_fpar(fpar(ftext("Statewide Analysis", H1), fp_p = fp_par(text.align = "left"))) %>%
  body_add_par("") %>%
  body_add_fpar(statewide_analysis_paragraph) %>%
  body_add_par("") %>%
  body_add_fpar(fpar(ftext("Entry-Level Education Requirements", H2), fp_p = fp_par(text.align = "justify"))) %>%
  body_add_par("") %>%
  body_add_fpar(fpar(ftext(entry_level_education_requirements_paragraph, body_text_style), fp_p = fp_par(text.align = "justify"))) %>%
  body_add_par("") %>%
  body_add_fpar(
    fpar(
      ftext("Exhibit 2 National-level Educational Attainment for Occupation", H3),
      fp_p = fp_par(text.align = "center")
    )
  ) %>%
  body_add_docx("ex2_linked.docx")

print(doc4, "officeR_004entryeducation.docx")
