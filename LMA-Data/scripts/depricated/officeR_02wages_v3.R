
#reg_demand_ex("NCV/NML") %>%  filter(lev == "Entry-Level Hourly Earnings")%>%
#  mutate(Description = str_replace_all(Description, "\n", " "))
reg_demand("CV")
reg_demand("A")

over_lw_n <- reg_demand_ex("N") %>% filter(lev == "Entry-Level Hourly Earnings") %>%
  filter(earnings >= as.numeric(str_remove(ncv_living_wage, "\\$")))

wages_sentence_1 <- paste0(
  str_to_title(reg_lw_count("N")),
       " of the ",
       xfun::n2w(length(SOC)),
       " occupations have an entry-level hourly wage that is above the living wage for one adult in the NCV/NML subregion (",ncv_living_wage, ").")

wages_sentence_2 <- paste0("The ",N_S_CVML("N"), " average wage for these occupations is ", formatted_wage(regional_ave_wage("N")),
                           ", which is", ifelse(regional_ave_wage("N") >= regional_ave_wage("A"), " above", " below"),
                           " the the average statewide wage of ", formatted_wage(regional_ave_wage("A")), ".")

wages_sentence_3 <- paste0("Exhibit ", N_S_CVML_ex("N"), " shows the wage range for ", str_to_lower(jobs), " and how they compare to the ", region_region("N"), " living wage.")


wages_func <- function(reg){

  reg_lw <- if (reg == "N") {
    ncv_living_wage
  } else if (reg == "S") {
    scv_living_wage
  } else if (reg == "C") {
    cvml_living_wage
  }
  wages_sentence_1 <- paste0(
    str_to_title(((reg_lw_count(reg)))),
                             " of the ",
                             xfun::n2w(length(SOC)),
                             " occupations have an entry-level hourly wage that is above the living wage for one adult in the ",
      region_region(reg), " (",reg_lw, "). ")

  wages_sentence_2 <- paste0("The ",N_S_CVML(reg), " average wage for these occupations is ", formatted_wage(regional_ave_wage(reg)),
                             ", which is", ifelse(regional_ave_wage(reg) >= regional_ave_wage("A"), " above", " below"),
                             " the average statewide wage of ", formatted_wage(regional_ave_wage("A")), ". ")

  wages_sentence_3 <- paste0("Exhibit ", N_S_CVML_ex(reg), " shows the wage range for ", str_to_lower(jobs), " and how they compare to the ", region_region(reg), "'s living wage.")
paste0(wages_sentence_1, wages_sentence_2, wages_sentence_3)
}

#N_S_CVML("C")
#wages_func("N")
#wages_func("S")
#wages_func("C")
#exhibit4_paragraph_text("NCV/NML")
#```{r officer_02wage_v2s, chunk_progress=TRUE}
text_h2 <- "Wages:"

text_b_1 <- paste0(
  "The labor market endorsement in this report considers the entry-level hourly wages for ",
  jobs,
  " as they relate to the subregions and region's living wage. NCV/NML, SCV/SML, and CVML wages are included below to provide a complete analysis of the region.")
#paste0(jobs,
#text_b_2 <- paste(ex4a_wages_text, ex4a_wages_text2, "Exhibit 4a shows the wage range for", jobs,"and how it compares to the NCV/NML subregion's living wage.", sep = " ")
#text_b_3 <- paste(ex4b_wages_text, ex4b_wages_text2, "Exhibit 4b shows the wage range for", jobs,"and how it compares to the SCV/SML subregion's living wage.", sep = " ")
#text_b_4 <- paste(ex4c_wages_text, ex4c_wages_text2, "Exhibit 5 shows the wage range for", jobs,"and how it compares to the CVML subregion's living wage.", sep = " ")

#text_b_2 <- exhibit4_paragraph_text("NCV/NML")
#text_b_3 <- exhibit4_paragraph_text("SCV/SML")
#text_b_4 <- exhibit4_paragraph_text("CVML")



text_b_2 <- wages_func("N")
text_b_3 <- wages_func("S")
text_b_4 <- wages_func("C")

#"Exhibit 4a shows the wage range for", jobs,"and how it compares to the NCV/NML subregion's living wage.
##ex4 <- exhibit4_scv %>%  filter(str_detect(lev, pattern = "Entry")) %>%  filter(earnings != 0) %>%  mutate(gap = NULL)

#prompt_text <-   paste("Write a coherent response that says the typical entry level earnings for the following jobs and say if they are higher or lower than the living wages for one #adult in ", requesting_region, "which is", scv_living_wage, ". Only respond with one or two sentences:", sep = " ")

#prompt <- paste(prompt_text, paste(as.character=(ex4), collapse= ", "))
#text_b_2_2 <- call_claude_api_for_table(prompt)

text_h3_1 <- paste0("Exhibit 4a: Wages by Occupation in NCV/NML")
text_h3_2 <- paste0("Exhibit 4b: Wages by Occupation in SCV/SML")
text_h3_3 <- paste0("Exhibit 5: Wages by Occupation in CVML")

#text_b_3_2 <- call_claude_api_for_table(
#  paste("Complete the following sentence with the given data:",
#        region,
#        "average wage for", jobs, "is",
#        paste(as.character(scv_demand[(!is.na(scv_demand$Description)&scv_demand$`Avg. Hourly Earnings`!= 0),c("Description", "Avg. Hourly Earnings")]), collapse = ","),
#        "which is ABOVE OR BELOW",
#        "the", opposite_region,  "average, which is",
#        paste(as.character(ncv_demand[which((!is.na(ncv_demand$Description)&ncv_demand$`Avg. Hourly Earnings`!= 0)),c("Description", "Avg. Hourly Earnings")]), collapse = ","),
#        ". Only respond with one or two sentences:", sep = " "))

#text_b_4 <- call_claude_api_for_table(
#  paste(
#    "Compare the following wages between California and the", region, " region.",
#    "The average hourly earnings in ", region, "for", paste(as.character(scv_demand[(!is.na(scv_demand$Description)& scv_demand$`Avg. Hourly Earnings`!= 0),c("Description", "Avg. Hourly #Earnings")]), collapse = ","),
#    "which is ABOVE OR BELOW",
#    "the statewide average, which is ",
#    paste(as.character(ca_demand[which(!is.na(ca_demand$Description) & ca_demand$`Avg. Hourly Earnings` != 0),c("Description", "Avg. Hourly Earnings")]), collapse = ","),
#    "where the statewide living wage is", ca_living_wage,
#    ". Only respond with one or two sentences:", sep = " ")
#  )

#img_size_4A <- adjust_image_size("exhibit4A.png", dpi = 300)
#img_size_4B <- adjust_image_size("exhibit4B.png", dpi = 300)
#img_size_5 <- adjust_image_size("exhibit5.png", dpi = 300)

#### Create Components

#header2_fpar <- fpar(ftext(text_h2, H2))
#body_fpar_1  <- fpar(ftext(text_b_1, prop = body_text_style))
body_fpar_2  <- fpar(ftext(text_b_2, prop = body_text_style))
#body_fpar_2_2  <- fpar(ftext(text_b_2_2, prop = highlighted_text))
body_fpar_3  <- fpar(ftext(text_b_3, prop = body_text_style))
#body_fpar_3_2  <- fpar(ftext(text_b_3_2, prop = highlighted_text))
body_fpar_4  <- fpar(ftext(text_b_4, prop = highlighted_text))

header3_fpar1 <- fpar(ftext(text_h3_1, H3),  fp_p = fp_par(text.align = "center"))
#img_exhibit4A <- fpar(external_img("exhibit4A.png", width = img_size_4A$width, height = img_size_4A$height), fp_p = fp_par(text.align ="center"))
header3_fpar2 <- fpar(ftext(text_h3_2, H3),  fp_p = fp_par(text.align = "center"))
#img_exhibit4B <- fpar(external_img("exhibit4B.png", width = img_size_4B$width, height = img_size_4B$height), fp_p = fp_par(text.align ="center"))
header3_fpar3 <- fpar(ftext(text_h3_3, H3),  fp_p = fp_par(text.align = "center"))
#img_exhibit5 <- fpar(external_img("exhibit5.png", width = img_size_5$width, height = img_size_5$height), fp_p = fp_par(text.align ="center"))

# Headers and images combined in single `fpar` blocks
block_1 <- fpar(
  ftext(text_h3_1, H3),
  fp_p = fp_par(text.align = "center")#,
  #external_img("exhibit4A.png", width = img_size_4A$width, height = img_size_4A$height)
)

block_2 <- fpar(
  ftext(text_h3_2, H3),
  fp_p = fp_par(text.align = "center")#,
  #external_img("exhibit4B.png", width = img_size_4B$width, height = img_size_4B$height)
)

block_3 <- fpar(
  ftext(text_h3_3, H3),
  fp_p = fp_par(text.align = "center")#,
  #external_img("exhibit5.png", width = img_size_5$width, height = img_size_5$height)
)

doc3 <- read_docx("lma_blank_template.docx") %>%
#doc3 <- read_docx("ex4a_linked.docx") %>%
  body_add_fpar(fpar(ftext(text_h2, H2))) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(fpar(ftext(text_b_1, prop = body_text_style))) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(body_fpar_2) %>%
  #body_add_gg(exhibit4A) %>%
  #body_add_fpar(body_fpar_2_2) %>%
  #body_add_par("", style = "Normal") %>%
  body_add_fpar(block_1) %>%
  body_add_docx(src = "ex4a_linked.docx", pos = "after") %>%
  body_add_fpar(body_fpar_3) %>%
  #body_add_gg(exhibit4B) %>%
  body_add_par("", style = "Normal") %>%
  #body_add_fpar(body_fpar_3_2) %>%
  #body_add_par("", style = "Normal") %>%
  body_add_fpar(block_2) %>%
  body_add_docx(src = "ex4b_linked.docx", pos = "after") %>%
  body_add_fpar(body_fpar_4) %>%
  #body_add_gg(exhibit5) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(block_3) %>%
  body_add_docx(src = "ex5_linked.docx", pos = "after")

print(doc3, "officeR_02wages.docx")
