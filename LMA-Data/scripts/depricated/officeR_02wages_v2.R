reg_demand <- function(x)if (str_sub(x, 1,1)=="N"){
  ncv_demand
} else if (str_sub(x, 1,1)=="S"){
  scv_demand
} else if (str_sub(x, 1,1)=="C"){
  cvml_demand
} else if (str_sub(x, 1,1)=="CA"){
  ca_demand
}

reg_demand_ex <- function(x)if (str_sub(x, 1,1)=="N"){
  exhibit4_ncv
} else if (str_sub(x, 1,1)=="S"){
  exhibit4_scv
} else if (str_sub(x, 1,1)=="C"){
  exhibit5_cvml
}

#reg_demand_ex("NCV/NML") %>%  filter(lev == "Entry-Level Hourly Earnings")%>%
#  mutate(Description = str_replace_all(Description, "\n", " "))



exhibit4_paragraph_text <- function(region_ex4) {
  intro <- "The typical entry-level hourly wage for "

  ex_text <- reg_demand_ex(region_ex4) %>%
    filter(lev == "Entry-Level Hourly Earnings") %>%
    mutate(Description = str_replace_all(Description, "\n", " "))
  # Ensure living wage is consistent
  living_wage_val <- unique(ex_text$living_wage)
  region_type <- ifelse(str_detect(region_ex4, "\\/"), " subregion", " region")


  if (nrow(ex_text) == 1) {
    desc <- ex_text$Description[1]
    wage <- ex_text$earnings[1]
    status <- ifelse(wage >= living_wage_val, "above", "below")

    sentence <- paste0(
      intro, desc, " ($", wage, ") is ", status,
      " the living wage for one adult in the ", region_ex4, region_type,
      " (", living_wage_val, ")."
    )

    ending <- paste0(" The ", region_ex4, " average wage for this occupation is $", average_wage(reg_demand(region_ex4)),
                     " which is", ifelse(average_wage(reg_demand(region_ex4)) >= average_wage(ca_demand), " above", " below"),
                     " the the average statewide wage of $", average_wage(ca_demand), ".")

  } else {
    above <- ex_text %>% filter(earnings >= living_wage_val)
    below <- ex_text %>% filter(earnings < living_wage_val)

    format_list <- function(descs, wages) {
      n <- length(descs)
      parts <- paste0(descs, " ($", wages, ")")
      if (n == 1) {
        return(parts[1])
      } else if (n == 2) {
        return(paste(parts, collapse = " and "))
      } else {
        return(paste0(paste(parts[1:(n-1)], collapse = ", "), ", and ", parts[n]))
      }
    }

    sentence_parts <- c()

    if (nrow(above) > 0) {
      above_sentence <- paste0(
        format_list(above$Description, above$earnings),
        " ", ifelse(nrow(above) == 1, "is", "are"),
        " above the living wage for one adult in the ", region_ex4, region_type,
        " ($", living_wage_val, ")"
      )
      sentence_parts <- c(sentence_parts, above_sentence)
    }

    if (nrow(below) > 0) {
      below_sentence <- paste0(
        format_list(below$Description, below$earnings),
        " ", ifelse(nrow(below) == 1, "is", "are"),
        " below the living wage for one adult in the ", region_ex4, region_type,
        " ($", living_wage_val, ")"
      )
      sentence_parts <- c(sentence_parts, below_sentence)
    }

    sentence <- paste(sentence_parts, collapse = "; ")
    sentence <- paste0(sentence, ".")

    ending <- paste0(" The ", region_ex4, " average wage for these occupations is $", average_wage(reg_demand(region_ex4)),
                     " which is", ifelse(average_wage(reg_demand(region_ex4)) >= average_wage(ca_demand), " above", " below"),
                     " the the average statewide wage of $", average_wage(ca_demand), ".")
  }

  return(paste0(sentence, ending))
}

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

text_b_2 <- paste(exhibit4_paragraph_text("NCV/NML"), "Exhibit 4a shows the wage range for", jobs,"and how it compares to the NCV/NML subregion's living wage.", sep = " ")
text_b_3 <- paste(exhibit4_paragraph_text("SCV/SML"), "Exhibit 4b shows the wage range for", jobs,"and how it compares to the SCV/SML subregion's living wage.", sep = " ")
text_b_4 <- paste(exhibit4_paragraph_text("CVML"), "Exhibit 5 shows the wage range for", jobs,"and how it compares to the CVML subregion's living wage.", sep = " ")

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

doc <- doc %>%
  body_add_fpar(fpar(ftext(text_h2, H2))) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(fpar(ftext(text_b_1, prop = body_text_style))) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(body_fpar_2) %>%
  body_add_par("", style = "Normal") %>%
  #body_add_fpar(body_fpar_2_2) %>%
  #body_add_par("", style = "Normal") %>%
  body_add_fpar(block_1) %>%
  body_add_fpar(body_fpar_3) %>%
  body_add_par("", style = "Normal") %>%
  #body_add_fpar(body_fpar_3_2) %>%
  #body_add_par("", style = "Normal") %>%
  body_add_fpar(block_2) %>%
  body_add_fpar(body_fpar_4) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(block_3)

