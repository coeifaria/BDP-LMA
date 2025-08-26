#```{r officer_05ccsupply, chunk_progress=TRUE}
#### Text
#ccc_supply_v2

completers <- ccc_supply_v2 %>%
  #filter(`TOP6 or CIP` == str_remove(TOP, "\\.")) %>%
  filter(`TOP6 or CIP` %in% str_remove(TOP_supplementary, "\\.")) %>%
  select(`institution name`, Regions, `Econ Subregion`, starts_with("20")) %>%
  #mutate(join_id = str_remove_all(`institution name`, pattern = "(City|College)")) %>%
  filter(
    str_detect(Regions, "Central") &
      str_detect(`Econ Subregion`, "Valley")
  ) %>%
  distinct() %>%
  #inner_join(colleges_df[,c("College", "Classification")], by = c("join_id"= "College")) %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "acadyr",
               values_to = "awards") %>%
  distinct() %>%
  group_by(`institution name`,`Econ Subregion`) %>%
  summarize(awards = sum(awards)) %>%
  arrange(`Econ Subregion`)

top_completers <- completers %>%
  group_by(`Econ Subregion`) %>%
  filter(awards == max(awards)) %>%
  ungroup()

n_or_s_top_completers <- function(north_or_south){
  top_completers %>%
    filter(str_detect(`Econ Subregion`, north_or_south)) %>%
    mutate(`institution name` = paste0(`institution name`, " (", awards, ")")) %>%
    pull(`institution name`)

}

exhibit10_north

#top_completers <- ns %>%
#  mutate(`Award.Type` = str_trim(ifelse(str_detect(`Award.Type`, pattern = "Associate"), "Associate Degree", `Award.Type`), side = "both")) %>%
#  pivot_longer(cols = starts_with("Annual"),
#               values_to = "Completions",
#               names_to = "AY") %>%
#  group_by(region, `College.Name`) %>%
#  summarise(Completions = sum(as.numeric(Completions), na.rm = T)) %>%
#  filter(Completions == max(Completions))

#n_top_completer <- top_completers %>%
#  filter(region == "ncvml") %>%
#  pull(College.Name)

#s_top_completer <- top_completers %>%
#  filter(region == "scvml") %>%
#  pull(College.Name)

n_top_completer <- n_or_s_top_completers("North")
s_top_completer <- n_or_s_top_completers("South")

#years_e11 <- str_replace_all(str_remove_all(names(ns)[str_detect(names(ns), pattern = "20")], "Annual."), "\\.", "-")
#years_e11 <- str_replace_all(str_remove_all(names(ns)[str_detect(names(ns), pattern = "20")], "Annual."), "\\.", "-")

years_e11 <- names(ccc_supply_v3)[str_detect(names(ccc_supply_v3), "^20")]
years_e11_1 <- names(ncc_supply)[str_detect(names(ncc_supply), "^20")]

if (str_sub(region_acro,1,1)=="N"){
  college_awards_conferred <- unique(ccc_supplyn$College)
} else if(str_sub(region_acro,1,1)=="S") {
  college_awards_conferred <- unique(ccc_supplys$College)
}



# Define the vector of college names
# Assuming college_awards_conferred is your original list/vector
# If college_awards_conferred[1] truly holds *another* vector, you'd use that.
# For this example, let's assume college_awards_conferred is the vector itself.


# Function to format the list into a sentence fragment
format_college_list <- function(colleges) {
  num_colleges <- length(colleges)

  if (num_colleges == 0) {
    # Handle empty list
    return("")
  } else if (num_colleges == 1) {
    # Handle single college
    return(colleges[1])
  } else if (num_colleges == 2) {
    # Handle two colleges with "and"
    return(paste(colleges[1], "and", colleges[2]))
  } else {
    # Handle three or more colleges
    # Get all colleges except the last one
    first_part <- colleges[1:(num_colleges - 1)]
    # Get the last college
    last_part <- colleges[num_colleges]
    # Paste the first part together with ", "
    joined_first_part <- paste(first_part, collapse = ", ")
    # Combine the joined first part, ", and ", and the last part
    final_string <- paste0(joined_first_part, ", and ", last_part)
    return(final_string)
  }
}

# Format the list
formatted_list_college_awards <- format_college_list(college_awards_conferred)


# --- Testing edge cases ---
#print(format_college_list(c("Fresno City College"))) # Test with one
#print(format_college_list(c("Clovis Community", "Fresno City College"))) # Test with two
#print(format_college_list(character(0))) # Test with zero

text_h1 <- "Educational Supply"
text_h2 <- "Community College Supply:"
text_b_1 <- paste0(
  #"Exhibit 10a and 10b show the annual and three-year average number of awards conferred by community colleges in the related TOP codes: ",
  "Exhibit 10a and 10b show the annual and three-year average number of awards conferred by community colleges in the programs that have historically trained for the occupations included in this report. ",
  stringr::str_to_title(xfun::n2w(length(college_awards_conferred))), " community colleges in the region conferred awards - ", formatted_list_college_awards, "."
  )

#  top_code_title_w_top_code_supplementary) #create a bullet point loop for this


if(length(params$TOP_supplementary[-no_awards_ind]) > 0){
  text_b_1_1 <- paste0(
    "No awards were conferred for the following TOP codes: ",
    paste0(params$TOP_supplementary[-no_awards_ind], collapse = ", ")
    )} else {
    text_b_1_1 <- ""
}




# Dynamically build the sentence depending on what's available
if (length(n_top_completer) > 0 && length(s_top_completer) > 0) {
  region_text <- paste0(
    "The colleges with the most completions in the North and South CVML regions are ",
    n_top_completer, " and ", s_top_completer, "."
  )
} else if (length(n_top_completer) > 0) {
  region_text <- paste0(
    "The college with the most completions was in the North CVML region: ",
    n_top_completer, "."
  )
} else if (length(s_top_completer) > 0) {
  region_text <- paste0(
    "The college with the most completions was in the South CVML region: ",
    s_top_completer, "."
  )
} else {
  region_text <- "No colleges in the North or South CVML regions reported completions."
}
text_b_1_1_1 <- region_text


text_b_1_2 <- "Over the past 12 months, there were no other related program recommendation requests from regional community colleges."
#text_h3_1 <- paste0("Exhibit 10: Regional Community College Awards (Certificates and Degrees) ", years_e11[1], " through ", years_e11[3])
text_h3_1 <- paste0("Exhibit 10a: NCV/NML Community College Awards (Certificates and Degrees) ", years_e11[1], " through ", years_e11[3])
text_h3_11 <- paste0("Exhibit 10b: SCV/SML Community College Awards (Certificates and Degrees) ", years_e11[1], " through ", years_e11[3])

text_b_2 <- paste0(
  "Exhibit 11 shows the annual average community college awards by type from ",
  years_e11[1],
  " through ",
  years_e11[3],
  ". Of the ",
  sum(ex11_1$Completions), " awards conferred in the ", region_region(str_sub(region_acro, 1, 1)), ", ", round(ex11_1$Completions[1]/sum(ex11_1$Completions),2)*100, "% (", ex11_1$Completions[1], ") ", "were for ", as.character(ex11_1$`Award.Type`[1]), ", ", round(ex11_1$Completions[2]/sum(ex11_1$Completions),2)*100, "% (", ex11_1$Completions[2], ") ",
  "were for ",
  as.character(ex11_1$`Award.Type`[2]),
  ", and ",
  paste0(round(ex11_1$Completions[3]/sum(ex11_1$Completions),2)*100, "% (", ex11_1$Completions[3], ")"),
  " were for ",
  as.character(ex11_1$`Award.Type`[3])
)
text_h3_2 <- paste0("Exhibit 11: Annual Average Community College Awards (",
                    CHANGEME2_REGION2,") by Type, ",
                    paste0(str_sub(years_e11[1], 1, 4),
                           "-",
                           str_sub(years_e11[3], 1, 4)))

prompt <- "The following table shows the average annual community college awards by type. Only respond with a concise analysis of the table:"
prompt <- paste0(prompt, capture.output(print(ex11_1 %>% select(`Award.Type`, Completions, region, `TOP6 Title`))) %>% paste(collapse = "\n"))

if (use_ai) {
  result <- tryCatch({
    call_claude_api_for_table(
      prompt = prompt,
      model = "claude-3-opus-20240229",
      max_tokens = 4000
    )
  }, error = function(e) {
    paste("API call error:", e$message)
  })
} else {
  result <- "AI usage is disabled, no analysis provided."
}

text_b_2_2 <- result

#exhibit10_files <- list.files(pattern = "^exhibit10_.*\\.png$")

#img_size_10_n <- adjust_image_size("exhibit10_North.png")
#img_size_10_s <- adjust_image_size("exhibit10_South.png")
#img_size_11_n <- adjust_image_size("exhibit10_NCC_North.png")
#img_size_11_s <- adjust_image_size("exhibit10_NCC_South.png", dpi = 300)
#img_size_11B <- adjust_image_size("exhibit11B.png", dpi = 300)

#### Create Components
#footnote_1 <- ""

header1_fpar <- fpar(ftext(text_h1, H1))
header2_fpar <- fpar(ftext(text_h2, H2))
body_fpar_1  <- fpar(ftext(text_b_1, prop = body_text_style)
                     #,ftext(text_b_1_2, prop = highlighted_text)
)
header3_fpar1 <- fpar(ftext(text_h3_1, H3),  fp_p = fp_par(text.align = "center"))
header3_fpar11 <- fpar(ftext(text_h3_11, H3),  fp_p = fp_par(text.align = "center"))


#img_exhibit10_1 <- fpar(external_img("exhibit10_1.png", width = img_size_10_1$width, height = img_size_10_1$height), fp_p = fp_par(text.align = "center"))
#img_exhibit10_2 <- fpar(external_img("exhibit10_2.png", width = img_size_10_2$width, height = img_size_10_2$height), fp_p = fp_par(text.align = "center"))
#img_exhibit10_3 <- fpar(external_img("exhibit10_3.png", width = img_size_10_3$width, height = img_size_10_3$height), fp_p = fp_par(text.align = "center"))

body_fpar_2  <- fpar(ftext(text_b_2, prop = body_text_style))
body_fpar_3  <- fpar(ftext(text_b_2_2, prop = body_text_style))
#header3_fpar2 <- fpar(ftext(text_h3_2, prop = H3), run_footnote(x = footnote_1, prop = fp_text(bold = F, color = exhibit_header_color, vertical.align = "superscript")), fp_p = fp_par(text.align = "center"))
#img_exhibit11 <- fpar(external_img("exhibit11.png", width = img_size_11$width, height = img_size_11$height), fp_p = fp_par(text.align = "center"))
#img_exhibit11B <- fpar(external_img("exhibit11B.png", width = img_size_11B$width, height = img_size_11B$height), fp_p = fp_par(text.align = "center"))

# Headers and images combined in single `fpar` blocks
#block_0 <- fpar(
#  ftext(text_h2, H2),
#  fp_p = fp_par(text.align = "center"),
#  ftext(text_b_1, prop = body_text_style)
#)

# Corrected block_1
#block_1 <- fpar(
#  ftext(text_h3_1, H3),
#  external_img("exhibit10_1.png", width = img_size_10_1$width, height = img_size_10_1$height),
#  ftext("")#,
#  external_img("exhibit10_2.png", width = img_size_10_2$width, height = img_size_10_2$height),
#  ftext(""),
#  external_img("exhibit10_3.png", width = img_size_10_3$width, height = img_size_10_3$height),
#  fp_p = fp_par(text.align = "center")
#)

# Corrected block_1
block_1 <- fpar(
  ftext(text_h3_1, H3)#,
  #external_img("exhibit10_North.png", width = img_size_10_n$width, height = img_size_10_n$height),
  #external_img("exhibit10_South.png", width = img_size_10_s$width, height = img_size_10_s$height)
  #fp_p = fp_par(text.align = "center")
)

block_11 <- fpar(
  ftext(text_h3_11, H3)#,
  #external_img("exhibit10_North.png", width = img_size_10_n$width, height = img_size_10_n$height),
  #external_img("exhibit10_South.png", width = img_size_10_s$width, height = img_size_10_s$height)
  #fp_p = fp_par(text.align = "center")
)
# Corrected block_2
#block_2 <- fpar(
#  ftext(text_h3_1_2, H3)#,
  #external_img("exhibit10_NCC_North.png", width = img_size_11_n$width, height = img_size_11_n$height),
  #external_img("exhibit10_NCC_South.png", width = img_size_11_s$width, height = img_size_11_s$height)
  #fp_p = fp_par(text.align = "center")
#)


#### CONSTRUCT DOCUMENT
# Construct document
doc6 <- read_docx("lma_blank_template.docx") %>%
  body_add_fpar(fpar(ftext(text_h1, H1))) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(fpar(ftext(text_h2, H2))) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(fpar(ftext(text_b_1, prop = body_text_style))) %>%
  body_add_fpar(fpar(ftext(text_b_1_1, prop = body_text_style))) %>%
  body_add_fpar(fpar(ftext(text_b_1_1_1, prop = body_text_style))) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(block_1) %>% #Header for plots
  body_add_flextable(exhibit10_north_table_ft) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(block_11) %>%
  body_add_flextable(exhibit10_south_table_ft) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(body_fpar_2) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(body_fpar_2) %>%
  body_add_fpar(fpar(ftext(text_h3_2, prop = H3))) %>%
  body_add_docx(src = "ex11_linked.docx", pos = "after")
  #body_add_gg(exhibit11, width = 6, height = 3, res = 300)


print(doc6, "officeR_05ccsupply.docx")
#print(doc3, "test3.docx")
#end 05CC Supply
