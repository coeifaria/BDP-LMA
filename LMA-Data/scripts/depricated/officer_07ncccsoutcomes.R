
years_e11_1 <- names(ncc_supply)[str_detect(names(ncc_supply), "^20")]


ncc_completers <- ncc_supply %>%
  #filter(`TOP6 or CIP` %in% str_remove(TOP_supplementary, "\\.")) %>%
  filter(CIP %in% CIP) %>%
  select(`Institution Name`, Regions, `Econ Subregion`, starts_with("20")) %>%
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
  group_by(`Institution Name`,`Econ Subregion`) %>%
  summarize(awards = sum(awards)) %>%
  arrange(`Econ Subregion`)

ncc_top_completers <- ncc_completers %>%
  group_by(`Econ Subregion`) %>%
  filter(awards == max(awards)) %>%
  ungroup()

ncc_n_or_s_top_completers <- function(north_or_south){
  ncc_top_completers %>%
    filter(str_detect(`Econ Subregion`, north_or_south)) %>%
    mutate(`Institution Name` = paste0(`Institution Name`, " (", awards, ")")) %>%
    pull(`Institution Name`)

}


ncc_n_top_completer <- ncc_n_or_s_top_completers("North")
ncc_s_top_completer <- ncc_n_or_s_top_completers("South")

#years_e11 <- str_replace_all(str_remove_all(names(ns)[str_detect(names(ns), pattern = "20")], "Annual."), "\\.", "-")
#years_e11 <- str_replace_all(str_remove_all(names(ns)[str_detect(names(ns), pattern = "20")], "Annual."), "\\.", "-")

years_e11_1 <- names(rename_academic_year_columns(ncc_supply))[str_detect(names(ncc_supply), "^20")]

text_h1 <- "Non-Community College Supply:"
text_b_1 <- paste0("Exhibit 11 shows the annual and three-year average number of awards conferred by Non-community colleges in the related CIP codes: ", CIP_string) #create a bullet point loop for this



if(length(params$CIP) > 0){
  text_b_1_1 <- paste0(
    "No awards were conferred for the following CIP codes: ",
    paste0(params$CIP, collapse = ", ")
  )} else {
    text_b_1_1 <- ""
  }




# Dynamically build the sentence depending on what's available
if (length(ncc_n_top_completer) > 0 && length(ncc_s_top_completer) > 0) {
  region_text <- paste0(
    "The ncc colleges with the most completions in the North and South CVML regions are ",
    ncc_n_top_completer, " and ", ncc_s_top_completer, "."
  )
} else if (length(ncc_n_top_completer) > 0) {
  region_text <- paste0(
    "The ncc college with the most completions was in the North CVML region: ",
    ncc_n_top_completer, "."
  )
} else if (length(ncc_s_top_completer) > 0) {
  region_text <- paste0(
    "The ncc college with the most completions was in the South CVML region: ",
    ncc_s_top_completer, "."
  )
} else {
  region_text <- "No ncc colleges in the North or South CVML regions reported completions."
}

text_b_1_1_1 <- region_text


text_b_1_2 <- "Over the past 12 months, there were no other related program recommendation requests from regional community colleges."
text_h3_1 <- paste0("Exhibit 11: Non Community College Awards (Certificates and Degrees) ", years_e11_1[1], " through ", years_e11_1[3])

text_b_2 <- paste0(
  "Exhibit 11 shows the annual ncc average community college awards by type from ", years_e11_1[1], " through ", years_e11_1[3], ". Of the ",sum(ex11_1$Completions), " awards, ", round(ex11_1$Completions[1]/sum(ex11_1$Completions),2)*100, "% (", ex11_1$Completions[1], ") ", "were for ", as.character(ex11_1$`Award.Type`[1]), ", ", round(ex11_1$Completions[2]/sum(ex11_1$Completions),2)*100, "% (", ex11_1$Completions[2], ") ",
  "were for ", as.character(ex11_1$`Award.Type`[2]),", and ", paste0(round(ex11_1$Completions[3]/sum(ex11_1$Completions),2)*100, "% (", ex11_1$Completions[3], ")"),
  " were for ",
  as.character(ex11_1$`Award.Type`[3])
)



text_h3_2 <- paste0("Exhibit 11a: NCV/NML Subregional Non-Community College Awards, ", paste0(str_sub(years_e11_1[1], 1, 4), "-", str_sub(years_e11_1[3], 6, 9)))
text_h3_3 <- paste0("Exhibit 11b: SCV/SML Subregional Non-Community College Awards, ", paste0(str_sub(years_e11_1[1], 1, 4), "-", str_sub(years_e11_1[3], 6, 9)))

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
  result <- "AI analysis not requested."
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
header3_fpar2 <- fpar(ftext(text_h3_2, H3),  fp_p = fp_par(text.align = "center"))


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

# Corrected block_2
#block_2 <- fpar(
#  ftext(text_h3_1_2, H3)#,
#external_img("exhibit10_NCC_North.png", width = img_size_11_n$width, height = img_size_11_n$height),
#external_img("exhibit10_NCC_South.png", width = img_size_11_s$width, height = img_size_11_s$height)
#fp_p = fp_par(text.align = "center")
#)


#### CONSTRUCT DOCUMENT
#doc2 <- officer::read_docx()
# Construct document
#doc <- doc %>%
doc8 <- read_docx("lma_blank_template.docx") %>%
  body_add_fpar(fpar(ftext(text_h1, H2))) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(fpar(ftext(text_b_1, prop = body_text_style))) %>%
  body_add_fpar(fpar(ftext(text_b_1_1, prop = body_text_style))) %>%
  body_add_fpar(fpar(ftext(text_b_1_1_1, prop = body_text_style))) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(block_1) %>%
  body_add_fpar(fpar(ftext(text_h3_2, H3),  fp_p = fp_par(text.align = "center"))) %>%
  body_add_flextable(exhibit10_ncc_north_table_ft) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(fpar(ftext(text_h3_3, H3),  fp_p = fp_par(text.align = "center"))) %>%
  body_add_flextable(exhibit10_ncc_south_table_ft) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(body_fpar_2) %>%
  body_add_par("", style = "Normal")

print(doc8, target = "office_07ncccsoutcomes.docx")
#print(doc3, "test3.docx")
#end 05CC Supply
