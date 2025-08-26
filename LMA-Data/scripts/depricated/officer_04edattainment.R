#```{r officer_04edattainment, chunk_progress=TRUE}

some_college_or_aa_degree <- paste(
  apply(exhibit9_1 %>%
          filter(ifelse(lev == "Some college, no degree" | lev == "Associate's degree", 'y', 'n') == 'y') %>%
          group_by(`2023 National Employment Matrix title`) %>%
          summarize(per = round(sum(per),0)),
        1,
        function(row) paste(gsub(" ", " ", row[1]), paste0("(", row[2], "%)"), sep = " ")
  ),
  collapse = ", "
)
min_ed_2 <- readxl::read_excel(demand_postings[str_detect(demand_postings, pattern = region_acro)],
                               sheet = "Edu and Experience Break... (2)", skip = 1)

names(min_ed_2) <- c("min_ed", "unique_posts_min", "unique_posts_max", "min_percent_total")

second_place <- min_ed_2[rank(min_ed_2$min_percent_total) == max(rank(min_ed_2$min_percent_total))-1,]

most_common_ed_level <- exhibit9_2 %>%
  group_by(field) %>%
  filter(per == max(per)) %>%
  pull(ed_levels) %>%
  unique() %>%
  paste(collapse=" and ")

#### Text
text_h2 <- "Educational Attainment:"
text_b_1 <- paste("The Bureau of Labor Statistics (BLS) lists a", most_common_ed_level
                  , "as the typical entry-level education for ")
text_b_1_1 <- paste0(jobs, ". ")

if (use_ai) {
    text_b_1_2 <-   call_claude_api_for_table(
  paste0(
    "Rewrite this sentence to make it more coherent. Only respond with the fixed sentence:",
    paste0(
      ". The national-level educational attainment data indicates ",
      some_college_or_aa_degree,
      " of workers in the field have completed some college or an associate degree as their highest level of education. Exhibit 9 shows the educational attainment for ",
      jobs)
  ))
  } else{
    text_b_1_2 <-   paste0(
      ". The national-level educational attainment data indicates that ",
      some_college_or_aa_degree,
      " of workers in the field have completed some college or an associate degree as their highest level of education. Exhibit 9 shows the educational attainment for ",
      jobs)
}
jp_min_edu_postings_per <- paste0(round(100*as.numeric(jp_min_ed_postings)/as.numeric(unique_job_postings)), "%")
jp_min_ed_hs <- pull(jp_min_edu_required[str_detect(jp_min_edu_required$`Minimum Education Level`, "High school|GED"),], `Unique Postings (minimum)`) %>% as.numeric()
jp_min_ed_hs_perc <- paste0(round(100*jp_min_ed_hs/as.numeric(jp_min_ed_postings)), "%")

text_b_2 <- paste0(
  "Of the ",
  total_job_postings,
  " online job postings, ", jp_min_edu_postings_per, " (equivalent to ", prettyNum(jp_min_ed_postings, big.mark = ","), " postings) of cumulative job postings for ", jobs,
  " listed a minimum education requirement in the ", region, " subregion. Of the ", prettyNum(jp_min_ed_postings, big.mark = ","), " postings, ", jp_min_ed_hs_perc,
  " requested a high school diploma or equivalent. ")

#paste0(" ", round(as.numeric(distinct(min_ed_2[min_ed_2$min_percent_total==max(min_ed_2$min_percent_total),"unique_posts_min"]))/as.numeric(str_remove_all(total_job_postings, ",")),2)*100,
#                                  "%",
#                                  " (equivalent to ",
#                                  as.numeric(distinct(min_ed_2[min_ed_2$min_percent_total==max(min_ed_2$min_percent_total),"unique_posts_min"]))," postings) of cumulative job postings for "  ))

text_b_2_1 <- jobs

text_b_2_2 <- paste0(
  " listed a minimum education requirement in the ", region, ", and ",
  paste0(
    paste0(round(second_place$unique_posts_min / sum(min_ed_2$unique_posts_min), 2)*100,"%"),
    " (",
    second_place$unique_posts_min,
    ")"
  ),
  " requested a ",
  min_ed_2[min_ed_2$min_percent_total==max(min_ed_2$min_percent_total),"min_ed"],
  ".")

text_h3_1 <- paste0("Exhibit 9: National-level Educational Attainment for ", jobs)

#img_size_9 <- adjust_image_size("exhibit9.png", dpi = 300)

#### Create Components

footnote_1 <- block_list(
  fpar(
    ftext(text_fn_1, fp_text(font.size = 10, color = "black", font.family = "Tw Cen MT")))
)

body_fpar_1  <- fpar(
  ftext(text_b_1, prop = body_text_style),
  ftext(text_b_1_1, prop = body_text_style_italic),
  ftext(text_b_1_2, prop = body_text_style)
)

#body_fpar_1_1  <- fpar(ftext(text_b_1_1, prop = body_text_style_italic))
#body_fpar_1_2  <- fpar(ftext(text_b_1_2, prop = body_text_style))

body_fpar_2  <- fpar(
  ftext(text_b_2, prop = body_text_style),
  ftext(text_b_2_1, prop = body_text_style_italic),
  ftext(text_b_2_2, prop = body_text_style)
)

#body_fpar_2_1  <- fpar(ftext(text_b_2_1, prop = body_text_style_italic))
#body_fpar_2_2  <- fpar(ftext(text_b_2_2, prop = body_text_style))

# Headers and images combined in single `fpar` blocks
#block_0 <- fpar(
#  ftext(text_h2, H2),
#  fp_p = fp_par(text.align = "center"),
#  ftext(text_b_1, prop = body_text_style)
#)

block_1 <- fpar(
  ftext(text_h3_1, H3),
  fp_p = fp_par(text.align = "center")#,
  #external_img("exhibit9.png", width = img_size_9$width, height = img_size_9$height)
)

#### CONSTRUCT DOCUMENT
doc5 <- read_docx("lma_blank_template.docx") %>%

  body_add_fpar(fpar(ftext(text_h2, H2))) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(body_fpar_1) %>%
  #body_add_fpar(body_fpar_1_1) %>%
  #body_add_fpar(body_fpar_1_2) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(body_fpar_2) %>%
  #body_add_fpar(body_fpar_2_1) %>%
  #body_add_fpar(body_fpar_2_2) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(block_1) %>%
  body_add_docx(src = "ex9_linked.docx", pos = "after")

print(doc5, "officeR_04edattainment.docx")

#print(doc, target = "texting.docx")
#rm(doc)
#end 04edattainment
