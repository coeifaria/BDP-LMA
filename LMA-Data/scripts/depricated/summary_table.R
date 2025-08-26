# Install and load necessary packages
# install.packages("flextable")
# install.packages("officer")
library(flextable)
library(dplyr)
library(officer)
library(markdown) # Keep loaded for potential markdown rendering later
library(tidyverse)

regional_ave_wage <- function(wage_region){last(pull(reg_demand(wage_region), "Avg. Hourly Earnings"))}
#regional_ave_wage("N")

formatted_wage <- function(wage){
  paste0("$",
         str_pad(
           round(wage,2),
           "right", width = 5, "0"
         )
  )
}
region_region <- function(x){
  if (x %in% c("N", "S")) {
    paste0(x, "CV/",x,"ML", " subregion")
  } else {
    paste0("CVML region")
  }
}


N_S_CVML <- function(x, b="") {
  if (str_sub(x, 1,1)=="N") {
    paste0("NCV/NML", b)
  } else if (str_sub(x, 1,1)=="S"){
    paste0("SCV/SML", b)
  } else {
    paste0("CVML", b)
  }

}
N_S_CVML("C")

N_S_CVML_ex <- function(x)if (str_sub(x, 1,1)=="N"){
  "4a"
} else if (str_sub(x, 1,1)=="S"){
  "4b"
} else {
  "5"
}
# Define colors and border color
color_light_yellow_green = "#E5EBB0"
color_light_blue = "#71FDDE"
color_border = "#000000"

#params
#region_subregion <- paste0(N_S_CVML(str_sub(area_direction, 1,1)), " subregion")
# Define comment strings (using raw strings to avoid issues with backslashes in data frame creation)


sum_compare_text <- function(data) {
  if(nrow(data)==1){
    opp <- tibble(
      compare = !data$compare,
      weight = 1-data$weight
    )
    d_opp <- bind_rows(data, opp)
    d_opp <- paste0(pull(d_opp[d_opp$compare==T,], "weight")*100, "%")
    return(d_opp)
  } else {
    d_opp <- paste0(pull(data[data$compare==T,], "weight")*100, "%")
  }
  return(d_opp)
}


summary_wages_func <- function(reg){

  reg_lw <- if (reg == "N") {
    ncv_living_wage
  } else if (reg == "S") {
    scv_living_wage
  } else if (reg == "CV") {
    cvml_living_wage
  }
  wages_sentence_1 <- paste0(
    str_to_title(((reg_lw_count(reg)))),
    " of the ",
    xfun::n2w(length(SOC)),
    " occupations included in this report ",
    "(representing ", sum_compare_text(lw_compare2),
    " of annual openings) ",
    "have an entry-level hourly wage")
}

summary_wages_p2 <- function(reg){

  reg_lw <- if (reg == "N") {
    ncv_living_wage
  } else if (reg == "S") {
    scv_living_wage
  } else if (reg == "CV") {
    cvml_living_wage
  }
   wage_sentenct_p2 <- paste0(" above the ",
    N_S_CVML(str_sub(area_direction, 1,1)),
    " living wage of ",
    reg_lw,
    ".")
   return(wage_sentenct_p2)
}




# Define the data frame exactly as provided by the user's tibble output structure
# Replacing placeholders with full comment strings where they seem intended based on visual and context

endo_magnitude <- c(
  "Yes" = "#17C71B",
  "Maybe"= "#FFC000",
  "No"= "#FF0000"
  )

endo_func <- function(endorsement#, endo_magnitude=NULL
                      ){
  endo <- vector(mode = "character", length = 2)
  y_n <- vector(mode = "character", length = 2)

  if(endorsement == T){
  endo[1] <- "\u2611"
  endo[2] <- "\u2610"
  #y_n <- c("\u2705", "\u2610")

  } else if(endorsement == F){
  endo[1] <- "\u2610"
  endo[2] <- "\u2611"
  #y_n <- rev(c("\u2705", "\u2610"))
  }

  y_n[1] <- paste0("Yes ", endo[1])
  y_n[2] <- paste0("No ", endo[2])

  #y_n[1] <- endo[1]
  #y_n[2] <- endo[2]

  return(y_n)
}

endo_color_func <- function(endorsement, endo_magnitude="Yes"){

  col <- list()
    if(endorsement == T){
    col[[1]] <- c("c2", "c3")

  } else if(endorsement == F){
    col[[1]] <- c("c4", "c5")
  }

  if(endo_magnitude == "Yes") {
    color <-   "#32A431"
  } else if (endo_magnitude == "Maybe") {
    color <- "#FFC000"
  } else if (endo_magnitude == "No") {
    color <-   "#C00000"
  } else {
    color <- "#FFFFFF"
  }

  col[[2]] <- color
  return(col)
}

endo_color_func(T, "Yes")[[1]]
endo_color_func(T, "Yes")[1:2]
endo_color_func(T, "Yes")[3]



endo_func(T)[[1]]
endo_func(T)[1]
endo_func(T)
supply_gap_dec <- ifelse(regional_openings_summary > (cc_supply_average_summary + ncc_supply_average_summary), T, F)
endo_func(supply_gap_dec)

summary_1_table

lw_compare <- ex4_pick %>%
  filter(str_detect(lev, "Entry")) %>%
  filter(earnings != 0) %>%
  mutate(living_wage = as.numeric(str_remove_all(living_wage, pattern = "\\$"))) %>%
  mutate(Description = str_squish(as.character(Description))) %>%
  select(Description, earnings, living_wage) %>%
  mutate(compare = ifelse(earnings > living_wage, T, F))

lw_compare2 <- summary_1_table %>%
  mutate(jobs = str_squish(jobs)) %>%
  left_join(lw_compare, by = c("jobs" = "Description")) %>%
  mutate(weight = as.numeric(str_remove_all(weight, pattern = " %"))*.01) %>%
  select(jobs, Demand, weight, earnings, living_wage, compare)  %>%
  group_by(compare) %>%
  summarize(
    weight = sum(weight)
  )


sum_compare <- function(data) {
if(nrow(data)==1){
  opp <- tibble(
    compare = !data$compare,
    weight = 1-data$weight
  )
  d_opp1 <- bind_rows(data, opp)
  d_opp <- ifelse(pull(d_opp1[d_opp1$compare==T,], "weight") > pull(d_opp1[d_opp1$compare==F,], "weight"), T, F)
  return(d_opp)
} else {
  d_opp <- ifelse(pull(data[data$compare==T,], "weight") > pull(data[data$compare==F,], "weight"), T, F)
}
  return(d_opp)
}



#sum_compare_text(e_compare)

e_compare <- summary_1_table %>%
  mutate(jobs = str_squish(jobs)) %>%
  mutate(weight = as.numeric(str_remove_all(weight, pattern = " %"))*.01) %>%
  mutate(CCAttainment = as.numeric(str_remove_all(CCAttainment, pattern = "%"))) %>%
  mutate(compare = ifelse(`Typical Entry Level Education` == "Some College or Associate Degree", T, F)) %>%
  group_by(compare) %>%
  summarize(
    weight = sum(weight)
  )

e_compare_text <- summary_1_table %>%
  mutate(jobs = str_squish(jobs)) %>%
  mutate(weight = as.numeric(str_remove_all(weight, pattern = " %"))*.01) %>%
  mutate(CCAttainment = as.numeric(str_remove_all(CCAttainment, pattern = "%"))) %>%
  #mutate(compare = ifelse(`Typical Entry Level Education` == "Some College or Associate Degree", T, F)) %>%
  group_by(`Typical Entry Level Education`) %>%
  summarize(
    weight = sum(weight)
  ) %>%
  arrange(desc(weight)) %>%
  slice(1)

#e_decision <- ifelse(pull(e_compare[e_compare$compare==T,], "weight") > pull(e_compare[e_compare$compare==F,], "weight"), T, F)

e_decision <- sum_compare(e_compare)
lw_decision <- sum_compare(lw_compare2)
endo_color_func(supply_gap_dec, )
decision_color <- function(dec) {
  if(dec==T){
    "Yes"
  } else {
    "No"
  }
}


sg_comments_raw <- paste0("Comments: ",
                          "There are projected to be ",
                          paste(prettyNum(CHANGEME_BOLD_NUMBER, big.mark = ",")),
                          " annual job openings throughout the ",
                          region_region("N"),
                          " for ",
                          general_field,
                          "-related occupations, which are ",
                          CHANGEME_BOLD_MORE_OR_LESS,
                          " than the three-year average of ",
                          CHANGEME_BOLD_AWARD_NUMBER,
                          " awards conferred by educational institutions. (CC + Non-CC)."
)


lw_comments_raw <- paste0(
  "Comments: ",
  summary_wages_func(str_sub(area_direction, 1,1)),
  summary_wages_p2(str_sub(area_direction, 1,1))
)


ed_comments_raw <- paste0(
  "Comments: The typical entry-level education for ",
  general_field, "-related occupations is a ",
  pull(e_compare_text, 1),
  " (about ", 100*pull(e_compare_text, 2), "%). ",
  " Additionally, between ",
  paste(range(summary_1_table$CCAttainment), collapse = " and "),
  " have completed some college or an associate degree as their highest level of education.")

## Define comment strings using as_paragraph and ftext for italics and bolding


body_data_7x5 <- data.frame(
  c1 = c("Program LMI Endorsement Criteria", "Supply Gap:", "Supply Gap:", "Living Wage:\n(Entry-Level, 25th):", "Living Wage:\n(Entry-Level, 25th):", "Education:", "Education:"),
  c2 = c("Program LMI Endorsement Criteria", endo_func(supply_gap_dec)[1], sg_comments_raw, endo_func(lw_decision)[1], lw_comments_raw, endo_func(e_decision)[1], ed_comments_raw),
  c3 = c("Program LMI Endorsement Criteria", endo_func(supply_gap_dec)[1], sg_comments_raw, endo_func(lw_decision)[1], lw_comments_raw, endo_func(e_decision)[1], ed_comments_raw),
  c4 = c("Program LMI Endorsement Criteria", endo_func(supply_gap_dec)[2], sg_comments_raw, endo_func(lw_decision)[2], lw_comments_raw, endo_func(e_decision)[2], ed_comments_raw), # Assuming these columns should have the content for merging
  c5 = c("Program LMI Endorsement Criteria", endo_func(supply_gap_dec)[2], sg_comments_raw, endo_func(lw_decision)[2], lw_comments_raw, endo_func(e_decision)[2], ed_comments_raw), # Assuming these columns should have the content for merging
  stringsAsFactors = FALSE
)


#unique(unlist(body_data_7x5[2,])) %>% str_detect(pattern = "\u2611")
#unique(unlist(body_data_7x5[4,])) %>% str_detect(pattern = "\u2611")
#unique(unlist(body_data_7x5[6,])) %>% str_detect(pattern = "\u2611")

yeses <- body_data_7x5[c(2,4,6),2]
yes_count <- str_detect(yeses, pattern = "\u2611") %>% sum()



#body_data_7x5 <- data.frame(
#  c1 = c("Program LMI Endorsement Criteria", "Supply Gap:", "Supply Gap:", "Living Wage:\n(Entry-Level, 25th):", "Living Wage:\n(Entry-Level, 25th):", "Education:", "Education:"),
#  c2 = c("Program LMI Endorsement Criteria","Yes", sg_comments_raw, "Yes", lw_comments_raw, "Yes", ed_comments_raw),
#  c3 = c("Program LMI Endorsement Criteria", endo_func(T)[1], sg_comments_raw, endo_func(T)[1], lw_comments_raw, endo_func(F)[1], ed_comments_raw),
#  c4 = c("Program LMI Endorsement Criteria", "No", sg_comments_raw, "No", lw_comments_raw, "No", ed_comments_raw), # Assuming these columns should have the content for merging
#  c5 = c("Program LMI Endorsement Criteria", endo_func(T)[2], sg_comments_raw, endo_func(T)[2], lw_comments_raw, endo_func(F)[2], ed_comments_raw), # Assuming these columns should have the content for merging
#  stringsAsFactors = FALSE
#)

# Create flextable from this data
# This will create a table with a default header (c1 to c5) and the 7 rows as body
ft <- flextable(body_data_7x5)

# Delete the default header (c1 to c5) created by flextable
ft <- delete_part(ft, part = "header")

# --- Restructure Header ---

# The first row of the provided data ("Program LMI Endorsement Criteria")
# should be the second header row in the final table. Move it to the header.
# Add this row as the first header row (index 1)
ft <- add_header_row(ft, values = as.list(body_data_7x5[1, ]), top = TRUE)
ft <- color(ft, color = "#455F51", part = "header")
# Delete this row from the body as it's now in the header
ft <- delete_rows(ft, i = 1, part = "body")

# Merge all columns (c1 to c5) in this header row (now header row 1)
ft <- merge_at(ft, i = 1, j = 1:5, part = "header")
#ft <- merge_at(ft, i = 1, j = 1:5, part = "body")
# Add the top header row from the image ("Program LMI Endorsement", etc.)
# This will be the second header row (index 2) in the flextable.
# We need to map the 4 visual header segments to the 5 underlying body columns (c1-c5).
# Interpretation: "Program LMI Endorsement" spans over c1 & c2, then 1 segment per remaining column.

# After adding, header rows in the flextable are:
# Header Row 1: "Program LMI Endorsement", "Endorsed: All...", "Endorsed: Some...", "Not LMI Endorsed..." (colwidths 2, 1, 1, 1)
# Header Row 2: "Program LMI Endorsement Criteria" (merged c1-c5)

# Merge "Program LMI Endorsement" across header rows 1 and 2, in the columns it spans (c1 and c2)
# This targets header rows 1 and 2 (i=1:2), and columns c1 and c2 (j=1:2)

#ft <- merge_at(ft, i = 1:2, j = 1:2, part = "header")



# Your predefined styles (make sure these are defined before the comment strings)
#S1 <- fp_text(font.size = 14, color = "#455F51", font.family = "Tw Cen MT")

#body_text_style <- fp_text(font.size = 11, font.family = "Tw Cen MT")
#body_text_style_bold <- fp_text(font.size = 11, font.family = "Tw Cen MT", bold = TRUE)
#body_text_style_italic <- fp_text(font.size = 11, font.family = "Tw Cen MT", italic = TRUE)
#body_text_style_italicbold <- fp_text(font.size = 11, font.family = "Tw Cen MT", italic = TRUE, bold = TRUE)

# --- Corrected Comment String Definitions using your styles ---

sg_comments_raw2 <- as_paragraph(
  as_chunk("Comments:", prop = body_text_style_italic), # Use your italic style
  as_chunk(" There are projected to be ", prop = body_text_style), # Use your default body style
  as_chunk(paste(prettyNum(CHANGEME_BOLD_NUMBER, big.mark = ",")), prop = body_text_style_bold), # Use your bold style
  as_chunk(" annual job openings ", prop = body_text_style_bold),
  as_chunk(paste0("throughout the ", region_region(str_sub(area_direction,1,1))), prop = body_text_style), # Apply style to the function output
  as_chunk(" for ", prop = body_text_style),
  as_chunk(general_field, prop = body_text_style_italic), # Apply style to the variable
  as_chunk("-related occupations, which are ", prop = body_text_style),
  as_chunk(CHANGEME_BOLD_MORE_OR_LESS, prop = body_text_style_bold), # Use your bold style
  as_chunk(" than the three-year average of ", prop = body_text_style_bold),
  as_chunk(CHANGEME_BOLD_AWARD_NUMBER, prop = body_text_style_bold), # Use your bold style
  as_chunk(" awards conferred by educational institutions. (CC + Non-CC).", prop = body_text_style_bold)
)

lw_comments_raw2 <- as_paragraph(
  as_chunk("Comments:", prop = body_text_style_italic), # Use your italic style
  as_chunk(" ", prop = body_text_style), # Add a space with default style
  as_chunk(summary_wages_func(str_sub(area_direction, 1,1)), prop = body_text_style), # Apply style to the function output
  as_chunk(summary_wages_p2(str_sub(area_direction, 1,1)), prop = body_text_style_bold) # Apply style to the function output
)

ed_comments_raw2 <- as_paragraph(
  as_chunk("Comments:", prop = body_text_style_italic), # Use your italic style
  as_chunk(" The typical entry-level education for ", prop = body_text_style),
  as_chunk(general_field, prop = body_text_style_italic),
  as_chunk("-related occupations is a ", prop = body_text_style),
  as_chunk(pull(e_compare_text, 1), prop = body_text_style), # Apply style
  as_chunk(" (about ", prop = body_text_style),
  as_chunk(paste0(100 * pull(e_compare_text, 2), "%"), prop = body_text_style), # Apply style
  as_chunk("). Additionally, ", prop = body_text_style),
  as_chunk(paste0("between ", paste(range(summary_1_table$CCAttainment), collapse = " and ")), prop = body_text_style_bold), # Apply style
  as_chunk(" have completed some college or an associate degree as their highest level of education.", prop = body_text_style_bold)
)

# Apply compose to the comment cells (rows 2, 4, 6 in current body of ft)
ft <- compose(ft, i = 2, j = c("c2", "c3", "c4", "c5"), value = sg_comments_raw2, part = "body")
ft <- compose(ft, i = 4, j = c("c2", "c3", "c4", "c5"), value = lw_comments_raw2, part = "body")
ft <- compose(ft, i = 6, j = c("c2", "c3", "c4", "c5"), value = ed_comments_raw2, part = "body")

# Now you can safely apply the horizontal merges for the comment rows IF you want them to span
# Note: If your design requires these comments to span multiple columns *after* being composed,
# you might need to merge them first, then compose into the merged cell.
# However, for as_paragraph, you typically compose into the "starting" cell of the merged block.
# Given your existing merge_h(ft, i = 1:6, part = "body"), this means merging all 5 columns.
# So, the compose calls should target a single column (e.g., 'c2') for the merged block.

# Let's adjust based on your current merge_h(ft, i = 1:6, part = "body")
# This means rows 2, 4, and 6 will become single cells spanning c2-c5.
# So, compose into c2, and the as_paragraph will fill the merged cell.

# Redefine the compose calls to target just 'c2' as it will be the start of a merged cell
ft <- compose(ft, i = 2, j = "c2", value = sg_comments_raw2, part = "body")
ft <- compose(ft, i = 4, j = "c2", value = lw_comments_raw2, part = "body")
ft <- compose(ft, i = 6, j = "c2", value = ed_comments_raw2, part = "body")
# --- Header Formatting ---

# Apply background colors
# Header row 1 in code (top row in image) gets light yellow/green background
#ft <- bg(ft, i = 1, part = "header", bg = color_light_blue) %>% color(i = 1, part = "header", color = table_header_fontcolor)
#table_header_fontcolor <- "#000000"
# Header row 2 in code ("Program LMI Endorsement Criteria") gets light blue background
ft <- bg(ft, i = 1, part = "header", bg = color_light_blue)
ft <-color(ft, i = 1, part = "header", color = "#455F51")

# Set header font, bold, and alignment
ft <- font(ft, fontname = "Tw Cen MT", part = "header")
ft <- bold(ft, part = "header", bold = F)
ft <- align(ft, align = "center", part = "header")
#ft <- valign(ft, valign = "middle", part = "header")

thin_border <- fp_border(color = color_border, style = "solid", width = .5)
# Apply specific header font sizes (as determined in previous attempts)
ft <- fontsize(ft, size = 16, part = "header") # Default header size
#ft <- flextable::line_spacing(ft, space = 1, part= "header")
ft <- hline_top(ft, part = "header", border = thin_border)
ft <- hline_bottom(ft, part = "header", border = thin_border)

# --- Body Formatting and Merging ---
# Body rows in ft are now rows 1-6 (corresponding to original data rows 2-7)
# Need to merge cells within the body to match the visual layout based on content and position

# Set body font and default size
ft <- font(ft, fontname = "Tw Cen MT", part = "body")
ft <- fontsize(ft, size = 11, part = "body")
ft <- fontsize(ft, j = 1 ,size = 12, part = "body")

# Bolding for Category labels (c1)
#ft <- bold(ft, j = "c1", part = "body", bold = TRUE)

# Align text in body columns based on visual interpretation
# Category (c1) - left align, top valign
# Yes/No (merged cells) - center align, middle or top valign (visual looks top)
# Comments (merged cells) - left align, top valign

ft <- align(ft, j = "c1", align = "left", part = "body")
ft <- valign(ft, part = "body", valign = "top")

c_combo <- function(decision){
  return(endo_color_func(decision, decision_color(decision)))
}

ft <- color(ft, i = 1, j = c_combo(supply_gap_dec)[[1]], part = "body", color = c_combo(supply_gap_dec)[[2]])
ft <- color(ft, i = 3, j = c_combo(lw_decision)[[1]], part = "body", color = c_combo(lw_decision)[[2]])
ft <- color(ft, i = 5, j = c_combo(e_decision)[[1]], part = "body", color = c_combo(e_decision)[[2]])


# Merge c4 and c5 for "No ☐"
#ft <- merge_h(ft, i = c(1, 3, 5), j = c("c4", "c5"), part = "body")

# Merge horizontally for Comment blocks (rows 2, 4, 6 in current body)
# Merge c2, c3, c4, and c5 for the comment text
#ft <- merge_h(ft, i = c(2, 4, 6), j = c("c2", "c3", "c4", "c5"), part = "body")

# Now that Yes/No and Comments are merged blocks, set their alignment
# Yes/No block (merged c2 & c3): Center align
ft <- align(ft, i = c(1, 3, 5), j = c("c2", "c3", "c4", "c5"),
            align = "center", part = "body") # Target the merged cell area
# Comments block (merged c2-c5): Left align
ft <- align(ft, i = c(2, 4, 6), j = c("c2", "c3", "c4", "c5"), align = "left", part = "body") # Target the merged cell area
ft <- align(ft, i = c(1:6), j = "c1", align = "center", part = "body") # Target the merged cell area


# Apply markdown rendering to comment cells (rows 2, 4, 6 in current body, columns c2-c5 merged)
# Temporarily skipped due to potential markdown_text errors.
# The markdown text is in the data frame, but won't be rendered without compose + markdown_text.
# If markdown_text issues are resolved, uncomment and adjust j and i if needed after merging.
# Example (might need adjustment based on final merged structure):
# ft <- compose(ft, i = c(2, 4, 6), j = "c2", value = as_paragraph(flextable::markdown_text(.))) # Target the start of the merged comment cell


# Add borders to the table (header and body)

# Create a border object for thin black lines
thin_border <- fp_border(color = color_border, style = "solid", width = .25)

# Add inner horizontal borders in the header
#ft <- border_inner_h(ft, part = "header", border = thin_border)
#ft <- hline_bottom(ft, part = "header", border = thin_border)
# Add inner vertical borders in the header
# Based on the header mapping to 5 columns, add vertical borders after the column spans
# Border after the Program LMI Endorsement span (after c2, which is j=2)
# Border after Endorsed: All span (after c3, which is j=3)
# Border after Endorsed: Some span (after c4, which is j=4)
#ft <- border_inner_v(ft, part = "header", border = thin_border#, j = c(2, 3, 4)
#)


# Add borders in the body
# Add a bottom horizontal border to each *section* of the body (after rows 2, 4, 6 in current body)

#ft <- border(ft, i = c(1:6), part = "body", border.bottom = thin_border)
ft <- border(ft, i = c(1,3,5), part = "body", border.bottom = fp_border(color = "grey", style = "solid", width = .1))
ft <- border(ft, i = c(2,4,6), part = "body", border.bottom = thin_border)
ft <- border(ft, i = c(1:6), j = 1, part = "body", border.bottom = thin_border)
ft <- border(ft, i = 6, part = "body", border.bottom = fp_border(color = "white", style = "solid", width = .25))
ft <- border(ft, i = 5,j=1, part = "body", border.bottom = fp_border(color = "white", style = "solid", width = .25))




ft <- fontsize(ft, i = c(1, 3, 5), j = c("c2", "c3", "c4", "c5"), part = "body", size = 14) # Target the merged cell area
#ft <- flextable::line_spacing(ft, i = c(1, 3, 5), j = c("c2", "c3", "c4", "c5"), part = "body", space = 1) # Target the merged cell area
#ft <- flextable::line_spacing(ft,part = "header", space = 1) # Target the merged cell area
ft <- valign(ft, i = c(1, 3, 5), valign = "center") # Target the merged cell area
# Add inner vertical borders in the body
# Based on the original 5 columns and merging: border after c1, and after the merged Yes/No (which was c2/c3)
#ft <- border_inner_v(ft, part = "body", border = thin_border#, j = c(1, 3)
#                     ) # Border after c1 and c3


# Add outer borders around the entire table (header and body)
#ft <- border_outer(ft, part = "all", border = thin_border)

# Merge cells in the body based on identical content within specific regions
# Merge vertically in c1 for identical category names across two rows (rows 1&2, 3&4, 5&6 in current body)

ft <- merge_v(ft, j = "c1", part = "body")

# --- NEW: Merge ALL columns horizontally for ALL body rows ---
# This is the crucial step for your requirement "all the rows horizontally merged"
ft <- merge_h(ft, i = 1:6, part = "body")

# Default center alignment for all body rows (merged columns)
ft <- align(ft, align = "center", part = "body")
ft <- align(ft, i = c(2, 4, 6), align = "left", part = "body")
ft <- valign(ft, j = 1, part = "body", valign = "center")

# Auto-adjust column widths to fit the content
#ft <- autofit(ft)
#ft <- valign(ft, part = "body", valign = "top")


# Set table properties
ft <- set_table_properties(ft,
                           layout = "fixed",
                           #layout = "fixed",
                           width = 1,
                           align = "center") %>%
  flextable::padding(part = "header", padding.top = 0, padding.bottom = 0) %>%
  flextable::padding(part = "body", i = c(1,3,5), padding.top = 1, padding.bottom = 1)

ft <- width(ft, j = 1, width = 1.7, unit = "in")
ft <- width(ft, j = c(2:5), width = (6.5-1.7)/4, unit = "in")
1.7+1.2*4
#ft <- valign(ft, part = "body", valign = "top")

# Install and load the flextable package
# install.packages("flextable")
library(flextable)
library(dplyr)

# Define colors based on the image (using the user-provided hex codes)
color_light_yellow_green = "#E5EBB0"


yeses <- body_data_7x5[c(2,4,6),2]
yes_count <- str_detect(yeses, pattern = "\u2611") %>% sum()

values = c(
  "Program LMI Endorsement",
  "Endorsed: All LMI Criteria Met ",
  "Endorsed: Some LMI Criteria Met ",
  "Not LMI Endorsed "
)

your_function <- function(yes_count) {
  if (yes_count == 1 || yes_count == 2) {
    values <- c(
      "Program LMI Endorsement", " ",
      "Endorsed: All LMI Criteria Met ", "\u2610",
      "Endorsed: Some LMI Criteria Met ", "\u2327",
      "Not LMI Endorsed ", "\u2610"
    )
  } else if (yes_count == 3) {
    values <- c(
      "Program LMI Endorsement", " ",
      "Endorsed: All LMI Criteria Met ", "\u2327",
      "Endorsed: Some LMI Criteria Met ", "\u2610",
      "Not LMI Endorsed ", "\u2610"
    )
  } else {
    values <- c(
      "Program LMI Endorsement", " ",
      "Endorsed: All LMI Criteria Met ", "\u2610",
      "Endorsed: Some LMI Criteria Met ", "\u2610",
      "Not LMI Endorsed ", "\u2327"
    )
  }
  return(values)
}

# Example usage:
# your_function(1) will return the first set of values
# your_function(3) will return the second set of values
# your_function(0) or your_function(4) will return the third set of values

colwidths <- c(10, 1, 6, 1, 6, 1, 6, 1) # sums to 32

# Create an empty flextable with 32 columns
empty_tbl <- as_tibble(matrix(ncol = 32, nrow = 0, dimnames = list(NULL, paste0("col", 1:32))))
#empty_tbl[1,] <- t(as.data.frame(rep("Program LMI Endorsement Criteria", 32)))

summary_header_top <- flextable(empty_tbl)


last_part <- as_tibble(matrix(ncol = 2, nrow = 0, dimnames = list(NULL, paste0("col", 1:2)))) %>%
  flextable() %>%
  add_body_row(values = c("Yes \u2610", "No \u2611"), colwidths = c(1, 1), top = T) %>%
  add_body_row(values = rep("Comments: N/A", 2), colwidths = c(1, 1), top = F) %>%
  add_header_row(values = rep("Emerging Occupations(s)", 2), colwidths = c(1, 1), top = T) %>%
  flextable::delete_rows(i = 2, part = "header") %>%
  bg(part = "header", bg = "#BFBFBF") %>%
  color(part = "header", color = "#455F51") %>%
  font(fontname = "Tw Cen MT", part = "all") %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 16, part = "header") %>%
  hline_top(part = "header", border = thin_border) %>%
  hline_bottom(part = "header", border = thin_border) %>%
  fontsize(size = 14, i = 1, part = "body") %>%
  fontsize(size = 12, i = 2, part = "body") %>%
  hline_bottom(part = "body", border = thin_border) %>%
  merge_h(part = "body")%>%
  merge_h(part = "header") %>%
  set_table_properties(layout = "autofit",
                       #layout = "fixed",
                       width = 1,
                       align = "center")

#last_part

last_part <- as_tibble(matrix(ncol = 5, nrow = 0, dimnames = list(NULL, paste0("col", 1:5)))) %>%
  flextable() %>%
  add_body_row(values = c("Yes \u2610","Yes \u2610","", "No \u2611" , "No \u2611"), colwidths = rep(1, 5), top = T) %>%
  add_body_row(values = rep("Comments: N/A", 5), colwidths = rep(1, 5), top = F) %>%
  add_header_row(values = rep("Emerging Occupations(s)", 5), colwidths = rep(1, 5), top = T) %>%
  flextable::delete_rows(i = 2, part = "header") %>%
  bg(part = "header", bg = "#BFBFBF") %>%
  color(part = "header", color = "#455F51") %>%
  font(fontname = "Tw Cen MT", part = "all") %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 16, part = "header") %>%
  hline_top(part = "header", border = thin_border) %>%
  hline_bottom(part = "header", border = thin_border) %>%
  fontsize(size = 14, i = 1, part = "body") %>%
  fontsize(size = 12, i = 2, part = "body") %>%
  hline_bottom(part = "body", border = thin_border) %>%
  merge_h(part = "body")%>%
  merge_h(part = "header") %>%
  flextable::padding(part = "header", padding.top = 0, padding.bottom = 0) %>%
  flextable::padding(part = "body", padding.top = 1, padding.bottom = 1) %>%
  #line_spacing(part = "header", space = 1) %>%
  set_table_properties(layout = "autofit",
                       #layout = "fixed",
                       width = 1,
                       align = "center")


# Add your header row as a body row
summary_header_top <- add_body_row(summary_header_top, values = your_function(yes_count), colwidths = colwidths, top = TRUE)

# Remove the header
summary_header_top <- delete_part(summary_header_top, part = "header")

#summary_header_top <- bg(summary_header_top, i = 1, part = "header", bg = color_light_yellow_green)
summary_header_top <- bg(summary_header_top, i = 1, part = "body", bg = color_light_yellow_green)
summary_header_top <- color(summary_header_top, color = "#455F51", part = "all")
# Row 2 (bottom row in image) gets light blue background

# Apply text alignment (center and middle) to the header
summary_header_top <- align(summary_header_top, align = "center", part = "body")
summary_header_top <- valign(summary_header_top, valign = "center", part = "body")

# Apply font sizes and bolding

# Set default font size for the body to 13 first
summary_header_top <- fontsize(summary_header_top, size = 13, part = "body")

# Change "Program LMI Endorsement" to bold and size 16 (body row 1, column 1 asummary_header_toper merging)
summary_header_top <- bold(summary_header_top, i = 1, j = 1, part = "body", bold = F)
summary_header_top <- fontsize(summary_header_top, i = 1, j = 1, part = "body", size = 16)
summary_header_top <- font(summary_header_top, fontname = "Tw Cen MT", part = "body")
# Change "Program LMI Endorsement Criteria" to size 16 (body row 2)
#summary_header_top <- fontsize(summary_header_top, i = 2, part = "body", size = 16)

# Remove default borders and set table properties
summary_header_top <- border_remove(summary_header_top) %>% set_table_properties(layout = "autofit", width = 1, align = "center", )
row_height <- .4
summary_header_top <- height(summary_header_top, part = "header", height = row_height)
white_border <- fp_border(color = "white", style = "solid", width = 2)

summary_header_top <- hline_bottom(summary_header_top, border = white_border, part = "body")
summary_header_top <- hline_bottom(summary_header_top, border = white_border, part = "header")
#add_body_row(summary_header_top, values = t(as.data.frame(rep("Program LMI Endorsement Criteria", 32))), colwidths = colwidths, top = TRUE)

#empty_tbl[1,] <- t(as.data.frame(rep("Program LMI Endorsement Criteria", 32)))
summary_header_top <- bg(summary_header_top, i = 1, part = "header", bg = color_light_blue)
summary_header_top <- color(summary_header_top, color = "#455F51", part = "body")



lin <- fp_text(font.size = 11, font.family = "Tw Cen MT")

# Define a 2-pixel (about 1.5pt) bottom border for the paragraph
line_par <- fp_par(
  border.bottom = fp_border(width = 5, color = "blue")
)

line_fpar <- fpar("", fp_p = line_par)
#body_add_fpar(fpar(ftext(text_b_3_1, body_text_style)

# Add first table, the "deleteme" paragraph, and the second table

# Reload and remove the "deleteme" paragraph


# Create a header with a right-aligned image

# Create a right-aligned paragraph containing the image

# After creating the flextable 'ft' and before merging columns (or at least before merge_h(ft, i = 1:6, part = "body") )
# You MUST use compose to insert the rich text objects into the flextable cells.


library(magrittr)
# Print the flextable to Word
bit <- read_docx("LMA_TEMPLATE.docx") %>%
  body_add_flextable(summary_header_top) %>%
  #body_add_fpar(line_fpar) %>%
  body_add_par(value = " ", style = "flextable_break") %>%
  #body_add_par("", pos = "after") %>%
  #body_add_fpar("", style = lin) %>%
  #body_add_fpar(fp_text(color = "black", font.size = 10, font.family = "Tw Cen MT")) %>%
  body_add_flextable(ft) %>%
  body_add_par(value = " ", style = "flextable_break") %>%
  body_add_flextable(last_part)

# Save the final document
print(bit, target = "testing_summary.docx")
