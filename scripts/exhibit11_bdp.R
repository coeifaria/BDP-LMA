
ncc_awards <- unique(ncc_supply_raw$`Award Level`)
text_bachelor <- ncc_awards[str_detect(ncc_awards, "Bachelor")]
text_masters_up <- ncc_awards[str_detect(ncc_awards, "Master|Doctor")]

# Option 3: More dynamic approach using across() for all year columns
#ncc_supply_cvml %>%
exhibit_11_func <- function(cvml_or_ca, region){
  if (nrow(cvml_or_ca)==0) {
  ex11 <-   tibble(
    `CIP Code` = CIP,
    `CIP Title` = cip_titles,
    `CVML Non-CC Baccalaureate Awards` = 0,
    `CVML Non-CC Master's or Above Awards` = 0,
  )
  } else {
ex11 <- cvml_or_ca %>%
  mutate(awards =
           case_when(
             `Award Level` == text_bachelor ~ paste0(region, " Non-CC Baccalaureate Awards"),
             `Award Level` %in% text_masters_up ~ paste0(region, " Non-CC Master's or Above Awards"),
             TRUE ~ NA
           )) %>%
  select(CIP, CipTitle, awards, starts_with("20")) %>%
  filter(!is.na(awards)) %>%
  pivot_longer(
    cols = starts_with("20")
  ) %>%
  group_by(awards, CIP, CipTitle) %>%
  reframe(value = sum(value)) %>%
  pivot_wider(
    names_from = awards,
    values_from = value
  )


if(suppressWarnings(is.null(ex11[[paste0(region, " Non-CC Baccalaureate Awards")]]))) {
  ex11[[paste0(region, " Non-CC Baccalaureate Awards")]] <- NA
}


if(suppressWarnings(is.null(ex11[[paste0(region, " Non-CC Master's or Above Awards")]]))) {
  ex11[[paste0(region, " Non-CC Master's or Above Awards")]] <- NA
}

names(ex11)[1:2] <- c("CIP Code", "CIP Title")
}
return(ex11)
}


exhibit11 <- full_join(
exhibit_11_func(ncc_supply_cvml, "CVML"),
exhibit_11_func(ncc_supply_raw[ncc_supply_raw$CIP %in% CIP_string,], "CA")
) %>% suppressMessages()

total_row_ex11 <- tibble(
v1 = "",
v2 = "3-Year Average",
v3 = sum(exhibit11[,3]),
v4 = sum(exhibit11[,4]),
v5 = sum(exhibit11[,5]),
v6 = sum(exhibit11[,6])
)

names(total_row_ex11) <- names(exhibit11)
exhibit11_1 <- bind_rows(exhibit11,total_row_ex11) %>%
  map_dfc(as.character) %>%
  map_dfc(function(x){ifelse(x==0, "-", x)})

exhibit11_ft <- flextable(exhibit11_1) %>%

  # Center align all content
  align(align = "center", part = "all") %>%

  # Set header labels with line breaks
#  set_header_labels(.labels = setNames(
#    c(gsub(" ", "\n", col_change, fixed = TRUE),
#      gsub(" ", "\n", col_pct_change, fixed = TRUE)),
#    c(col_change, col_pct_change)
#  )) %>%

  # Style header
  fontsize(size = 12, part = "header") %>%
  font(fontname = "Segoe UI", part = "header") %>%
  bold(part = "header") %>%
  color(color = "white", part = "header") %>%
  bg(bg = table1_header_color, part = "header") %>%

  # Style body
  fontsize(size = 12, part = "body") %>%
  font(fontname = "Segoe UI", part = "body") %>%

  # Style footer (last row as footer proxy)
  bg(i = seq(ifelse(nrow(exhibit11_1)==1, 1, 2), nrow(exhibit11_1), 2), bg = table_banding_odd, part = "body") %>%
  bg(i = seq(1, nrow(exhibit11_1), 2), bg = table_banding_even, part = "body") %>%
  fontsize(i = nrow(exhibit11_1), size = 12, part = "body") %>%
  font(i = nrow(exhibit11_1), fontname = "Segoe UI", part = "body") %>%
  bg(nrow(exhibit11_1), bg = total_bar_color, part = "body") %>%
  bold(nrow(exhibit11_1), part = "body") %>%
  border_remove() %>%
  border_inner_h(border = border_settings, part = "body") %>%
  vline(j = 4, border = fp_border(width = 3), part = "all") %>%
  hline_top(border = border_settings, part = "body") %>%
  hline_bottom(border = border_settings, part = "body") %>%
  set_table_properties(layout = "autofit", width = 1) # Replaces autofit()
  # Autofit column widths
  #autofit()


exhibit_doc_func(exhibit11_ft)
#exhibit11_ft

##### Document officeR_013educational_supply_bdp.R


