
supply_years <- ncc_supply_cvml %>%
  pivot_longer(
    cols = starts_with("20"),
    values_to = "awards",
    names_to = "acadyr"
  ) %>%
  pull(acadyr) %>%
  unique()

supply_years_min <- str_sub(supply_years,1, 4) %>% min()
supply_years_max <- str_sub(supply_years,6, 7) %>% max()
supply_years_max <- paste0("20",supply_years_max)


ex12 <-   ncc_supply_cvml %>%
  pivot_longer(
    cols = starts_with("20"),
    values_to = "awards",
    names_to = "acadyr"
  ) %>%
  mutate(acadyr = paste0(acadyr, " Awards")) %>%
  mutate(awards = as.numeric(ifelse(awards == "", NA, awards))) %>%
  rename(
    Institution.Name = `Institution Name`,

    CIPwithTitle = `CIP with Title`
  ) %>%
  select(`Institution.Name`, CIPwithTitle, `Award Level`, acadyr, awards) %>%
  #distinct() %>%
  group_by(`Institution.Name`, CIPwithTitle, acadyr) %>%
  summarize(awards = sum(awards, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = acadyr, values_from = awards)


if (nrow(ex12) == 0) {
  # Get the award columns
  award_columns <- ncc_supply_cvml  %>%
    select(starts_with("20")) %>%
    names() %>%
    paste0(" Awards")

  # Create a zero-filled list for awards
  awards_zero <- as.list(rep(0, length(award_columns)))
  names(awards_zero) <- award_columns

  for (cip_code in seq_along(CIP)) {
    temp_row <- data.frame(
      `Institution.Name` = "-",
      `CIPwithTitle` = cip_titles_v2[cip_code],
      #`CIPwithTitle` = CIP_titles_code_supplementary[cip_code],
      awards_zero, # Assuming awards_zero is numeric and default is 0
      stringsAsFactors = FALSE
    )
    ex12 <- bind_rows(ex12, temp_row) # Add the new row to the dataframe
  }

  # Ensure 'award_columns' has the correct length
  names(ex12)[5:(4 + length(award_columns))] <- award_columns
}

# Proceed with the rest of the code
ex12_1 <- ex12 %>%
  mutate(
    `CIP Code` = str_split(CIPwithTitle, pattern = "-", simplify = TRUE)[,1],
    Program = str_squish(str_split(CIPwithTitle, pattern = "-", simplify = TRUE)[,2]),
    `CIP Code` = str_squish(sub("^(\\d{4})(\\d+)$", "\\1.\\2", `CIP Code`))
  ) %>%
  rename(Institution = `Institution.Name`) %>%
  select(`CIP Code`, Program, Institution, starts_with("20")) %>%
  rowwise() %>%
  mutate(`3-Year Award Average` = round(mean(c_across(starts_with("20")), na.rm = TRUE), 0)) %>%
  ungroup() %>%
  #mutate(`TOP Code` = paste0(str_sub(`TOP Code`, 1, 4), ".", str_sub(`TOP Code`, 5, 6))) %>%
  filter(`CIP Code` %in% CIP) %>%
  rename(Institution = Institution)

top_part_table <- ex12_1 %>%
  bind_rows(
    #   ccc_supplyn %>%
    ex12_1 %>%
      group_by(`CIP Code`, Program) %>%
      summarise(
        across(
          starts_with("20"),
          sum,
          na.rm = TRUE
        )
      ) %>%
      mutate(`3-Year Award Average` = round(mean(c_across(starts_with("20")), na.rm = TRUE), 0)) #%>%
      #  mutate(Institution = "North CVML")
      #mutate(Institution = paste0("z", ))
    #mutate(Institution = paste0("z", "North CVML"))
  ) %>%
  arrange(`CIP Code`, Institution)

top_part_table[nrow(top_part_table), 1] <- "Supply Total/Average"
top_part_table[nrow(top_part_table), 2] <- "Supply Total/Average"
top_part_table[nrow(top_part_table), 3] <- "Supply Total/Average"

exhibit12_ft <- top_part_table %>%
    map_dfc(function(x){ifelse(x==0, "-", x)}) %>%
    flextable() %>%
    theme_booktabs() %>% # Apply theme first

    # Set the overall font family
    font(fontname = "Segoe UI", part = "all") %>%

    # Set font sizes
    fontsize(size = 12, part = "all") %>%
    # Apply all borders initially
    border_remove() %>%
    border_inner_h(border = border_settings_v2, part = "header") %>%
    border_inner_v(border = border_settings_v2, part = "header") %>%


    align(j = 1:3, align = "left", part = "body") %>%
    align(j = 1:3, align = "center", part = "header") %>%
    align(j = 4:7, align = "center", part = "all") %>%
    align(i = ~ str_detect(Institution, "otal"),
          j = 1:3, align = "right", part = "body") %>%


    # Apply vertical merging to "TOP Code" and "Program" columns
    merge_v(j = c(1, 2)) %>%

    # Apply horizontal merging to "Subtotal/Average" and "Grand Total" rows
    # This will merge the first three columns in these specific rows
    #merge_h(i = ~ Institution == "Subtotal/Average" | str_detect(Institution, "Grand Total")
    #part = "body"
    #j = 1:3
    #) #%>%
    merge_h_range(
      i = ~ Institution == "Supply Total/Average",
      j1 = "CIP Code",
      j2 = "Institution",
      part = "body"
    ) %>%

    bg(part = "header", bg = table1_header_color) %>%
    color(part = "header", color = table_header_fontcolor) %>%
    bold(part = "header") %>%
    bg(nrow(top_part_table), bg = total_bar_color, part = "body") %>%
    bold(nrow(top_part_table), part = "body") %>%
    set_table_properties(layout = "autofit", width = 1)

exhibit_doc_func(exhibit12_ft)


