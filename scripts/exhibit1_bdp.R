e1 <- ca_demand %>%  select(SOC, Description, `Typical Entry Level Education`, `Work Experience Required`, `Typical On-The-Job Training`) %>%
  rename(Occupation = Description) %>%
  mutate(`Typical Entry Level Education` =  a_degree_fix(`Typical Entry Level Education`, a=F, b=F))

exhibit1_ft <- flextable(e1) %>%
    font(fontname = "Segoe UI", part = "all") %>%
  fontsize(size = 12, part = "all") %>%
  border_remove() %>%
  border_inner_h(border = border_settings, part = "body") %>%
  hline_top(border = border_settings, part = "body") %>%
  hline_bottom(border = border_settings, part = "body") %>%
  # Alignment
  align(j = 3:5, align = "center", part = "all") %>%
  # Header styling (no borders will be applied to header)
  bg(part = "header", bg = table1_header_color) %>%
  color(part = "header", color = table_header_fontcolor) %>%
  bold(part = "header") %>%
  bg(i = seq(ifelse(nrow(e1)==1, 1, 2), nrow(e1), 2), bg = table_banding_odd, part = "body") %>%
  bg(i = seq(1, nrow(e1), 2), bg = table_banding_even, part = "body") %>%
  # Table properties
  flextable::set_table_properties(layout = "autofit", width = 1)

exhibit_doc_func(exhibit1_ft)
