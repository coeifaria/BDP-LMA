educational_supply_paragraph_3 <- paste0(
  "Exhibit 12 shows the annual and three-year average number of awards conferred by non-community college institutions in the related CIP codes in the CVML region. Between ",
  supply_years_min,
  " and ",
  supply_years_max,
  ", non-community college institutions throughout the region conferred an average of ",
  format_num_func(last(pull(top_part_table, ncol(top_part_table))), "#"),
  " baccalaureate awards annually in related training programs. "
)


#doc14 <- read_docx("bdp_template.docx") %>%
doc14 <- read_docx() %>%
  body_add_fpar(fpar(ftext(educational_supply_paragraph_3, body_text_style), fp_p = fp_par(text.align = "justify"))) %>%
  body_add_par("") %>%
  body_add_fpar(
    fpar(
      ftext(
        paste0("Exhibit 12: Regional Non-Community College Awards, ", supply_years_min, "-", supply_years_max), H3),
      fp_p = fp_par(text.align = "center"))
  ) %>%
  body_add_flextable(exhibit12_ft)

print(doc14, "officeR_014educational_supply_2.docx")

