#```{r officer_08workbook, chunk_progress=TRUE}

#working_title2 <- paste0("../", paste(TOP, top_code_title, str_split(requesting_region, " ")[[1]][1], format(Sys.Date(), "%b%y"), sep = "_"), ".xlsx")[1]
gen_field <- params$general_field[1]
working_title2 <- paste0("../", paste(TOP, gen_field, str_split(requesting_region, " ")[[1]][1], format(Sys.Date(), "%b%y"), sep = "_"), ".xlsx")[1]

# Current sheet names
sheet_names <- names(wb)
#wb2 <- loadWorkbook("Graphics and Visuals Templates for LMA.xlsx")

# Define the desired order based on your description
desired_order <- c(
  "Exhibit 1 - Summary", "1Exhibit",
  "Exhibit 2 - Percentage Change", "2Exhibit","Exhibit 2-% change-all CA occs",
  "Exhibit 3 - Template", "3Exhibit",
  "Exhibit 4 - NCVNML WageChart", "Exhibit 4 - SCVSML WageChart", "4_5Exhibit",
  "Exhibit 5 - CVML WageChart",
  "6Exhibit", "7Exhibit", "8Exhibit",
  "Exhibit 6,7,8 - Template",
  "Exhibit 9 - Ed. Attainment", "9Exhibit", "(Ed Attainment) Table 5.3",
  #"Exhibit 10 - Template",
  "Supply Templates",
  "10_North", "10_South", "10_NCC_North", "10_NCC_South",
  "Exhibit 11 - Template", "11exhibit","12exhibit", "Exhibit 12 - Template",
  "North-CC Supply", "South-CC Supply", "North-Non CC Supply", "South-Non CC Supply ",
  "TOP-CIP-SOC - 2_25", "CC Supply", "NonCC Supply","Lightcast-specific codes - 2_25"
)

library(writexl)

exhibits <- list(
  `1Exhibit` = exhibit1_2_cleaned,
  `2Exhibit` = processed_data2,
  `3Exhibit` = exhibit3_table,

  `4a_Exhibit` = processed_data4[processed_data4$region == "ncvnml", -c(5, 6)],
  `4b_Exhibit` = processed_data4[processed_data4$region == "scvsml", -c(5, 6)],
  `5Exhibit` = processed_data4[processed_data4$region == "cvml", -c(5, 6)],

  `6Exhibit` = e_6,
  `7Exhibit` = e7,
  `8Exhibit` = tp_top_skills,
  `9Exhibit` = exhibit9_3,
  `10_North` = ccc_supplyn,
  `10_South` = ccc_supplys,
  `10_NCC_North` = nccc_supplyn,
  `10_NCC_South` = nccc_supplys,
  `11exhibit` = ex11_11,
  `12exhibit` = ex12_1_1
)

write.xlsx(exhibits, "exhibits.xlsx")
#write.xlsx(processed_data2, "exhibits.xlsx", sheetName = "2Exhibit")
#write.xlsx(exhibit3_table, "exhibits.xlsx", sheetName = "3Exhibit")

#write.xlsx(processed_data4[processed_data4$region == "ncvnml",-c(5,6)], "exhibits.xlsx", sheetName = "4a_Exhibit")
#write.xlsx(processed_data4[processed_data4$region == "scvsml",-c(5,6)], "exhibits.xlsx", sheetName = "4b_Exhibit")
#write.xlsx(processed_data4[processed_data4$region == "cvml",-c(5,6)], "exhibits.xlsx", sheetName = "5Exhibit")
#writeData(wb, "6Exhibit", e_6)
#desired_order[24]

#names(wb2)
names(wb)
# Reorder sheets
worksheetOrder(wb) <- match(desired_order, sheet_names)

# Save the new workbook

# Save the workbook with the reordered sheets
#saveWorkbook(wb, "Graphics and Visuals Templates for LMA_Reordered.xlsx", overwrite = TRUE)
saveWorkbook(wb, working_title2, overwrite = T)
