scripts_dir <- file.path("scripts")

process_word_and_excel_documents <- F

run_script <- function(script_name) {
  cat(paste0("\n\n", paste(rep("=", 50), collapse = ""), "\n"))
  cat(paste0("RUNNING: ", script_name, "\n"))
  cat(paste0(paste(rep("=", 50), collapse = ""), "\n\n"))

  script_path <- file.path(scripts_dir, paste0(script_name, ".R"))

  if(file.exists(script_path)) {
    tryCatch({
      start_time <- Sys.time()
      source(script_path)
      end_time <- Sys.time()
      elapsed <- end_time - start_time

      cat(paste0("\nCompleted ", script_name, " in ", round(elapsed, 2), " seconds\n"))
    }, error = function(e) {
      cat(paste0("\nERROR in ", script_name, ": ", e$message, "\n"))
    })
  } else {
    cat(paste0("\nWARNING: Script file '", script_path, "' not found. Skipping.\n"))
  }
}
list.files(path= "scripts", pattern = "bdp")


checkme <- c(
  "2_Exhibit.xlsx",
  "7_Exhibit.xlsx",
  "9_Exhibit.xlsx",
  "13_Exhibit.xlsx"
  )
#checkme_test <- c("4a_Exhibit.xlsx", "5_Exhibit.xlsx", "9Exhibit.xlsx", "11Exhibit.xlsx")

if (process_word_and_excel_documents==F) {
checkme_test <- c(
  "2_Exhibit.xlsx",
  "7_Exhibit.xlsx",
  "9_Exhibit.xlsx",
  "13_Exhibit.xlsx"
)
} else {
  checkme_test <- c()
}

skip_exhibit <- function(){
  if (length(checkme[which(checkme %in% checkme_test)]) == length(checkme)){
    skip_continue <- "continue"
  } else {
    skip_continue <- checkme[which(!(checkme %in% checkme_test))]
  }
  return(skip_continue)
}

skip_exhibit()


scripts_bdp <- c(
  "01parameters_bdp",
  "02parameter_assignments_bdp",
  "03officeR_setup_bdp",
  "04file_loading_bdp",
  "05functions_data_bdp",
  "06functions_formatting_bdp",
  "exhibit1_bdp",
  "exhibit2_bdp",
  "exhibit3_bdp",
  "exhibit4_bdp",
  "exhibit5_bdp",
  "exhibit6_bdp",
  "exhibit7_bdp",
  "exhibit8_bdp",
  "exhibit9_bdp",
  "exhibit10_bdp",
  "exhibit11_bdp",
  "exhibit12_bdp",
  "exhibit13_bdp"
)

scripts_officeR <- c(
  "officeR_000summary_bdp",
  "officeR_001keyfindings_bdp",
  "officeR_002introduction",
  "officeR_003keyoccupations",
  "officeR_004entryeducation",
  "officeR_005jobpostings_ed",
  "officeR_006jobpostings_LCC",
  "officeR_007jobpostings_workexp",
  "officeR_008advertised_salary",
  "officeR_009occupationalprojections1",
  "officeR_010occupationalprojections2",
  "officeR_011regional_analysis_1",
  "officeR_012regional_analysis_2",
  "officeR_013educational_supply",
  "officeR_014educational_supply_2",
  "officeR_015wages"
)

r_scripts_bdp <- list.files(path = "scripts", pattern = "bdp\\.R$", full.names = TRUE)
scripts_officer <- r_scripts_bdp[which(str_detect(r_scripts_bdp, "officeR_0"))] %>% str_remove_all("scripts/|\\.R")
stack <- list.files(path = "scripts", pattern = "stack\\.R$", full.names = TRUE)
stack <- stack[which(str_detect(stack, "stack"))] %>% str_remove_all("scripts/|\\.R")
# Run all scripts in sequence

scripts_all <- c(scripts_bdp, scripts_officer, stack)

cat("STARTING EXECUTION OF ALL SCRIPTS\n")
cat(paste0("Total scripts to run: ", length(scripts_bdp), "\n"))
cat(paste0("Start time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n"))
cat(paste0("Scripts directory: ", normalizePath(scripts_dir), "\n\n"))

overall_start <- Sys.time()

# Start the PowerShell script in the background


for(script in scripts_all) {
  run_script(script)
}
# When you want to stop the PowerShell script:

overall_end <- Sys.time()
overall_elapsed <- overall_end - overall_start

cat("\n\n", paste(rep("=", 50), collapse = ""), "\n")
cat("ALL SCRIPTS COMPLETED\n")
cat(paste0("Total execution time: ", round(overall_elapsed, 2), " minutes\n"))
cat(paste0("End time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n"))
cat(paste(rep("=", 50), collapse = ""), "\n\n")

r_scripts <- list.files(path = "scripts", pattern = "bdp\\.R$", full.names = TRUE)

combined_text <- ""
for (script_path in r_scripts) {
  script_name <- basename(script_path) # Get just the filename
  content <- readLines(script_path)

  # Add the filename header and script content
  combined_text <- paste0(combined_text,
                          "--- SCRIPT: ", script_name, " ---\n\n",
                          paste(content, collapse = "\n"),
                          "\n\n", # Add extra newlines for separation
                          "--------------------------------------------------\n\n")
}

writeLines(combined_text, "all_r_scripts_combined.txt")
# step 1 - Open the exhibits.xlsx file and save it over itself as a .xlsx file.
# step 2 - Open template_attempt.xlsx
#   step 2a - Click Data (tab) > Refresh All
#   step 2b - press Alt + F11 and F5 (open VBA and manually run script)

