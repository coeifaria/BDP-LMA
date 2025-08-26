


soc_codes_00 <- digit_adder_onet(SOC)

intro_description_and_job_titles <- vector(mode = "list", length(soc_codes_00))

for (soc_code in soc_codes_00) {
  ONET_call <- fetch_soc_data(soc_code)
  intro_description_and_job_titles[[soc_code]][["description"]] <- ONET_call$summary$occupation$description
  intro_description_and_job_titles[[soc_code]][["sample_titles"]] <- ONET_call$summary$occupation$sample_of_reported_job_titles
}

#intro_pargraph_1 <- "The Applied Agricultural Science and Technology, , program proposed by Cerro Coso College is designed prepare students to integrate, install, operate, and maintain advanced technology systems in agricultural operations (e.g., food production, processing, and distribution businesses). Specific competencies include applications of IoT networks, precision agriculture, robotics, automation, and data analytics. Students learn to bridge traditional agricultural knowledge with emerging technology implementation, preparing them for careers as agricultural technology specialists, systems integrators, and field support technicians. The program emphasizes hands-on application, industry partnerships, and real-world problem-solving to meet the growing demand for technology-skilled professionals in California's agricultural sector. Graduates will be equipped to implement, troubleshoot, and optimize smart farming and agricultural industry systems that enhance productivity, sustainability, and profitability in diverse agricultural operations."
intro_pargraph_1 <- paste0(
  "The " ,
  general_field,
  ", ",
  degree_title,
  " Degree, program proposed by ",
  requesting_college,
  " is designed for students planning to become California licensed respiratory care practitioners and registered respiratory therapists. Completion of the requirements also allows students to apply for all state and national advanced specialty credentialing examinations. Students will acquire the skills to provide a wide range of high technology and high-touch therapeutic interventions to patients in acute and chronic care settings.  Competencies are assessed through the use of classroom, laboratory, and clinical performance evaluations in simulated and actual patient care situations. Program success is determined through examining  attrition rates, employment rates, and licensure exam pass rates.  The degree and certificate in respiratory care are awarded after successful completion of the advanced registry-level respiratory care program.

  ",
  pull(CIPCode2020[(CIPCode2020$CIPCode %in% CIP_pull),], CIPDefinition)
)

#CIP_pull <- CIP_string
#CIP_pull <- '01.0000'
#pull(CIPCode2020[(CIPCode2020$CIPCode %in% CIP_pull),], CIPTitle)
#pull(CIPCode2020[(CIPCode2020$CIPCode %in% CIP_pull),], CIPDefinition)



doc2 <- read_docx("bdp_template.docx") %>%
  body_add_fpar(fpar(ftext("Introduction", H1), fp_p = fp_par(text.align = "left"))) %>% # This line is where the error occurs
  body_add_par("", style = "Normal") %>%
  body_add_fpar(fpar(ftext(intro_pargraph_1, body_text_style), fp_p = fp_par(text.align = "justify"))) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(fpar(ftext("The key occupations that typically require a bachelor's degree are:", body_text_style), fp_p = fp_par(text.align = "left"))) %>%
body_add_par("", style = "Normal")

for (soc_code in SOC) {
  job_title <- paste0(soc_code_titles_w_soc_code, ": ")
  job_description <- fpar(
    ftext(job_title, body_text_style_bold),
    ftext(intro_description_and_job_titles[[digit_adder_onet(soc_code)]][["description"]], body_text_style),
    ftext(".", body_text_style),
    fp_p = fp_par(text.align = "justify")
  )
  sample_title <- fpar(
    ftext("Sample of reported job titles: ", body_text_style_bold),
    ftext(paste(intro_description_and_job_titles[[digit_adder_onet(soc_code)]][["sample_titles"]][["title"]],collapse = ", "), body_text_style),
    ftext(".", body_text_style),
    fp_p = fp_par(text.align = "justify")
  )

doc2 <- doc2 %>%
  body_add_fpar(job_description) %>%
  body_add_par("") %>%
  body_add_fpar(sample_title)

}

print(doc2, "officeR_002introduction.docx")
