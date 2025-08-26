#Sys.setenv(CHROMOTE_CHROME = "C:/Users/if001/AppData/Local/Google/Chrome/Application/chrome.exe")
#font_import(pattern = "Segoe UI")
#H0_color <- "#549E39"
H0_color <- "#000000"

#H1_color <- "#0989B1"
H1_color <- "#0F4761"
#H1_color <- "#0E4660"
#H2_color <- "#4AB5C4"
H2_color <- "#008985"

exhibit_header_color <- "#029676"
header1_color <- "#4AB5C4"
changed_color_line <- "#bed62f"

#table1_header_color <- "#E5EBB0"
table1_header_color <- "#126877"

#table_header_fontcolor <- "#455F51"
table_header_fontcolor <- "#FFFFFF"


table2_header_color <- "#d9e288"
table_bottom_sum_color <- "#4AB5C4"

solid_bar_graph_color <- "#549E39"
our_names <- "#3E762A"
total_bar_color <- "#AAAAA9"

table_banding_even <- "#FFFFFF"
table_banding_odd <- "#D3D4D3"

#border_settings <- fp_border(color = "#A6A6A6", width = 1)
border_settings <- fp_border(color = "#2EAA7C", width = 1)

border_settings_v2 <- fp_border(color = "#FFFFFF", width = 1)

H0 <- fp_text(font.size = 22, color = H0_color, font.family = "Segoe UI", bold = T)
H0_1 <- fp_text(font.size = 18, color = H0_color, font.family = "Segoe UI", bold = F)

H1            <- fp_text(font.size = 20, color = H1_color, font.family = "Segoe UI")
H1_italicized <- fp_text(font.size = 20, color = H1_color, font.family = "Segoe UI", italic = T)

H2            <- fp_text(font.size = 16, color = H1_color, font.family = "Segoe UI")
H2_italicized <- fp_text(font.size = 16, color = H1_color, font.family = "Segoe UI", italic = T)

H3            <- fp_text(font.size = 13, color = H1_color, font.family = "Segoe UI")
H3_italicized <- fp_text(font.size = 13, color = H1_color, font.family = "Segoe UI", italic = T)

#H3 <- fp_text(font.size = 14, color = exhibit_header_color, font.family = "Segoe UI")
S1 <- fp_text(font.size = 14, color = "#455F51", font.family = "Segoe UI")
body_text_style <- fp_text(font.size = 12, font.family = "Segoe UI")
body_text_style_bold <-fp_text(font.size = 12, font.family = "Segoe UI", bold = TRUE)
body_text_style_italic <-fp_text(font.size = 12, font.family = "Segoe UI", italic = TRUE)

body_text_style_related_occupations <- body_text_style
#body_text_style_related_occupations <- body_text_style_italic
body_text_style_italicbold <-fp_text(font.size = 12, font.family = "Segoe UI", italic = TRUE, bold = TRUE)
#fp_p_justify <- fp_par(text.align = "justify")

text_date <- format(Sys.Date(), "%B %Y")

# Define text styles
#body_text_outro_style <- fp_text(font.size = 13, color = "#000000", font.family = "Segoe UI")
#body_text_outro_style1 <- fp_text(font.size = 12, font.family = "Segoe UI")

highlighted_text <- fp_text(
  font.size = 11,
  font.family = "Segoe UI",
  color = "darkgrey"
  #shading.color = "red"
)

highlighted_text <- body_text_style

