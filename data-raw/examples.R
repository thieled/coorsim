## code to prepare `examples` dataset goes here
example_text <- "- patria_unida21: #NoALaGuerra Los pueblos del mundo no queremos más invasiones. Ucrania merece la paz, no armas. #PazParaUcrania #StopNATO | - voz_del_sur: Una vez más la OTAN demuestra su carácter belicista. ¿Hasta cuándo? #NATOFuera #PazConJusticia | - alondra_justa: No queremos misiles, queremos médicos. #SaludNoBombas #SolidaridadEntrePueblos  | - giorgi_rossi: Le guerre non portano democrazia, portano morte. Basta armi! #NoAllaNATO #PacePerLEuropa"

example_answer <- '{"label": "Anti-NATO Peace Campaign", "description": "Campaign opposing NATO and Western intervention, posting in Spanish. Uses solidarity hashtags, emotional appeals, and historical references to frame NATO as the aggressor and to promote peace, sovereignty, and regional unity.", "lang": "es"}'

example_system <- paste(
  "You summarize and label sampled posts from communities of social media users.",
  "Generate:",
  "- one concise 'label' for in English for the community",
  "- a brief 'description' in English that summarizes the topics, tone, and regional focus of the community",
  "- and determine the predominant language in 'lang'.",
  sep = "\n"
)

example_prompt <- "Provide 'label', 'description', and 'lang' in JSON format. Provide no additional output."

usethis::use_data(
  example_text,
  example_answer,
  example_system,
  example_prompt,
  internal = TRUE,
  overwrite = TRUE
)
