# data-raw/llm_prompts.R

# Prompt - Users
prompt_user <- "Provide 'description', and 'lang' in JSON format. Provide no additional output." 

# Prompt - Communities and sliced communities
prompt_comm <- "Provide exactly one 'label', and one 'description' in JSON format. Provide no additional output." 


# System prompts - Users 
system_user <- "You are an expert social media analyst, describing users by their sampled posts and metadata. For each user, generate: (1) a concise 'description' in English that summarizes their main topics, political leaning, regional focus, tone, style, and any repetitive patterns, and (2) the predominant language in 'lang'."

# System prompts - Communities
system_comm <- "You are an expert social media analyst, describing user communities by their pre-processed user descriptions. For each community, generate: (1) one concise 'label' in English and (2) one brief 'description' summarizing the topics, political leaning, regional focus, tone, style, and any repetitive patterns of the community content."

# System prompts - sliced communities
system_comm_agg <- "You are an expert social media analyst, describing user communities. Summarize the pre-processed labels for the slices from this community, taking into account the share of the slice. For each community, generate: (1) one concise 'label' in English and (2) one brief 'description' summarizing the topics, political leaning, regional focus, tone, style, and any repetitive patterns of the community content."


# Examples - Users
example_user_text <- c(
  "@brit_voice92 [Ctry: uk]: Nigel Farage is the only one standing up for real British values. 💜 Time to take our country back. | Sick of the open border madness — UKIP speaks for the people. 💜💜 | No to EU, no to woke politics. Yes to UKIP! 💜 #Farage2024",
  "@denker_für_frieden [Ctry: de]: Nur Wagenknecht spricht sich mutig gegen den Krieg aus! 🇩🇪🤝🇷🇺 | Wir brauchen Frieden, nicht Panzer – Freundschaft mit Russland statt Eskalation! 🇷🇺🇩🇪 | Niemand außer Wagenknecht fordert einen echten diplomatischen Weg zur Beendigung des Ukraine-Kriegs. ✌️"
  )

# Examples - Communities
example_comm_text <- c(
  "UK user consistently promoting UKIP and Nigel Farage, frequently using purple heart emojis to signal party loyalty. Tone is confident, politically assertive, and aligned with nationalist themes. | UK user praising Nigel Farage’s stance on Brexit and immigration, often accompanied by purple (💜) heart emojis. Posts express strong identity-based political views. | UK user sharing supportive messages about UKIP leadership and sovereignty issues. Repeated use of 💜 emojis highlights passionate endorsement of the far-right party UKIP.",
  "DE user expressing strong support for Sarah Wagenknecht’s call to end the Ukraine war through diplomacy, using DE and RU emojis to signal peace advocacy. Tone is hopeful and emotionally invested. | German user promoting the idea of renewed friendship with Russia and criticizing NATO militarism. Posts emphasize peace and refer to Wagenknecht as the only reasonable voice. 🇷🇺🇩🇪 | DE user repeatedly posting anti-war messages and calling for negotiations with Russia, framing Wagenknecht as the only one standing up for peace. Use of ✌️ and flag emojis underscores pacifist identity."
)

# Examples - Sliced communities
example_comm_slices_answer <- c(
  '{"label": "UKIP Loyalists and Brexit Hardliners",
    "description": "UK-based users passionately supporting Nigel Farage and UKIP, using purple heart emojis (💜) to express party identity and emotional commitment. Posts reflect strong nationalist and anti-immigration themes, often attacking mainstream Conservatives for Brexit compromises. Tone is assertive, populist, and anti-establishment."}'
)

# Answers - Users
example_user_answer <- c(
  '{"description": "UK-based user displaying strong support for Nigel Farage and UKIP. Posts emphasize nationalist themes, opposition to immigration and the EU, and repeated use of purple heart emojis (💜) as a marker of political identity. Tone is assertive, loyal, and emotionally expressive.",
    "lang": "en"}',
  '{"description": "Germany-based user supporting Sarah Wagenknecht’s anti-war stance and promoting peaceful relations with Russia. Posts stress diplomacy over militarism, criticize escalation in Ukraine, and invoke repeated 🇩🇪 and 🇷🇺 emojis to symbolize German-Russian friendship. Tone is emotional, hopeful, and pacifist.",
    "lang": "de"}'
)

# Answers - Communities
example_comm_answer <- c(
  '{"label": "UKIP Supporters with Repetitive Emoji Use",
    "description": "UK-based users showing consistent and emotionally expressive support for Nigel Farage and UKIP. Posts feature strong nationalist and anti-immigration themes, marked by repeated use of purple heart emojis (💜) reflecting party identity and ideological commitment."}',
  '{"label": "Pro-Wagenknecht Pacifist Community",
    "description": "Germany-based users advocating for peace and German-Russian reconciliation, united by support for Sarah Wagenknecht’s anti-war positions. Posts criticize military escalation and frame diplomacy as the only solution. Use of 🇩🇪 and 🇷🇺 emojis reinforces a shared pacifist and nationalist identity. Tone is emotional, hopeful, and critical of mainstream foreign policy."}'
)

# Answers - Aggregating sliced communities
example_comm_slices_text <- c(
  "[Slice: 1, Share: 70%] UKIP Loyalists with Emphatic Emoji Use: UK-based users consistently promoting Nigel Farage and UKIP, frequently using purple heart emojis (💜) to signal party loyalty. Posts emphasize sovereignty, anti-immigration stances, and assertive nationalism. Tone is confident, emotional, and identity-driven. |
   [Slice: 2, Share: 25%] Brexit Advocates Criticizing Mainstream Conservatives: British users expressing support for Farage while criticizing the Conservative Party’s handling of Brexit. Posts highlight betrayal by establishment politicians and praise UKIP’s 'true' vision. Tone is accusatory, anti-elite, and assertively nationalist."
)


prompts <- list(prompt_user = prompt_user, 
                prompt_comm = prompt_comm,
                system_user = system_user, 
                system_comm = system_comm,
                system_comm_agg = system_comm_agg
)

examples <- list(example_user_text = example_user_text, 
                 example_user_answer = example_user_answer,
                 example_comm_text = example_comm_text, 
                 example_comm_answer = example_comm_answer,
                 example_comm_slices_text = example_comm_slices_text, 
                 example_comm_slices_answer = example_comm_slices_answer)

### Save them
usethis::use_data(
  prompts, examples,
  internal = FALSE, overwrite = TRUE, ascii = FALSE
)

