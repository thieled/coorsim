# data-raw/llm_prompts.R

# Users - Prompts -------------------------------------------------------------------

# # Prompt - Users
prompt_user <- "Return valid JSON only — no explanations, no comments."

# System prompts - Users 
system_user <- "
You are an expert social media analyst, describing users.
Summarize a user's posts and metadata as structured JSON following the schema.

Include:
- 'description': brief English summary of topics, entities, sentiment, regional focus, tone, and style.
- 'lang': language code.
- 'topic': up to 5 topical categories.
- 'named_entities': up to 5 key persons, organizations, or countries, each with sentiment.
- 'repetitive_patterns': repeated emojis, slogans, hashtags, or stylistic markers.
- 'emotion_valence': overall emotional valence of the posts (positive, negative, neutral).
- 'incivility': whether posts use uncivil, foul, or offensive language (yes, no).
- 'elaborate': whether language style is rather elaborate or simple (elaborate, simple).
- 'confidence': overall annotation certainty (0–1 scale).

Be concise and base all annotations solely on text after ### NEW INPUT ###!
Use ### EXAMPLES ### only to understand the output format, but ignore its content during annotation.
"


# Users - Examples --------------------------------------------------------

# Examples - Users
example_user_text <- c(
  
  # Example 1: British nationalist user
  jsonlite::toJSON(list(
    user_name = "brit_voice92",
    posts = list(
      list(content = "Nigel #Farage2024 is the only one standing up for real British values. 💜 Time to take our country back."),
      list(content = "Sick of the open border madness — UKIP speaks for the people. #Farage2024 💜💜"),
      list(content = "Fuck EU! No to woke politics. Yes to UKIP! 💜 #Farage2024")
    )
  ), auto_unbox = TRUE, pretty = FALSE),
  
  # Example 2: European politics news account
  jsonlite::toJSON(list(
    user_name = "EUinsight",
    posts = list(
      list(content = "Between 6 and 9 June 2024, around 373 million Europeans were eligible to elect the 720 members of the new European Parliament."),
      list(content = "Preliminary results show voter turnout at roughly 51 percent — the highest participation rate in twenty years. #EPelections2024 #EU #EuropeVotes"),
      list(content = "Across the EU, conservative and far-right parties made notable gains, while centrist and green groups lost seats in several member states."),
      list(content = "These shifts could influence the appointment of the next European Commission and the future direction of EU policy. #EPelections2024 #EuropeVotes #EU")
    )
  ), auto_unbox = TRUE, pretty = FALSE),
  
  # Example 3: German pro-Russia peace activist
  jsonlite::toJSON(list(
    user_name = "denker_fuer_frieden",
    posts = list(
      list(content = "Nur Wagenknecht spricht sich mutig gegen den Krieg aus! 🇩🇪🤝🇷🇺"),
      list(content = "Wir brauchen Frieden, nicht Panzer – Freundschaft mit Russland statt Eskalation! 🇷🇺🇩🇪"),
      list(content = "Niemand außer Wagenknecht fordert einen echten diplomatischen Weg zur Beendigung des Ukraine-Kriegs. ✌️")
    )
  ), auto_unbox = TRUE, pretty = FALSE),
  
  # Example 4: Humorous / low-effort user
  jsonlite::toJSON(list(
    user_name = "mr_niceguy",
    posts = list(
      list(content = "😂😂😂"),
      list(content = "😂😂😂"),
      list(content = "😂😂😂")
    )
  ), auto_unbox = TRUE, pretty = FALSE)
)


# Answers - Users
example_user_answer <- c(
  '{
    "description": "UK user supporting Nigel Farage and UKIP, opposing immigration and the EU. Tone is abrasive, emotional, and nationalist with frequent 💜 emojis.",
    "lang": "en",
    "topic": ["politics", "migration", "society"],
    "named_entities": [
      {"entity": "Nigel Farage", "sentiment": "positive"},
      {"entity": "UKIP", "sentiment": "positive"},
      {"entity": "EU", "sentiment": "negative"}
    ],
    "repetitive_patterns": ["💜", "#Farage2024"],
    "emotion_valence": "negative",
    "incivility": "yes",
    "elaborate": "moderate",
    "confidence": 0.85
  }',
  
  '{
    "description": "EU-focused account sharing factual updates about the 2024 European Parliament elections, including turnout and voting patterns. Tone is neutral and informative.",
    "lang": "en",
    "topic": ["politics", "society"],
    "named_entities": [
      {"entity": "European Parliament", "sentiment": "neutral"},
      {"entity": "EU", "sentiment": "neutral"}
    ],
    "repetitive_patterns": ["EPelections2024", "EuropeVotes", "#EU"],
    "emotion_valence": "neutral",
    "incivility": "no",
    "elaborate": "elaborate",
    "confidence": 0.95
  }',
  
  '{
    "description": "German user endorsing Sarah Wagenknecht’s anti-war stance and advocating peace with Russia. Posts emphasize diplomacy and friendship. Tone is emotional and pacifist.",
    "lang": "de",
    "topic": ["politics", "security"],
    "named_entities": [
      {"entity": "Wagenknecht", "sentiment": "positive"},
      {"entity": "Russia", "sentiment": "positive"},
      {"entity": "Ukraine", "sentiment": "negative"}
    ],
    "repetitive_patterns": ["🇩🇪", "🇷🇺", "✌️"],
    "emotion_valence": "positive",
    "incivility": "no",
    "elaborate": "elaborate",
    "confidence": 0.85
  }',

  
  '{
    "description": "User posting repeated laughing emojis without substantial content. The content appears non-political and purely expressive.",
    "lang": "und",
    "topic": [],
    "named_entities": [],
    "repetitive_patterns": ["😂😂😂"],
    "emotion_valence": "positive",
    "incivility": "no",
    "elaborate": "simple",
    "confidence": 0.6
  }'
  
)


# Users - Schema ----------------------------------------------------------

# Output-schema - Users
schema_user <- list(
  type = "object",
  properties = list(
    
    description = list(type = "string", description = "Concise English summary of main topics, entities, sentiment, regional focus, tone, and style in the user's content."),
    
    lang = list(type = "string", description = "Language code (ISO code, e.g., 'en', 'de')."),
    
    topic = list(
      type = "array",
      description = "Up to 5 topics.",
      items = list(
        type = "string",
        enum = c("politics",
                 "security",
                 "economy",
                 "society",
                 "culture",
                 "science",
                 "environment",
                 "migration")
      ),
      uniqueItems = TRUE,
      minItems = 0,
      maxItems = 5
    ),
    
    named_entities = list(
      type = "array",
      description = "Up to 5 named persons, organizations, or countries with sentiment.",
      items = list(
        type = "object",
        properties = list(
          entity = list(type = "string"),
          sentiment = list(
            type = "string",
            enum = c("positive", "negative", "neutral")
          )
        ),
        required = c("entity", "sentiment")
      ),
      minItems = 0,
      maxItems = 5
    ),
    
    repetitive_patterns = list(
      type = "array",
      description = "Repeated emojis, slogans, or phrases.",
      items = list(type = "string"),
      minItems = 0,
      maxItems = 3
    ),
    
    # emotion_valence = list(
    #   type = "string",
    #   enum = c("positive", "negative", "neutral"),
    #   description = "Overall emotional tone of posts."
    # ),
    
    incivility = list(
      type = "string",
      enum = c("yes", "no"),
      description = "Presence of derogatory language, insults, swear-words."
    ),
    
    elaborate = list(
      type = "string",
      enum = c("elaborate", "moderate", "simple"),
      description = "Linguistic complexity and verbosity of post."
    ),
    
    confidence = list(
      type = "number",
      minimum = 0,
      maximum = 1,
      description = "Rate your overall confidence in annotating this user, scaled 0–1."
    )
  ),
  required = c(
    "description", 
    "lang", 
    "topic", 
    "named_entities",
    "repetitive_patterns", 
  #  "emotion_valence", 
    "incivility",
    "elaborate",
    "confidence"
  )
)


# Labelling Communities ---------------------------------------------------

# Prompt - Communities
prompt_comm <- "Return valid JSON only — no explanations, no comments."

# System Prompt - Communities
system_comm <- "
You are an expert social media analyst, describing user communities.
Summarize a community of social media users based on their descriptions and metadata in a structured JSON following the schema.

Include:
- 'label': concise, informative English label characterizing the community.
- 'description': concise English summary of the community’s topics, entities, sentiments, regional focus, tone, and style.
- 'lang': predominant language among users.
- 'topic': up to 5 recurring thematic categories.
- 'named_entities': up to 5 key persons, organizations, or countries repeatedly mentioned, each with prevailing sentiment.
- 'repetitive_patterns': recurring emojis, slogans, hashtags, or stylistic markers.
- 'incivility': whether the community commonly uses uncivil, foul, or offensive language (yes, no).
- 'elaborate': typical linguistic elaboration (elaborate, moderate, simple).
- 'confidence': your confidence in the overall annotation, scaled 0–1.

Be concise and base all annotations solely on text after ### NEW INPUT ###!
Use ### EXAMPLES ### only to understand the output format, but ignore its content during annotation.
"

# Examples / Answers - Communities ----------------------------------------

example_comm_text <- c(
  # Example 1: UKIP community
  '[{"description": "UK user supporting Nigel Farage and UKIP, opposing immigration and the EU. Tone is abrasive and emotional.", "lang": "en", "topic": ["politics","society"], "named_entities": [{"entity":"Nigel Farage","sentiment":"positive"}, {"entity":"UKIP","sentiment":"positive"}, {"entity":"EU","sentiment":"negative"}], "repetitive_patterns":["💜","#Farage2024"], "incivility":"yes","elaborate":"moderate"},
    {"description": "User praising UKIP and Farage with patriotic tone. Frequent 💜 emojis indicate partisan loyalty.", "lang":"en", "topic":["politics"], "named_entities":[{"entity":"UKIP","sentiment":"positive"},{"entity":"EU","sentiment":"negative"}], "repetitive_patterns":["💜","#Farage2024"], "incivility":"yes","elaborate":"simple"}]',
  
  # Example 2: Pro-Wagenknecht peace community
  '[{"description": "German user endorsing Sarah Wagenknecht’s peace initiatives and criticizing NATO militarism. Tone is emotional, pacifist, and patriotic.", "lang":"de", "topic":["politics","security"], "named_entities":[{"entity":"Sarah Wagenknecht","sentiment":"positive"},{"entity":"Russia","sentiment":"positive"},{"entity":"NATO","sentiment":"negative"}], "repetitive_patterns":["🇩🇪","🇷🇺","✌️"], "incivility":"no","elaborate":"moderate"},
    {"description": "German user promoting friendship with Russia and opposing weapons delivery to Ukraine. Tone is hopeful and naive.", "lang":"de", "topic":["politics","society"], "named_entities":[{"entity":"Russia","sentiment":"positive"},{"entity":"Ukraine","sentiment":"negative"}], "repetitive_patterns":["🇩🇪","🇷🇺"], "incivility":"no","elaborate":"moderate"}]',
  
  # Example 3: Mixed and unclear community
  '[{"description": "User posting repeated laughing emojis without substantial content. The content appears non-political and purely expressive.", "lang": "und", "topic": [], "named_entities": [], "repetitive_patterns": ["😂😂😂"], "incivility": "no", "elaborate": "simple"},
    {"description": "User sharing URLs and humorous comments with no consistent topic. Posts are sarcastic and informal, sometimes including light profanity.", "lang": "en", "topic": ["society"], "named_entities": [], "repetitive_patterns": ["lol", "😂"], "incivility": "no", "elaborate": "simple"},
    {"description": "User reacting to news on CNN.com with emojis and brief remarks, sometimes mocking politicians without clear stance.", "lang": "en", "topic": ["politics","society"], "named_entities": [{"entity":"Biden","sentiment":"neutral"}], "repetitive_patterns":["😂","🙄"], "incivility": "no", "elaborate": "simple"}]'
)

example_comm_answer <- c(
  '{
    "label": "UKIP Supporter Community",
    "description": "UK-based users showing consistent and emotionally expressive support for Nigel Farage and UKIP. Posts emphasize nationalism, anti-EU sentiment, and frequent 💜 emojis as political markers.",
    "lang": "en",
    "topic": ["politics", "society"],
    "named_entities": [
      {"entity": "Nigel Farage", "sentiment": "positive"},
      {"entity": "UKIP", "sentiment": "positive"},
      {"entity": "EU", "sentiment": "negative"}
    ],
    "repetitive_patterns": ["💜", "#Farage2024"],
    "incivility": "yes",
    "elaborate": "moderate",
    "confidence": 0.9
  }',
  
  '{
    "label": "Pro-Wagenknecht Peace Advocates",
    "description": "Germany-based users advocating for peace with Russia and supporting Sarah Wagenknecht’s diplomatic stance. Tone is emotional, hopeful, and pacifist, with 🇩🇪 and 🇷🇺 emojis symbolizing German-Russian friendship.",
    "lang": "de",
    "topic": ["politics", "security", "society"],
    "named_entities": [
      {"entity": "Sarah Wagenknecht", "sentiment": "positive"},
      {"entity": "Russia", "sentiment": "positive"},
      {"entity": "NATO", "sentiment": "negative"}
    ],
    "repetitive_patterns": ["🇩🇪", "🇷🇺", "✌️"],
    "incivility": "no",
    "elaborate": "moderate",
    "confidence": 0.9
  }',
  
  '{
    "label": "Diverse Reaction-Based Users Lacking Substance",
    "description": "Mixed group of users posting humorous reactions, emojis, and URLs with minimal substantive content. Tone is casual, expressive, and non-political, marked by frequent use of laughing emojis.",
    "lang": "en",
    "topic": ["society"],
    "named_entities": [],
    "repetitive_patterns": ["😂", "lol", "🙄"],
    "incivility": "no",
    "elaborate": "simple",
    "confidence": 0.6
  }'
)

# Slices ------------------------------------------------------------------

# System Prompt - Communities
system_comm_slices <- "
You are an expert social media analyst, describing user communities.
Summarize a community of social media users based on the structured descriptions of slices of this community, providing a structured JSON following the schema.

Include:
- 'label': concise, informative English label characterizing the community.
- 'description': concise English summary of the community’s topics, entities, sentiments, regional focus, tone, and style.
- 'lang': predominant language among users.
- 'topic': up to 5 recurring thematic categories.
- 'named_entities': up to 5 key persons, organizations, or countries repeatedly mentioned, each with prevailing sentiment.
- 'repetitive_patterns': recurring emojis, slogans, hashtags, or stylistic markers.
- 'incivility': whether the community commonly uses uncivil, foul, or offensive language (yes, no).
- 'elaborate': typical linguistic elaboration (elaborate, moderate, simple).
- 'confidence': your confidence in the overall annotation, scaled 0–1.

Be concise and base all annotations solely on text after ### NEW INPUT ###!
Use ### EXAMPLES ### only to understand the output format, but ignore its content during annotation.
"



# Aggregating slices
example_comm_slices_text <- c(
  
  # --- Example 1: UKIP Community ---
  '[{"slice":1,"share":70,"label":"UKIP Loyalists with Emphatic Emoji Use",
     "description":"UK-based users consistently promoting Nigel Farage and UKIP, frequently using purple heart emojis (💜) to signal party loyalty. Posts emphasize sovereignty, anti-immigration stances, and assertive nationalism. Tone is confident, emotional, and identity-driven."},
    {"slice":2,"share":25,"label":"Brexit Advocates Criticizing Mainstream Conservatives",
     "description":"British users expressing support for Farage while criticizing the Conservative Party’s handling of Brexit. Posts highlight betrayal by establishment politicians and praise UKIP’s \"true\" vision. Tone is accusatory, anti-elite, and assertively nationalist."}]',
  
  # --- Example 2: Pro-Wagenknecht Peace Community ---
  '[{"slice":1,"share":65,"label":"Pro-Wagenknecht Peace Supporters",
     "description":"German users supporting Sarah Wagenknecht’s call for diplomatic solutions to the Ukraine conflict. Posts emphasize peace, neutrality, and opposition to militarism. Tone is emotional and patriotic."},
    {"slice":2,"share":30,"label":"Pacifists and Pro-Russian Sympathizers",
     "description":"German-speaking users promoting peaceful relations with Russia, often using 🇩🇪 and 🇷🇺 emojis to express friendship and solidarity. Posts are anti-war, sentimental, and critical of NATO’s escalation."}]',
  
  # --- Example 3: Mixed and Reaction-Based Users ---
  '[{"slice":1,"share":55,"label":"Humorous Reaction Posters",
     "description":"Users frequently posting laughing emojis and light-hearted memes without clear topical focus. Tone is casual and expressive."},
    {"slice":2,"share":35,"label":"Mixed Reaction-Based Users",
     "description":"Accounts sharing scattered humorous or sarcastic remarks, sometimes reacting to trending content or media posts, with minimal argumentation or ideological consistency."}]'
)

# Example answers
example_comm_slices_answer <- c(
  
  # --- Example 1: UKIP ---
  '{
    "label": "UKIP Loyalists and Brexit Hardliners",
    "description": "UK-based users passionately supporting Nigel Farage and UKIP, using purple heart emojis (💜) to express party identity and emotional commitment. Posts reflect strong nationalist and anti-immigration themes, often attacking mainstream Conservatives for Brexit compromises. Tone is assertive, populist, and anti-establishment.",
    "lang": "en",
    "topic": ["politics", "society"],
    "named_entities": [
      {"entity": "Nigel Farage", "sentiment": "positive"},
      {"entity": "UKIP", "sentiment": "positive"},
      {"entity": "EU", "sentiment": "negative"}
    ],
    "repetitive_patterns": ["💜", "#Farage2024"],
    "incivility": "yes",
    "elaborate": "moderate",
    "confidence": 0.9
  }',
  
  # --- Example 2: Wagenknecht ---
  '{
    "label": "Pro-Wagenknecht Peace Advocates",
    "description": "Germany-based users supporting Sarah Wagenknecht’s diplomatic stance and promoting peace with Russia. Posts emphasize emotional pacifism, patriotic sentiment, and criticism of NATO’s military actions. Tone is empathetic, hopeful, and anti-escalation.",
    "lang": "de",
    "topic": ["politics", "security", "society"],
    "named_entities": [
      {"entity": "Sarah Wagenknecht", "sentiment": "positive"},
      {"entity": "Russia", "sentiment": "positive"},
      {"entity": "NATO", "sentiment": "negative"}
    ],
    "repetitive_patterns": ["🇩🇪", "🇷🇺", "✌️"],
    "incivility": "no",
    "elaborate": "moderate",
    "confidence": 0.9
  }',
  
  # --- Example 3: Diverse / Reaction-based ---
  '{
    "label": "Diverse Reaction-Based Users Lacking Substance",
    "description": "Mixed group of users posting humorous reactions, emojis, and memes with minimal substantive or ideological content. Tone is casual, expressive, and non-political, dominated by laughing emojis and sarcasm.",
    "lang": "en",
    "topic": ["society"],
    "named_entities": [],
    "repetitive_patterns": ["😂", "lol", "🙄"],
    "incivility": "no",
    "elaborate": "simple",
    "confidence": 0.6
  }'
)


# Schema - Communities ----------------------------------------------------

schema_comm <- list(
  type = "object",
  properties = list(
    
    label = list(
      type = "string",
      description = "Concise and informative English label characterizing the community."
    ),
    
    description = list(
      type = "string",
      description = "Brief English summary describing the main topics, entities, sentiment, regional focus, tone, and style of the community."
    ),
    
    lang = list(
      type = "string",
      description = "Predominant language of users in the community (ISO code, e.g., 'en', 'de')."
    ),
    
    topic = list(
      type = "array",
      description = "Up to 5 recurring topics.",
      items = list(
        type = "string",
        enum = c("politics",
                 "security",
                 "economy",
                 "society",
                 "culture",
                 "science",
                 "environment",
                 "migration")
      ),
      uniqueItems = TRUE,
      minItems = 0,
      maxItems = 5
    ),
    
    named_entities = list(
      type = "array",
      description = "Up to 5 key persons, organizations, or countries discussed within the community, with prevailing sentiment.",
      items = list(
        type = "object",
        properties = list(
          entity = list(type = "string"),
          sentiment = list(
            type = "string",
            enum = c("positive", "negative", "neutral")
          )
        ),
        required = c("entity", "sentiment")
      ),
      minItems = 0,
      maxItems = 5
    ),
    
    repetitive_patterns = list(
      type = "array",
      description = "Recurring emojis, slogans, hashtags, or stylistic markers repeatedly used by members of the community.",
      items = list(type = "string"),
      minItems = 0,
      maxItems = 3
    ),
    
    incivility = list(
      type = "string",
      enum = c("yes", "no"),
      description = "Indicates whether posts from this community often contain uncivil, foul, or offensive language."
    ),
    
    elaborate = list(
      type = "string",
      enum = c("elaborate", "moderate", "simple"),
      description = "Typical linguistic complexity and verbosity within the community."
    ),
    
    confidence = list(
      type = "number",
      minimum = 0,
      maximum = 1,
      description = "Model-assessed confidence in the community-level summary, scaled 0–1."
    )
  ),
  required = c(
    "label",
    "description",
    "lang",
    "topic",
    "named_entities",
    "repetitive_patterns",
    "incivility",
    "elaborate",
    "confidence"
  )
)


# Bind, store  ------------------------------------------------------------

prompts <- list(prompt_user = prompt_user, 
                prompt_comm = prompt_comm,
                system_user = system_user, 
                system_comm = system_comm,
                system_comm_slices = system_comm_slices
)

examples <- list(example_user_text = example_user_text, 
                 example_user_answer = example_user_answer,
                 example_comm_text = example_comm_text, 
                 example_comm_answer = example_comm_answer,
                 example_comm_slices_text = example_comm_slices_text, 
                 example_comm_slices_answer = example_comm_slices_answer)


schemata <- list(schema_user = schema_user,
                 schema_comm = schema_comm)

# Recreate clean data
unlink("data/prompts.rda")
unlink("data/examples.rda")
unlink("data/schemata.rda")

### Save them
usethis::use_data(
  prompts, examples, schemata, 
  internal = FALSE, overwrite = TRUE, ascii = FALSE
)

devtools::document()
