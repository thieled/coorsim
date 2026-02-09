# data-raw/llm_prompts.R

# Users - Prompts -------------------------------------------------------------------

# # Prompt - Users
prompt_user <- "Return valid JSON only — no explanations, no comments."

# System prompts - Users 
system_user <- "
You are an expert social media analyst, describing users.
Summarize a user's posts and metadata as structured JSON following the schema.

Include:
- 'description': EXACTLY 5 sentences following this structure:
  Sentence 1: User's language/location and main topics addressed.
  Sentence 2: Focal entities and their evaluation (supportive, dismissive, neutral). If no focal entities: state 'No focal entities are repeatedly referenced.'
  Sentence 3: Repetitive patterns (emojis, hashtags, phrases). If none: state 'No repetitive patterns are present.'
  Sentence 4: Tone and style characteristics (emotion_valence, incivility, elaborate).
  Sentence 5: Noteworthy details beyond what was already described. If none: state 'No striking features are observable.'
- 'lang': language code.
- 'topic': up to 5 topical categories.
- 'named_entities': up to 3 key persons, organizations, or countries, each with sentiment.
- 'repetitive_patterns': repeated emojis, slogans, hashtags, or stylistic markers.
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
  ), auto_unbox = TRUE, pretty = FALSE),
  
  # Example: U.S. political / Iraq-related spammer
  jsonlite::toJSON(list(
    user_name = "Alpha044IraqiHero",
    posts = list(
      list(content = "@WhiteHouse @JoeBiden @UNClimateSummit t.co/."),
      list(content = "@JoeBiden @UNClimateSummit t.co/."),
      list(content = "@TheDemocrats @UNClimateSummit @JoeBiden t.co/."),
      list(content = "@SenateGOP @UNClimateSummit @USUN t.co/.")
    )
  ), auto_unbox = TRUE, pretty = FALSE)
  
)


# Answers - Users
example_user_answer <- c(
  # Example 1: British nationalist user
  '{
    "description": "The English-speaking user posts about politics, migration, and society, namely about UK sovereignty and immigration control. The user repeatedly supports Nigel Farage and UKIP while expressing dismissive views toward the EU. The user frequently uses the purple heart emoji 💜 and the hashtag #Farage2024. The tone is abrasive and emotional with negative valence, using uncivil language, and the style is moderately elaborate. A striking feature is the consistent nationalist framing combined with emotionally charged rhetoric.",
    "lang": "en",
    "topic": ["politics", "society", "migration"],
    "named_entities": [
      {"entity": "Nigel Farage", "sentiment": "positive"},
      {"entity": "UKIP", "sentiment": "positive"},
      {"entity": "EU", "sentiment": "negative"}
    ],
    "repetitive_patterns": ["💜", "#Farage2024"],
    "incivility": "yes",
    "elaborate": "moderate",
    "confidence": 0.85
  }',
  
  # Example 2: European politics news account
  '{
    "description": "The English-speaking user posts about politics and society, namely about the 2024 European Parliament elections. The user repeatedly references the European Parliament and the EU in a neutral manner. The user frequently uses hashtags #EPelections2024, #EuropeVotes, and #EU. The tone is neutral and informative with neutral emotional valence, no incivility, and an elaborate style. No striking features are observable.",
    "lang": "en",
    "topic": ["politics", "society"],
    "named_entities": [
      {"entity": "European Parliament", "sentiment": "neutral"},
      {"entity": "EU", "sentiment": "neutral"}
    ],
    "repetitive_patterns": ["#EPelections2024", "#EuropeVotes", "#EU"],
    "incivility": "no",
    "elaborate": "elaborate",
    "confidence": 0.95
  }',
  
  # Example 3: German pro-Russia peace activist
  '{
    "description": "The German-speaking user posts about politics and security, namely about the Ukraine conflict and peace diplomacy. The user supports Sarah Wagenknecht and Russia while expressing dismissive views toward Ukraine. The user repeatedly uses the German flag 🇩🇪, Russian flag 🇷🇺, and peace sign ✌️ emojis. The tone is emotional and pacifist with positive emotional valence, no incivility, and an elaborate style. A striking feature is the consistent framing of German-Russian friendship as a path to peace.",
    "lang": "de",
    "topic": ["politics", "security"],
    "named_entities": [
      {"entity": "Sarah Wagenknecht", "sentiment": "positive"},
      {"entity": "Russia", "sentiment": "positive"},
      {"entity": "Ukraine", "sentiment": "negative"}
    ],
    "repetitive_patterns": ["🇩🇪", "🇷🇺", "✌️"],
    "incivility": "no",
    "elaborate": "elaborate",
    "confidence": 0.85
  }',

  # Example 4: Humorous / low-effort user
  '{
    "description": "The user\'s language is undetermined and no clear topics are addressed. No focal entities are repeatedly referenced. The user repeatedly uses the laughing emoji sequence 😂😂😂. The tone is expressive with positive emotional valence, no incivility, and a simple style. A striking feature is the absence of any substantive content beyond emoji expression.",
    "lang": "und",
    "topic": [],
    "named_entities": [],
    "repetitive_patterns": ["😂😂😂"],
    "incivility": "no",
    "elaborate": "simple",
    "confidence": 0.6
  }',
  
  # Example: U.S. political / Iraq-related spammer
  '{
  "description": "The English-speaking user potentially based in Iraq posts about politics and society, namely about U.S. political actors. The user repeatedly references Joe Biden and the White House in a neutral manner. The user frequently uses @JoeBiden, t.co/ links, and @UNClimateSummit tags. The tone is neutral with neutral emotional valence, no incivility, and a simple style. A striking feature is the spam-like behavior with minimal original content beyond tags and links.",
  "lang": "en",
  "topic": ["politics", "society"],
  "named_entities": [
    {"entity": "Joe Biden", "sentiment": "neutral"},
    {"entity": "White House", "sentiment": "neutral"}
  ],
  "repetitive_patterns": ["@JoeBiden", "t.co/", "@UNClimateSummit"],
  "incivility": "no",
  "elaborate": "simple",
  "confidence": 0.9
}'
  
)


# Users - Schema ----------------------------------------------------------

# Output-schema - Users
schema_user <- list(
  type = "object",
  properties = list(
    
    description = list(type = "string", description = "EXACTLY 5 sentences: (1) Language/location and main topics, (2) Focal entities and evaluation, (3) Repetitive patterns, (4) Tone and style, (5) Noteworthy details. State explicitly when information is absent (e.g., 'No focal entities are repeatedly referenced.')."),
    
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
      description = "Up to 3 named persons, organizations, or countries with sentiment.",
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
      maxItems = 3
    ),
    
    repetitive_patterns = list(
      type = "array",
      description = "Repeated emojis, slogans, or phrases.",
      items = list(type = "string"),
      minItems = 0,
      maxItems = 3
    ),
    
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
- 'description': EXACTLY 5 sentences following this structure:
  Sentence 1: Community's predominant language/location and main topics addressed.
  Sentence 2: Focal entities and their typical evaluation across the community. If no focal entities: state 'No focal entities are repeatedly referenced across the community.'
  Sentence 3: Repetitive patterns shared across the community. If none: state 'No shared repetitive patterns are present.'
  Sentence 4: Predominant tone and style characteristics across the community.
  Sentence 5: Noteworthy community-level features beyond what was already described. If none: state 'No striking features are observable.'
- 'lang': predominant language among users.
- 'topic': up to 5 recurring thematic categories.
- 'named_entities': up to 3 key persons, organizations, or countries repeatedly mentioned, each with prevailing sentiment.
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
  '[{"name": "brit_voice92",
     "description": "The English-speaking user posts about politics, migration, and society, namely about UK sovereignty and immigration control. The user repeatedly supports Nigel Farage and UKIP while expressing dismissive views toward the EU. The user frequently uses the purple heart emoji 💜 and the hashtag #Farage2024. The tone is abrasive and emotional with negative valence, using uncivil language, and the style is moderately elaborate. A striking feature is the consistent nationalist framing combined with emotionally charged rhetoric.",
     "lang": "en",
     "topic": ["politics", "society", "migration"],
     "named_entities": [
       {"entity": "Nigel Farage", "sentiment": "positive"},
       {"entity": "UKIP", "sentiment": "positive"},
       {"entity": "EU", "sentiment": "negative"}
     ],
     "repetitive_patterns": ["💜", "#Farage2024"],
     "incivility": "yes",
     "elaborate": "moderate"},
    {"name": "ukpatriot_84",
     "description": "The English-speaking user posts about politics and society, namely about Brexit and national sovereignty. The user supports UKIP and expresses dismissive views toward the EU. The user frequently uses the purple heart emoji 💜 and the hashtag #Farage2024. The tone is patriotic and emotional with negative valence, using uncivil language, and the style is simple. A striking feature is the use of 💜 as a partisan loyalty symbol.",
     "lang": "en",
     "topic": ["politics"],
     "named_entities": [
       {"entity": "UKIP", "sentiment": "positive"},
       {"entity": "EU", "sentiment": "negative"}
     ],
     "repetitive_patterns": ["💜", "#Farage2024"],
     "incivility": "yes",
     "elaborate": "simple"}]',
  
  # Example 2: Pro-Wagenknecht peace community
  '[{"name": "denker_fuer_frieden",
     "description": "The German-speaking user posts about politics and security, namely about the Ukraine conflict and peace diplomacy. The user supports Sarah Wagenknecht and Russia while expressing dismissive views toward NATO. The user repeatedly uses the German flag 🇩🇪, Russian flag 🇷🇺, and peace sign ✌️ emojis. The tone is emotional and pacifist with positive emotional valence, no incivility, and a moderately elaborate style. A striking feature is the consistent framing of German-Russian friendship as a path to peace.",
     "lang": "de",
     "topic": ["politics", "security"],
     "named_entities": [
       {"entity": "Sarah Wagenknecht", "sentiment": "positive"},
       {"entity": "Russia", "sentiment": "positive"},
       {"entity": "NATO", "sentiment": "negative"}
     ],
     "repetitive_patterns": ["🇩🇪", "🇷🇺", "✌️"],
     "incivility": "no",
     "elaborate": "moderate"},
    {"name": "friedenjetzt88",
     "description": "The German-speaking user posts about politics and society, namely about peace with Russia. The user supports Russia while expressing dismissive views toward Ukraine and weapon deliveries. The user repeatedly uses the German flag 🇩🇪 and Russian flag 🇷🇺 emojis. The tone is hopeful and emotional with positive emotional valence, no incivility, and a moderately elaborate style. A striking feature is the naive framing of peace through unilateral friendship.",
     "lang": "de",
     "topic": ["politics", "society"],
     "named_entities": [
       {"entity": "Russia", "sentiment": "positive"},
       {"entity": "Ukraine", "sentiment": "negative"}
     ],
     "repetitive_patterns": ["🇩🇪", "🇷🇺"],
     "incivility": "no",
     "elaborate": "moderate"}]',
  
  # Example 3: Mixed and unclear community
  '[{"name": "mr_niceguy",
     "description": "The user\'s language is undetermined and no clear topics are addressed. No focal entities are repeatedly referenced. The user repeatedly uses the laughing emoji sequence 😂😂😂. The tone is expressive with positive emotional valence, no incivility, and a simple style. A striking feature is the absence of any substantive content beyond emoji expression.",
     "lang": "und",
     "topic": [],
     "named_entities": [],
     "repetitive_patterns": ["😂😂😂"],
     "incivility": "no",
     "elaborate": "simple"},
    {"name": "coolbeans44",
     "description": "The English-speaking user posts about society with no consistent topic focus. No focal entities are repeatedly referenced. The user repeatedly uses lol and the laughing emoji 😂. The tone is sarcastic and informal with neutral to positive valence, no incivility, and a simple style. A striking feature is the informal sharing of URLs without substantive commentary.",
     "lang": "en",
     "topic": ["society"],
     "named_entities": [],
     "repetitive_patterns": ["lol", "😂"],
     "incivility": "no",
     "elaborate": "simple"},
    {"name": "newsreactor_x",
     "description": "The English-speaking user posts about politics and society, namely about news and current events. The user references Biden in a neutral manner. The user repeatedly uses the laughing emoji 😂 and eye-roll emoji 🙄. The tone is mocking and sarcastic with neutral valence, no incivility, and a simple style. A striking feature is the brief reactive commentary without taking clear political stances.",
     "lang": "en",
     "topic": ["politics", "society"],
     "named_entities": [
       {"entity": "Biden", "sentiment": "neutral"}
     ],
     "repetitive_patterns": ["😂", "🙄"],
     "incivility": "no",
     "elaborate": "simple"}]',
  
  # Example 4: U.S.–Iraq political spammer community
  '[{"name": "Alpha044IraqiHero",
     "description": "The English-speaking user potentially based in Iraq posts about politics and society, namely about U.S. political actors. The user repeatedly references Joe Biden and the White House in a neutral manner. The user frequently uses @JoeBiden, t.co/ links, and @UNClimateSummit tags. The tone is neutral with neutral emotional valence, no incivility, and a simple style. A striking feature is the spam-like behavior with minimal original content beyond tags and links.",
     "lang": "en",
     "topic": ["politics", "society"],
     "named_entities": [
       {"entity": "Joe Biden", "sentiment": "neutral"},
       {"entity": "White House", "sentiment": "neutral"}
     ],
     "repetitive_patterns": ["@JoeBiden", "@UNClimateSummit", "t.co/"],
     "incivility": "no",
     "elaborate": "simple"},
    {"name": "A044LeftBehind",
     "description": "The English-speaking user posts about politics and society. The user repeatedly references Joe Biden in a neutral manner. The user frequently uses t.co/ links, @JoeBiden, and @WhiteHouse tags. The tone is neutral with neutral emotional valence, no incivility, and a simple style. A striking feature is the semi-automated pattern focused on link amplification with minimal original content.",
     "lang": "en",
     "topic": ["politics", "society"],
     "named_entities": [
       {"entity": "Joe Biden", "sentiment": "neutral"}
     ],
     "repetitive_patterns": ["t.co/", "@JoeBiden", "@WhiteHouse"],
     "incivility": "no",
     "elaborate": "simple"}]'
)


example_comm_answer <- c(
  '{
    "label": "UKIP Supporter Community",
    "description": "The English-speaking community based in the UK posts about politics and society, namely about nationalism and immigration control. Users consistently support Nigel Farage and UKIP while expressing dismissive views toward the EU. Members frequently share the purple heart emoji 💜 and the hashtag #Farage2024 as political markers. The predominant tone is abrasive and emotional with negative valence, incivility is present, and the style is moderately elaborate. A striking feature is the use of 💜 as a consistent identity symbol for party loyalty.",
    "lang": "en",
    "topic": ["politics", "society", "migration"],
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
    "description": "The German-speaking community based in Germany posts about politics, security, and society, namely about the Ukraine conflict and peace diplomacy. Users support Sarah Wagenknecht and Russia while expressing dismissive views toward NATO. Members frequently share the German flag 🇩🇪, Russian flag 🇷🇺, and peace sign ✌️ emojis. The predominant tone is emotional and pacifist with positive valence, no incivility, and a moderately elaborate style. A striking feature is the symbolic use of flag emojis to express German-Russian friendship as a political statement.",
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
    "description": "The English-speaking community posts about society with minimal substantive or ideological content. No focal entities are repeatedly referenced across the community. Members frequently share laughing emoji 😂, the phrase lol, and the eye-roll emoji 🙄. The predominant tone is casual and expressive with neutral to positive valence, no incivility, and a simple style. A striking feature is the complete absence of political or argumentative content across the community.",
    "lang": "en",
    "topic": ["society"],
    "named_entities": [],
    "repetitive_patterns": ["😂", "lol", "🙄"],
    "incivility": "no",
    "elaborate": "simple",
    "confidence": 0.6
  }',
  
  '{
    "label": "US–Iraq Political Spammer Network",
    "description": "The English-speaking community potentially based in Iraq posts about politics and society, namely about U.S. political actors and online behavior. Users repeatedly reference Joe Biden and the White House in a neutral manner. Members frequently share @JoeBiden, @UNClimateSummit tags, and t.co/ links. The predominant tone is neutral with neutral emotional valence, no incivility, and a simple style. A striking feature is the coordinated spam-like behavior with minimal original content and recurring 044 identifiers suggesting automated activity.",
    "lang": "en",
    "topic": ["politics", "society"],
    "named_entities": [
      {"entity": "Joe Biden", "sentiment": "neutral"},
      {"entity": "White House", "sentiment": "neutral"}
    ],
    "repetitive_patterns": ["@JoeBiden", "@UNClimateSummit", "t.co/"],
    "incivility": "no",
    "elaborate": "simple",
    "confidence": 0.88
  }'
)


# Slices ------------------------------------------------------------------

# System Prompt - Communities
system_comm_slices <- "
You are an expert social media analyst, describing user communities.
Summarize a community of social media users based on the structured descriptions of slices of this community, providing a structured JSON following the schema.

Include:
- 'label': concise, informative English label characterizing the community.
- 'description': EXACTLY 5 sentences following this structure:
  Sentence 1: Community's predominant language/location and main topics addressed.
  Sentence 2: Focal entities and their typical evaluation across slices. If no focal entities: state 'No focal entities are repeatedly referenced across the community.'
  Sentence 3: Repetitive patterns shared across slices. If none: state 'No shared repetitive patterns are present.'
  Sentence 4: Predominant tone and style characteristics synthesized from slices.
  Sentence 5: Noteworthy community-level features beyond what was already described. If none: state 'No striking features are observable.'
- 'lang': predominant language among users.
- 'topic': up to 5 recurring thematic categories.
- 'named_entities': up to 3 key persons, organizations, or countries repeatedly mentioned, each with prevailing sentiment.
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
     "description":"The English-speaking community based in the UK posts about politics and society, namely about sovereignty and immigration control. Users consistently support Nigel Farage and UKIP with assertive nationalism. Members frequently share the purple heart emoji 💜 and the hashtag #Farage2024 as party loyalty signals. The predominant tone is confident, emotional, and identity-driven with negative valence, incivility is present, and the style is moderately elaborate. A striking feature is the emphatic use of 💜 as a political identity marker."},
    {"slice":2,"share":25,"label":"Brexit Advocates Criticizing Mainstream Conservatives",
     "description":"The English-speaking community based in the UK posts about politics and society, namely about Brexit and Conservative Party failures. Users support Farage and UKIP while expressing dismissive views toward the Conservative Party and establishment politicians. Members frequently share anti-establishment rhetoric and UKIP-related hashtags. The predominant tone is accusatory, anti-elite, and assertively nationalist with negative valence, incivility is present, and the style is moderately elaborate. A striking feature is the framing of establishment betrayal versus UKIP\'s true vision."}]',
  
  # --- Example 2: Pro-Wagenknecht Peace Community ---
  '[{"slice":1,"share":65,"label":"Pro-Wagenknecht Peace Supporters",
     "description":"The German-speaking community based in Germany posts about politics and security, namely about the Ukraine conflict and diplomatic solutions. Users support Sarah Wagenknecht while expressing opposition to militarism. Members frequently share peace-related emojis and German flag symbols. The predominant tone is emotional and patriotic with positive valence, no incivility, and a moderately elaborate style. A striking feature is the emphasis on peace, neutrality, and diplomatic alternatives to military escalation."},
    {"slice":2,"share":30,"label":"Pacifists and Pro-Russian Sympathizers",
     "description":"The German-speaking community posts about politics and security, namely about peaceful relations with Russia. Users support Russia while expressing dismissive views toward NATO and its escalation policies. Members frequently share the German flag 🇩🇪 and Russian flag 🇷🇺 emojis to express friendship and solidarity. The predominant tone is anti-war, sentimental, and hopeful with positive valence, no incivility, and a moderately elaborate style. A striking feature is the symbolic use of flag emojis to promote German-Russian friendship as a political statement."}]',
  
  # --- Example 3: Mixed and Reaction-Based Users ---
  '[{"slice":1,"share":55,"label":"Humorous Reaction Posters",
     "description":"The English-speaking community posts about society with no clear topical focus. No focal entities are repeatedly referenced across the community. Members frequently share laughing emojis 😂 and light-hearted memes. The predominant tone is casual and expressive with positive valence, no incivility, and a simple style. A striking feature is the complete absence of substantive or argumentative content."},
    {"slice":2,"share":35,"label":"Mixed Reaction-Based Users",
     "description":"The English-speaking community posts about society and trending content with minimal topical consistency. No focal entities are repeatedly referenced across the community. Members frequently share sarcastic remarks and reaction emojis. The predominant tone is humorous and sarcastic with neutral valence, no incivility, and a simple style. A striking feature is the lack of argumentation or ideological consistency, dominated by scattered reactions to trending media."}]'
)

# Example answers
example_comm_slices_answer <- c(
  
  # --- Example 1: UKIP ---
  '{
    "label": "UKIP Loyalists and Brexit Hardliners",
    "description": "The English-speaking community based in the UK posts about politics and society, namely about nationalism, immigration control, and Brexit. Users passionately support Nigel Farage and UKIP while expressing dismissive views toward the EU and mainstream Conservatives. Members frequently share the purple heart emoji 💜 and the hashtag #Farage2024 as symbols of party identity. The predominant tone is assertive, populist, and anti-establishment with negative valence, incivility is present, and the style is moderately elaborate. A striking feature is the emotional use of 💜 as a consistent marker of party loyalty and political commitment.",
    "lang": "en",
    "topic": ["politics", "society", "migration"],
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
    "description": "The German-speaking community based in Germany posts about politics, security, and society, namely about the Ukraine conflict and peace diplomacy. Users support Sarah Wagenknecht and Russia while expressing dismissive views toward NATO and its military escalation. Members frequently share the German flag 🇩🇪, Russian flag 🇷🇺, and peace sign ✌️ emojis. The predominant tone is empathetic, hopeful, and pacifist with positive valence, no incivility, and a moderately elaborate style. A striking feature is the emotional framing of pacifism through patriotic sentiment and the symbolic use of flag emojis to promote German-Russian friendship.",
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
    "description": "The English-speaking community posts about society with minimal substantive or ideological content. No focal entities are repeatedly referenced across the community. Members frequently share laughing emoji 😂, the phrase lol, and the eye-roll emoji 🙄. The predominant tone is casual, expressive, and non-political with neutral to positive valence, no incivility, and a simple style. A striking feature is the complete lack of argumentative or substantive content, with activity dominated by humorous reactions and sarcasm.",
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
      description = "EXACTLY 5 sentences: (1) Language/location and main topics, (2) Focal entities and evaluation, (3) Repetitive patterns, (4) Tone and style, (5) Noteworthy details. State explicitly when information is absent (e.g., 'No focal entities are repeatedly referenced across the community.')."
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
      description = "Up to 3 key persons, organizations, or countries discussed within the community, with prevailing sentiment.",
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
      maxItems = 3
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
