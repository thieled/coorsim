# data-raw/llm_prompts.R

####################################################################################
#
# Users - Prompts -------------------------------------------------------------------
#
####################################################################################

### Version
# Level: Users
# Version: 0.4
# Date-Time: 2026-04-28

# Users - Prompts -------------------------------------------------------------------

# Prompt - Users
prompt_user <- "Return valid JSON only — no explanations, no comments."

# System prompts - Users 
system_user <- "
You are a social media analyst describing users.
Output valid JSON following the schema.

# Fields to include:

  ## Open text fields:
  - 'description': Object containing EXACTLY 5 separate sentences that describe this user:
    + sentence_1: [Location/language and topics] **In ONE sentence**, describe the user's language (inferred from post content, not just metadata) and/or location (from location field, flags, or context), followed by up to three main topics from: politics, society, culture, economy, security, migration, climate, science, other. Add subtopics if salient (e.g., ', namely ...'). **If no topics clearly apply, describe what the user focuses on based on post content.**
    + sentence_2: [Named entities and sentiment] **In ONE sentence**, report repeatedly mentioned (≥2x) named entities with sentiment (positive/neutral/negative) and frequency (e.g., 'The user frequently mentions [entity] in a [sentiment] manner.'), or state 'No entities are repeatedly mentioned' if none exist. **Named entities are ONLY persons, organizations, or countries—NOT topics, themes, or concepts. Do not confuse concepts like 'climate crisis' or video/channel metadata with named entities.**
    + sentence_3: [Repetitive patterns] **In ONE sentence**, list up to 3 repetitive character-string patterns (emojis, hashtags, tags, phrases, URLs) appearing ≥2x across ≥2 posts (e.g., 'The user frequently uses [pattern list].'), or state 'No repetitive patterns are present' if none exist. **Patterns must appear in the post content itself, not in bio/metadata. Note: YouTube comments rarely contain hashtags. Do not report channel names, video titles, or usernames as patterns unless used by the user.**
    + sentence_4: [Tone and style] **In ONE sentence**, characterize the tone and style by integrating emotional valence, incivility, and elaborateness into descriptive labels (e.g., neutral-informative, aggressive, enthusiastic), or state why the tone cannot be characterized if applicable.
    + sentence_5: [Striking features] **In ONE sentence**, identify striking features from posting BEHAVIOR observable in posts—such as spam-like/campaign behavior, exact post duplication, unusual metadata patterns, political expressions, behavioral peculiarities, unique stylistic choices, particularly strong topic focus, or other notable characteristics (e.g., 'A striking feature is [specific feature].'), or state 'No striking features are observable' if none exist. **Do not use bio information. Do not repeat info from sentences 1-4.**

  ## Other fields:
  - 'lang': language code (ISO 639-2). Infer from post content.
  - 'topic': up to 3 most salient topics from: politics, security, economy, society, science, culture, migration, climate, other.
  - 'named_entities': List up to 3 repeatedly mentioned (≥2x) named entities (persons/organizations/countries) with sentiment (positive/negative/neutral). **Only actual entities—not topics/concepts.**
  - 'repetitive_patterns': List up to 3 repetitive character-string patterns (emojis, hashtags, tags, phrases, URLs) appearing ≥2x across different posts (≥2 posts). **From post content only.**
  - 'incivility': 'yes' if explicit insults/slurs/offensive language present; else 'no'
  - 'elaborate': 'yes' if majority of posts have ≥10 meaningful words with sentence structure; 'no' if mostly short/fragmented

# General guidelines:
- **Annotate faithfully**: All annotations must be verifiable from input after ### NEW INPUT ###. Do not hallucinate.
- **Check post content vs metadata**: Distinguish what the user writes from video titles, channel names, and metadata.
- **Follow instructions closely**: Provide exactly 5 sentences—no more, no less—as separate fields (sentence_1 through sentence_5).
- **Use ### EXAMPLES ### only for format understanding**—ignore their content for annotation.
"


# User examples ----------------------------------------------------------

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
    "description": {
      "sentence_1": "The English-speaking user posts about politics, migration, and society, namely UK sovereignty and immigration control.",
      "sentence_2": "The user repeatedly mentions Nigel Farage and UKIP in a positive manner while expressing negative views toward the EU.",
      "sentence_3": "The user frequently uses the purple heart emoji 💜 and the hashtag #Farage2024.",
      "sentence_4": "The style is abrasive and emotionally charged with uncivil language.",
      "sentence_5": "A striking feature is the consistent nationalist framing combined with anti-EU rhetoric."
    },
    "lang": "en",
    "topic": ["politics", "society", "migration"],
    "named_entities": [
      {"entity": "Nigel Farage", "sentiment": "positive"},
      {"entity": "UKIP", "sentiment": "positive"},
      {"entity": "EU", "sentiment": "negative"}
    ],
    "repetitive_patterns": ["💜", "#Farage2024"],
    "incivility": "yes",
    "elaborate": "yes"
  }',
  
  # Example 2: European politics news account
  '{
    "description": {
      "sentence_1": "The English-speaking user posts about politics and society, namely the 2024 European Parliament elections.",
      "sentence_2": "The user repeatedly mentions the European Parliament and EU in a neutral manner.",
      "sentence_3": "The user frequently uses the hashtags #EPelections2024, #EuropeVotes, and #EU.",
      "sentence_4": "The style is neutral and informative with factual presentation and structured language.",
      "sentence_5": "No striking features are observable."
    },
    "lang": "en",
    "topic": ["politics", "society"],
    "named_entities": [
      {"entity": "European Parliament", "sentiment": "neutral"},
      {"entity": "EU", "sentiment": "neutral"}
    ],
    "repetitive_patterns": ["#EPelections2024", "#EuropeVotes", "#EU"],
    "incivility": "no",
    "elaborate": "yes"
  }',
  
  # Example 3: German pro-Russia peace activist
  '{
    "description": {
      "sentence_1": "The German-speaking user posts about politics and security, namely the Ukraine conflict and peace diplomacy.",
      "sentence_2": "The user repeatedly mentions Sarah Wagenknecht and Russia in a positive manner while expressing negative views toward Ukraine.",
      "sentence_3": "The user frequently uses the German flag 🇩🇪, Russian flag 🇷🇺, and peace sign ✌️ emojis.",
      "sentence_4": "The style is emotional and pacifist with advocacy-oriented language.",
      "sentence_5": "A striking feature is the consistent framing of German-Russian friendship as a path to peace."
    },
    "lang": "de",
    "topic": ["politics", "security"],
    "named_entities": [
      {"entity": "Sarah Wagenknecht", "sentiment": "positive"},
      {"entity": "Russia", "sentiment": "positive"},
      {"entity": "Ukraine", "sentiment": "negative"}
    ],
    "repetitive_patterns": ["🇩🇪", "🇷🇺", "✌️"],
    "incivility": "no",
    "elaborate": "yes"
  }',

  # Example 4: Humorous / low-effort user
  '{
    "description": {
      "sentence_1": "The user\'s language is undetermined and no clear topics are addressed.",
      "sentence_2": "No entities are repeatedly mentioned.",
      "sentence_3": "The user frequently uses the laughing emoji sequence 😂😂😂.",
      "sentence_4": "The style is expressive with minimal text and purely emoji-based communication.",
      "sentence_5": "A striking feature is the complete absence of substantive content beyond emoji expression."
    },
    "lang": "und",
    "topic": [],
    "named_entities": [],
    "repetitive_patterns": ["😂😂😂"],
    "incivility": "no",
    "elaborate": "no"
  }',
  
  # Example: U.S. political / Iraq-related spammer
  '{
  "description": {
    "sentence_1": "The English-speaking user potentially based in Iraq posts about politics and society, namely U.S. political actors.",
    "sentence_2": "The user repeatedly mentions Joe Biden and the White House in a neutral manner.",
    "sentence_3": "The user frequently uses the tags @JoeBiden, @UNClimateSummit, and t.co/ links.",
    "sentence_4": "The style is formulaic and repetitive with minimal original text.",
    "sentence_5": "A striking feature is the spam-like behavior consisting primarily of tags and links."
  },
  "lang": "en",
  "topic": ["politics", "society"],
  "named_entities": [
    {"entity": "Joe Biden", "sentiment": "neutral"},
    {"entity": "White House", "sentiment": "neutral"}
  ],
  "repetitive_patterns": ["@JoeBiden", "t.co/", "@UNClimateSummit"],
  "incivility": "no",
  "elaborate": "no"
}'
  
)


# Users - Schema ----------------------------------------------------------

# Output-schema - Users
schema_user <- list(
  type = "object",
  properties = list(
    
    description = list(
      type = "object",
      description = "Object containing EXACTLY 5 sentences describing the user.",
      properties = list(
        sentence_1 = list(type = "string", description = "Language/location and up to 3 topics (politics, society, culture, economy, security, migration, climate, science, other) with subtopics if salient."),
        sentence_2 = list(type = "string", description = "Repeatedly mentioned (≥2x) named entities (persons/organizations/countries) with sentiment (positive/neutral/negative) and frequency indication."),
        sentence_3 = list(type = "string", description = "Up to 3 repetitive character-string patterns (emojis, hashtags, tags, phrases, URLs) appearing ≥2x across ≥2 posts with frequency indication."),
        sentence_4 = list(type = "string", description = "Tone and style characterization integrating emotional valence, incivility, and elaborateness into descriptive labels."),
        sentence_5 = list(type = "string", description = "Striking features not covered in previous sentences (political expressions, metadata particularities, behavioral patterns, stylistic choices, etc.) or state none observable.")
      ),
      required = c("sentence_1", "sentence_2", "sentence_3", "sentence_4", "sentence_5")
    ),
    
    lang = list(type = "string", description = "Language code (ISO 639-2 code, e.g., 'en', 'de')."),
    
    topic = list(
      type = "array",
      description = "Up to 3 most salient topics.",
      items = list(
        type = "string",
        enum = c("politics",
                 "security",
                 "economy",
                 "society",
                 "science",
                 "culture",
                 "migration",
                 "climate",
                 "other")
      ),
      uniqueItems = TRUE,
      minItems = 0,
      maxItems = 3
    ),
    
    named_entities = list(
      type = "array",
      description = "Up to 3 named entities (persons/organizations/countries) repeatedly mentioned (≥2x) with targeted sentiment (positive/negative/neutral).",
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
      description = "Up to 3 repetitive character-string patterns (emojis, hashtags, tags, phrases, URLs, shortened URLs) appearing repeatedly (≥2x) across different posts (≥2 posts).",
      items = list(type = "string"),
      minItems = 0,
      maxItems = 3
    ),
    
    incivility = list(
      type = "string",
      enum = c("yes", "no"),
      description = "Whether explicit insults, slurs, or offensive language are present."
    ),
    
    elaborate = list(
      type = "string",
      enum = c("yes", "no"),
      description = "'yes' if majority of posts have ≥10 meaningful words with sentence structure; 'no' if mostly short/fragmented."
    )
  ),

  required = c(
    "description", 
    "lang", 
    "topic", 
    "named_entities",
    "repetitive_patterns", 
    "incivility",
    "elaborate"
  )
)


####################################################################################
#
# Communities / Chunks - Prompts -------------------------------------------------------------------
#
####################################################################################


# Community Prompts ------------------------------------------------------

# Prompt - Communities
prompt_comm <- "Return valid JSON only — no explanations, no comments."

# System Prompt - Communities
system_comm <- "
You are a social media analyst summarizing user communities.

## Input
The input contains multiple user-level descriptions in structured JSON format from a previous round of LLM-summarization.

## Aim
Your task is to **characterize** the community of users as a WHOLE following the instructions below. Take ALL users in the input data into account for your community characterization.

## Rules
- **Annotate faithfully**: All annotations must be verifiable from input after ### NEW INPUT ###. Do not hallucinate!
- **Be precise**: Be specific in your characterizations, state the frequency of reported features, and highlight inconsistencies or remarkable similarities.
- **Follow the Instructions**: Each output field has a distinct, fixed task — read every field description carefully and do not mix up their functions.
- **Write complete sentences**: Every desc_ field MUST be a grammatically complete sentence — NOT a comma-separated list of keywords or labels. For example, 'Turkish, politics, society' or 'emotional, positive, elaborate' are INVALID. Write 'The community predominantly speaks Turkish and discusses politics and society.' and 'The predominant tone is emotional and positive with no incivility and an elaborate style.' instead.
- **Use sentence starters**: Begin each desc_ field with the prescribed opener (e.g., 'The community …', 'Users repeatedly mention …', 'Members frequently share …', 'The predominant tone is …', 'A striking feature is …' or the prescribed fallback phrase).
- **Use ### EXAMPLES ### only for format understanding**—summarize the input after ### NEW INPUT ###.

## Fields to Annotate
Output valid JSON, strictly following these instructions and the schema:

- 'label': [Community-Label] Title-Cased noun phrase of 4–8 words characterizing this specific community, by its predominant location or language, its focus on named entities, stances, specific subtopics, or a notable behavior from the input. DO NOT provide 'snake_case' labels. If a community is heterogeneous or ambivalent, state this.
  Examples of required specificity: 'UK Farage Supporters Opposing EU Immigration', 'Pakistani PPP Advocates Framing Bilawal as Climate Leader', 'Arabic Anti-Saudi Government Critics'

- 'desc_lang_topics': [Language and topics] Write ONE complete sentence starting with 'The community predominantly speaks …' (or 'The [language]-speaking community …'), describing the predominant language/location, then name up to 3 topics. Add subtopics if salient (', namely …'). Aggregate across all users — do not copy one user. 

- 'desc_named_entities': [Named entities] Write ONE complete sentence starting with 'Users repeatedly mention …' or 'No focal entities are repeatedly referenced across the community.' Name up to 3 persons/organizations/countries from ≥2 users, each with prevailing sentiment (positive/neutral/negative). 

- 'desc_patterns': [Repetitive patterns] Write ONE complete sentence starting with 'Members frequently share …' or 'No shared repetitive patterns are present.' List up to 3 character-string patterns (emojis, hashtags, tags, phrases, URLs) from ≥2 users. 

- 'desc_tone_style': [Tone and style] Write ONE complete sentence starting with 'The predominant tone is …', integrating emotional valence, majority incivility, and majority elaborateness using descriptive labels (e.g., neutral-informative, aggressive, enthusiastic). 

- 'desc_striking': [Striking community features] Write ONE complete sentence starting with 'A striking feature is …' or 'No striking features are observable.' Identify noteworthy community-level observations not already covered in the other desc_ fields.

- 'lang': predominant ISO 639-2 language code (e.g., 'en', 'und').
- 'topic': up to 3 recurring topics from: politics, security, economy, society, science, culture, migration, climate, other.
- 'named_entities': up to 3 persons/organizations/countries from ≥2 users' named_entities fields, with prevailing sentiment.
- 'repetitive_patterns': up to 3 patterns from ≥2 users' repetitive_patterns fields.
- 'incivility': 'yes' if >50% of users are annotated incivility=yes; else 'no'.
- 'elaborate': 'yes' if >50% of users are annotated elaborate=yes; else 'no'.
- 'confidence': 0–1.

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
     "elaborate": "yes"},
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
     "elaborate": "no"}]',

  # Example 2: U.S.–Iraq political spammer community
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
     "elaborate": "no"},
    {"name": "A044LeftBehind",
     "description": "The English-speaking user posts about politics and society. The user repeatedly references Joe Biden in a neutral manner. The user frequently uses t.co/ links, @JoeBiden, and @WhiteHouse tags. The tone is neutral with neutral emotional valence, no incivility, and a simple style. A striking feature is the semi-automated pattern focused on link amplification with minimal original content.",
     "lang": "en",
     "topic": ["politics", "society"],
     "named_entities": [
       {"entity": "Joe Biden", "sentiment": "neutral"}
     ],
     "repetitive_patterns": ["t.co/", "@JoeBiden", "@WhiteHouse"],
     "incivility": "no",
     "elaborate": "no"}]'
)


example_comm_answer <- c(
  '{
    "label": "UK Nationalist UKIP and Farage Loyalists",
    "desc_lang_topics": "The English-speaking community based in the UK posts about politics, society, and migration, namely about nationalism and immigration control.",
    "desc_named_entities": "Users repeatedly mention Nigel Farage and UKIP in a positive manner while expressing negative views toward the EU.",
    "desc_patterns": "Members frequently share the purple heart emoji 💜 and the hashtag #Farage2024 as political markers.",
    "desc_tone_style": "The predominant tone is abrasive and emotional with negative valence, incivility is present, and the style ranges from simple to moderately elaborate.",
    "desc_striking": "A striking feature is near-identical framing across users: both employ 💜 as a party loyalty symbol and share the same slogans, suggesting coordinated messaging.",
    "lang": "en",
    "topic": ["politics", "society", "migration"],
    "named_entities": [
      {"entity": "Nigel Farage", "sentiment": "positive"},
      {"entity": "UKIP", "sentiment": "positive"},
      {"entity": "EU", "sentiment": "negative"}
    ],
    "repetitive_patterns": ["💜", "#Farage2024"],
    "incivility": "yes",
    "elaborate": "no",
    "confidence": 0.9
  }',

  '{
    "label": "Iraq-Based Biden-Targeting Political Link Spammers",
    "desc_lang_topics": "The English-speaking community potentially based in Iraq posts about politics and society, namely about U.S. political actors.",
    "desc_named_entities": "Users repeatedly mention Joe Biden and the White House in a neutral manner.",
    "desc_patterns": "Members frequently share @JoeBiden tags and t.co/ links across users.",
    "desc_tone_style": "The predominant tone is neutral with neutral emotional valence, no incivility, and a simple style.",
    "desc_striking": "A striking feature is coordinated spam-like behavior — both users share the same tag-and-link template with minimal original content and display the \'044\' identifier pattern in usernames, suggesting automated or organized activity.",
    "lang": "en",
    "topic": ["politics", "society"],
    "named_entities": [
      {"entity": "Joe Biden", "sentiment": "neutral"},
      {"entity": "White House", "sentiment": "neutral"}
    ],
    "repetitive_patterns": ["@JoeBiden", "t.co/"],
    "incivility": "no",
    "elaborate": "no",
    "confidence": 0.88
  }'
)


# Slices ------------------------------------------------------------------

# System Prompt - Communities (Slices)
system_comm_slices <- "
You are a social media analyst summarizing user communities.

## Input
The input contains multiple slice-level community descriptions in structured JSON format from a previous round of LLM-summarization. Each slice represents a subset of users within this community and includes a 'share' field indicating its relative size (%) among all community members.

## Aim
Your task is to **characterize** the community of ALL slices as a WHOLE following the instructions below. Take ALL slices in the input data into account, **weighting each slice by its share** when aggregating.

## Rules
- **Annotate faithfully**: All annotations must be verifiable from input after ### NEW INPUT ###. Do not hallucinate!
- **Weight by share**: When aggregating across slices, weight each slice's characteristics proportionally by its 'share' value. Larger slices should contribute more to the overall characterization.
- **Be precise**: Be specific in your characterizations, state the frequency of reported features, and highlight inconsistencies or remarkable similarities.
- **Follow the Instructions**: Each output field has a distinct, fixed task — read every field description carefully and do not mix up their functions.
- **Write complete sentences**: Every desc_ field MUST be a grammatically complete sentence — NOT a comma-separated list of keywords or labels. For example, 'Turkish, politics, society' or 'emotional, positive, elaborate' are INVALID. Write 'The community predominantly speaks Turkish and discusses politics and society.' and 'The predominant tone is emotional and positive with no incivility and an elaborate style.' instead.
- **Use sentence starters**: Begin each desc_ field with the prescribed opener (e.g., 'The community …', 'Users repeatedly mention …', 'Members frequently share …', 'The predominant tone is …', 'A striking feature is …' or the prescribed fallback phrase).
- **Use ### EXAMPLES ### only for format understanding**—summarize the input after ### NEW INPUT ###.

## Fields to Annotate
Output valid JSON, strictly following these instructions and the schema:

- 'label': [Community-Label] Title-Cased noun phrase of 4–8 words characterizing this specific community, by its predominant location or language, its focus on named entities, stances, specific subtopics, or a notable behavior from the input. DO NOT provide 'snake_case' labels. If a community is heterogeneous or ambivalent, state this.
  Examples of required specificity: 'UK Farage Supporters Opposing EU Immigration', 'Pakistani PPP Advocates Framing Bilawal as Climate Leader', 'Arabic Anti-Saudi Government Critics'

- 'desc_lang_topics': [Language and topics] Write ONE complete sentence starting with 'The community predominantly speaks …' (or 'The [language]-speaking community …'), describing the predominant language/location, then name up to 3 topics. Add subtopics if salient (', namely …'). Aggregate across all slices weighted by share — do not copy one slice.

- 'desc_named_entities': [Named entities] Write ONE complete sentence starting with 'Users repeatedly mention …' or 'No focal entities are repeatedly referenced across the community.' Name up to 3 persons/organizations/countries from ≥2 slices, each with prevailing sentiment (positive/neutral/negative).

- 'desc_patterns': [Repetitive patterns] Write ONE complete sentence starting with 'Members frequently share …' or 'No shared repetitive patterns are present.' List up to 3 character-string patterns (emojis, hashtags, tags, phrases, URLs) from ≥2 slices.

- 'desc_tone_style': [Tone and style] Write ONE complete sentence starting with 'The predominant tone is …', integrating emotional valence, majority incivility (weighted by share), and majority elaborateness (weighted by share) using descriptive labels (e.g., neutral-informative, aggressive, enthusiastic).

- 'desc_striking': [Striking community features] Write ONE complete sentence starting with 'A striking feature is …' or 'No striking features are observable.' Identify noteworthy community-level observations not already covered in the other desc_ fields.

- 'lang': predominant ISO 639-2 language code (e.g., 'en', 'und').
- 'topic': up to 3 recurring topics from: politics, security, economy, society, science, culture, migration, climate, other.
- 'named_entities': up to 3 persons/organizations/countries from ≥2 slices' named_entities fields, with prevailing sentiment.
- 'repetitive_patterns': up to 3 patterns from ≥2 slices' repetitive_patterns fields.
- 'incivility': 'yes' if slices with combined share >50% have incivility=yes; else 'no'.
- 'elaborate': 'yes' if slices with combined share >50% have elaborate=yes; else 'no'.
- 'confidence': 0–1.
"


# Aggregating slices
example_comm_slices_text <- c(

  # --- Example 1: UK COP26 spam community ---
  '[{"slice":1,"share":17.6,"label":"UK Politician-Targeting Spam Accounts","desc_lang_topics":"The English-speaking community posts about politics and society, namely UK politicians and COP26.","desc_named_entities":"Users repeatedly mention Boris Johnson in a neutral manner.","desc_patterns":"Members frequently share @BorisJohnson tags and t.co/ links across users.","desc_tone_style":"The predominant tone is neutral with neutral emotional valence, no incivility, and a simple style.","desc_striking":"A striking feature is coordinated spam-like behavior — all users share the same tag-and-link template with minimal original content, suggesting automated or organized activity.","lang":"en","topic":["politics","society"],"named_entities":[{"entity":"Boris Johnson","sentiment":"neutral"},{"entity":"COP26","sentiment":"neutral"}],"repetitive_patterns":["@BorisJohnson","@COP26","t.co/"],"incivility":"no","elaborate":"no"},{"slice":2,"share":3.3,"label":"UK Politician-Targeting Spam Accounts","desc_lang_topics":"The English-speaking community posts about politics and society, namely UK politicians and COP26.","desc_named_entities":"Users repeatedly mention Boris Johnson in a neutral manner, with some users also mentioning the Lord Speaker, Conservatives, Alok Sharma, Tom Tugendhat, and @10DowningStreet.","desc_patterns":"Members frequently share tags such as @BorisJohnson, @Conservatives, @COP26, @LordSpeaker, @BGIPU, @10DowningStreet, and t.co/ links across users.","desc_tone_style":"The predominant tone is neutral with a formulaic and repetitive style, minimal original text, and no incivility.","desc_striking":"A striking feature is coordinated spam-like behavior — all users share the same tag-and-link template with minimal original content, suggesting automated or organized activity.","lang":"en","topic":["politics","society"],"named_entities":[{"entity":"Boris Johnson","sentiment":"neutral"},{"entity":"Lord Speaker","sentiment":"neutral"},{"entity":"Conservatives","sentiment":"neutral"}],"repetitive_patterns":["@BorisJohnson","@COP26","@10DowningStreet"],"incivility":"no","elaborate":"no"},{"slice":3,"share":0.1,"label":"UK-Based Anti-Institutional and Politically Critical Community","desc_lang_topics":"The English-speaking community based in the UK posts about politics and society, namely UK government actors.","desc_named_entities":"Users repeatedly mention the Royal Family in a negative manner while expressing criticism of hypocrisy.","desc_patterns":"Members frequently share @10DowningStreet tags and t.co/ links across users.","desc_tone_style":"The predominant tone is critical and accusatory with emotive language, incivility is present, and the style ranges from informal to provocative.","desc_striking":"A striking feature is the explicit criticism of UK institutions, with some users employing irony and sarcasm in addressing politicians.","lang":"en","topic":["politics","society"],"named_entities":[{"entity":"Royal Family","sentiment":"negative"},{"entity":"10 Downing Street","sentiment":"neutral"}],"repetitive_patterns":["@10DowningStreet","@COP26","@BorisJohnson"],"incivility":"yes","elaborate":"no"}]',

  # --- Example 2: German pro-Wagenknecht / pro-Russia peace community ---
  '[{"slice":1,"share":65.0,"label":"Pro-Wagenknecht Peace Advocates","desc_lang_topics":"The German-speaking community based in Germany posts about politics and security, namely the Ukraine conflict and peace diplomacy.","desc_named_entities":"Users repeatedly mention Sarah Wagenknecht in a positive manner while expressing criticism of NATO and military escalation.","desc_patterns":"Members frequently share peace-related emojis ✌️ and the German flag 🇩🇪.","desc_tone_style":"The predominant tone is emotional and pacifist with positive valence, no incivility, and a moderately elaborate style.","desc_striking":"A striking feature is the consistent framing of diplomacy as the only valid solution to the Ukraine conflict.","lang":"de","topic":["politics","security"],"named_entities":[{"entity":"Sarah Wagenknecht","sentiment":"positive"},{"entity":"NATO","sentiment":"negative"}],"repetitive_patterns":["✌️","🇩🇪"],"incivility":"no","elaborate":"yes"},{"slice":2,"share":30.0,"label":"Pro-Russian German Sympathizers","desc_lang_topics":"The German-speaking community posts about politics and security, namely German-Russian relations and the Ukraine war.","desc_named_entities":"Users repeatedly mention Russia in a positive manner while expressing negative views toward NATO.","desc_patterns":"Members frequently share the Russian flag 🇷🇺 and German flag 🇩🇪 emojis.","desc_tone_style":"The predominant tone is empathetic and pro-Russian with positive valence, no incivility, and a moderately elaborate style.","desc_striking":"A striking feature is the symbolic use of flag emojis to express German-Russian solidarity.","lang":"de","topic":["politics","security"],"named_entities":[{"entity":"Russia","sentiment":"positive"},{"entity":"NATO","sentiment":"negative"}],"repetitive_patterns":["🇷🇺","🇩🇪"],"incivility":"no","elaborate":"yes"},{"slice":3,"share":5.0,"label":"General German Anti-War Voices","desc_lang_topics":"The German-speaking community posts about politics and society, namely anti-war sentiment.","desc_named_entities":"No focal entities are repeatedly referenced across the community.","desc_patterns":"Members frequently share the peace sign ✌️.","desc_tone_style":"The predominant tone is neutral and hopeful with positive valence, no incivility, and a simple style.","desc_striking":"No striking features are observable.","lang":"de","topic":["politics","society"],"named_entities":[],"repetitive_patterns":["✌️"],"incivility":"no","elaborate":"no"}]'
)

# Example answers
example_comm_slices_answer <- c(

  # --- Example 1: UK COP26 spam community ---
  '{
    "label": "UK COP26-Targeting Political Link Spam Community",
    "desc_lang_topics": "The English-speaking community based in the UK posts about politics and society, namely UK politicians and COP26.",
    "desc_named_entities": "Users repeatedly mention Boris Johnson in a neutral manner, with some slices also referencing other UK political figures and institutions neutrally.",
    "desc_patterns": "Members frequently share @BorisJohnson tags, @COP26 tags, and t.co/ links across slices.",
    "desc_tone_style": "The predominant tone is neutral and formulaic with neutral emotional valence, no incivility, and a simple style.",
    "desc_striking": "A striking feature is near-identical coordinated spam behavior across the two dominant slices (combined share ~21%): all users share the same tag-and-link template with minimal original content, suggesting automated or organized activity; the small critical slice (0.1%) exhibits a markedly different, accusatory tone but has negligible weight.",
    "lang": "en",
    "topic": ["politics", "society"],
    "named_entities": [
      {"entity": "Boris Johnson", "sentiment": "neutral"},
      {"entity": "COP26", "sentiment": "neutral"}
    ],
    "repetitive_patterns": ["@BorisJohnson", "@COP26", "t.co/"],
    "incivility": "no",
    "elaborate": "no",
    "confidence": 0.88
  }',

  # --- Example 2: German pro-Wagenknecht / pro-Russia peace community ---
  '{
    "label": "German Pro-Wagenknecht Anti-NATO Peace Advocates",
    "desc_lang_topics": "The German-speaking community based in Germany posts about politics and security, namely the Ukraine conflict, peace diplomacy, and German-Russian relations.",
    "desc_named_entities": "Users repeatedly mention Sarah Wagenknecht in a positive manner and NATO in a negative manner, with the second-largest slice also referencing Russia positively.",
    "desc_patterns": "Members frequently share the German flag 🇩🇪 and peace sign ✌️ emojis across slices.",
    "desc_tone_style": "The predominant tone is emotional, pacifist, and empathetic with positive valence, no incivility, and an elaborate style.",
    "desc_striking": "A striking feature is strong ideological homogeneity across the two dominant slices (combined share 95%): both the pro-Wagenknecht and the pro-Russian subgroups frame German-Russian solidarity and diplomacy as alternatives to military escalation, using overlapping emoji symbolism as political identity markers.",
    "lang": "de",
    "topic": ["politics", "security"],
    "named_entities": [
      {"entity": "Sarah Wagenknecht", "sentiment": "positive"},
      {"entity": "NATO", "sentiment": "negative"},
      {"entity": "Russia", "sentiment": "positive"}
    ],
    "repetitive_patterns": ["🇩🇪", "✌️"],
    "incivility": "no",
    "elaborate": "yes",
    "confidence": 0.92
  }'
)



###########################################################################
#
# Schema - Communities ----------------------------------------------------
#
##########################################################################


schema_comm <- list(
  type = "object",
  properties = list(

    label = list(
      type = "string",
      description = "[Community-Label] Specific Title-Cased noun phrase (4–8 words) capturing the community's most distinctive characteristics: location/language, named entities, stance, subtopics, or behaviors. (e.g., 'UK Farage Supporters Opposing EU Immigration')"
    ),

    desc_lang_topics = list(
      type = "string",
      description = "COMPLETE SENTENCE. Start with 'The community predominantly speaks …' or 'The [language]-speaking community …'. State predominant language/location and up to 3 topics (politics, society, culture, economy, security, migration, climate, science, other) with subtopics if salient (', namely …')."
    ),

    desc_named_entities = list(
      type = "string",
      description = "COMPLETE SENTENCE. Start with 'Users repeatedly mention …' naming up to 3 persons/organizations/countries from ≥2 users with prevailing sentiment (positive/neutral/negative), OR write the exact phrase 'No focal entities are repeatedly referenced across the community.'"
    ),

    desc_patterns = list(
      type = "string",
      description = "COMPLETE SENTENCE. Start with 'Members frequently share …' listing up to 3 patterns (emojis, hashtags, tags, phrases, URLs) from ≥2 users, OR write the exact phrase 'No shared repetitive patterns are present.'"
    ),

    desc_tone_style = list(
      type = "string",
      description = "COMPLETE SENTENCE. Start with 'The predominant tone is …'. Characterize tone and style across ALL users using descriptive labels, integrating emotional valence, majority incivility, and majority elaborateness."
    ),

    desc_striking = list(
      type = "string",
      description = "COMPLETE SENTENCE. Start with 'A striking feature is …' describing a noteworthy community-level observation not covered by the other desc_ fields, OR write the exact phrase 'No striking features are observable.'."
    ),

    lang = list(
      type = "string",
      description = "Predominant language of users in the community (ISO code, e.g., 'en', 'de')."
    ),

    topic = list(
      type = "array",
      description = "Up to 5 most salient recurring topics.",
      items = list(
        type = "string",
        enum = c("politics", "security", "economy", "society", "science",
                 "culture", "migration", "climate", "other")
      ),
      uniqueItems = TRUE,
      minItems = 0,
      maxItems = 5
    ),

    named_entities = list(
      type = "array",
      description = "Up to 3 key persons, organizations, or countries appearing in ≥2 users' named_entities fields, with prevailing sentiment.",
      items = list(
        type = "object",
        properties = list(
          entity    = list(type = "string"),
          sentiment = list(type = "string", enum = c("positive", "negative", "neutral"))
        ),
        required = c("entity", "sentiment")
      ),
      minItems = 0,
      maxItems = 3
    ),

    repetitive_patterns = list(
      type = "array",
      description = "Recurring emojis, slogans, hashtags, or stylistic markers appearing in ≥2 users' repetitive_patterns fields.",
      items = list(type = "string"),
      minItems = 0,
      maxItems = 3
    ),

    incivility = list(
      type = "string",
      enum = c("yes", "no"),
      description = "'yes' if the majority of users/slices (>50%) are annotated with incivility; else 'no'."
    ),

    elaborate = list(
      type = "string",
      enum = c("yes", "no"),
      description = "'yes' if the majority of users/slices (>50%) are annotated as elaborate; else 'no'."
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
    "desc_lang_topics",
    "desc_named_entities",
    "desc_patterns",
    "desc_tone_style",
    "desc_striking",
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

### Save them (external: user-facing via coorsim::prompts etc.)
usethis::use_data(
  prompts, examples, schemata, 
  internal = FALSE, overwrite = TRUE, ascii = FALSE
)

### Save also as internal data (loads into namespace; accessible inside package functions)
usethis::use_data(
  prompts, examples, schemata,
  internal = TRUE, overwrite = TRUE, ascii = FALSE
)

devtools::document()


