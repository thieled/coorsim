
<!-- README.md is generated from README.Rmd. Please edit that file -->

# coorsim <img src="man/figures/logo.png" align="right" height="134" alt="" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/thieled/coorsim/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/thieled/coorsim/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/thieled/coorsim/graph/badge.svg)](https://app.codecov.io/gh/thieled/coorsim)
<!-- badges: end -->

The coorsim R package is designed to detect and analyze coordinated
social media manipulation (CSMM), allowing researchers to identify
suspiciously similar patterns of social media behavior that may indicate
coordinated efforts to spread content. By leveraging embeddings, coorsim
detects similarities in posts that share themes and semantics, even if
they use diverse vocabulary or languages. This approach is particularly
relevant for identifying manipulation with AI-generated content.

## Installation

You can install the development version of coorsim from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("thieled/coorsim")
```

## Example

Below is an example of how to use coorsim to detect and analyze
coordinated behavior in a set of Twitter data.

### Step 1: Create Toy Data

First, we create a toy dataset: 100 Tweets by 10 users. 5 out of the 10
users engage in ‘coordinated posting behavior’ and spread climate
denialist content. The others also talk about other climate issues but
are non-coordinated.

<details>

<summary>

Click to expand toy data creation
</summary>

``` r

library(data.table)

set.seed(123)

# Define time parameters
start_time <- as.POSIXct("2024-01-15 09:00:00", tz = "UTC")
end_time <- as.POSIXct("2024-01-15 12:00:00", tz = "UTC")  # Explicitly set end time

# Create 10 accounts (5 coordinated, 5 normal)
coordinated_accounts <- paste0("coord_", 1:5)
normal_accounts <- paste0("normal_", 1:5)
all_accounts <- c(coordinated_accounts, normal_accounts)

# Create users_df
users_df <- data.frame(
  account_id = all_accounts,
  account_name = c(
    "ClimateSkeptic1", "TruthSeeker99", "ScienceDebunker", 
    "FreedomFirst", "RealFacts2024",
    "EcoWarrior", "GreenFuture", "ClimateActionNow", 
    "SustainableLiving", "PlanetProtector"
  ),
  followers = sample(100:10000, 10),
  verified = sample(c(TRUE, FALSE), 10, replace = TRUE)
)

# Climate denial messages (coordinated behavior)
denial_templates <- list(
  list("Climate change is a hoax perpetrated by global elites",
       "The so-called climate crisis is manufactured by those in power",
       "Climate change claims are exaggerated by the establishment"),
  
  list("No real evidence supports human-caused warming",
       "There's insufficient proof that humans cause climate change",
       "Scientific evidence for anthropogenic warming is lacking"),
  
  list("Natural cycles explain all temperature variations we see",
       "Earth's climate has always changed through natural processes",
       "Temperature fluctuations are part of natural climate cycles"),
  
  list("CO2 is plant food, not a pollutant harming our planet",
       "Carbon dioxide benefits plants and isn't dangerous",
       "Higher CO2 levels are good for vegetation growth")
)

# Other climate topics (normal behavior)
other_topics <- c(
  "Just installed solar panels on my roof! #renewableenergy",
  "New study shows Arctic ice melting faster than predicted",
  "Electric vehicles are becoming more affordable each year",
  "Extreme weather events are increasing in frequency globally",
  "Local community starting a tree planting initiative",
  "Ocean acidification threatens marine ecosystems",
  "Wind energy now cheaper than fossil fuels in many regions",
  "Heat waves breaking records across multiple continents",
  "Sustainable agriculture practices can reduce emissions",
  "Youth climate activists organizing global strike",
  "Coral reefs dying at alarming rates due to warming",
  "Green technology investments reaching new highs",
  "Wildfires devastating forests due to drought conditions",
  "Carbon capture technology showing promising results",
  "Cities implementing bike-sharing programs",
  "Glaciers retreating at unprecedented rates",
  "Plant-based diets can reduce carbon footprint",
  "Sea levels rising faster in coastal areas",
  "Battery technology improving for energy storage",
  "Species extinction linked to habitat loss"
)

# Generate coordinated tweets (30 tweets)
coordinated_tweets <- list()
tweet_id <- 1

# Calculate time window in seconds
time_window <- as.numeric(difftime(end_time, start_time, units = "secs"))

# Create 6 coordinated events (each with 5 tweets within 10 minutes)
for (event in 1:6) {
  # Random time within the 3-hour window (leaving 10 minutes for the event)
  event_start <- start_time + runif(1, 0, time_window - 600)
  
  # Pick a random denial template
  template <- denial_templates[[sample(1:4, 1)]]
  
  for (i in 1:5) {
    coordinated_tweets[[length(coordinated_tweets) + 1]] <- data.frame(
      post_id = sprintf("tweet_%03d", tweet_id),
      account_id = coordinated_accounts[i],
      content = template[[sample(1:3, 1)]],
      created_at = event_start + runif(1, 0, 600)  # Within 10 minutes
    )
    tweet_id <- tweet_id + 1
  }
}

# Generate normal tweets (70 tweets)
normal_tweets <- list()

for (i in 1:70) {
  normal_tweets[[i]] <- data.frame(
    post_id = sprintf("tweet_%03d", tweet_id),
    account_id = sample(all_accounts, 1),
    content = sample(other_topics, 1),
    created_at = start_time + runif(1, 0, time_window)
  )
  tweet_id <- tweet_id + 1
}

# Combine all tweets
tweets_df <- rbind(
  do.call(rbind, coordinated_tweets),
  do.call(rbind, normal_tweets)
)

# Sort by time
tweets_df <- tweets_df[order(tweets_df$created_at), ]
rownames(tweets_df) <- NULL

# Convert to data.table
tweets_df <- as.data.table(tweets_df)
users_df <- as.data.table(users_df)
```

</details>

### Step 2: Get embeddings

Next, we retrieve document embeddigs from transformer models. The
function automatically installs and sets up a conda environment with the
necessary libraries, if not yet present.

``` r
# Install/initialize conda environment to run embedding model
coorsim::initialize_coorsim()

# Create dir to store embeddings
if(!dir.exists("data/emb")) dir.create("data/emb", recursive = T)


# For larger datasets this works best with a CUDA GPU
emb_matrix <- coorsim::save_embeddings(tweets_df,
                                  post_id = "post_id", 
                                  time = "created_at",
                                  content = "content",
                                  batch_size = 16L, 
                                  max_length = 512L, 
                                  use_fp16 = T,
                                  model_name = "Twitter/twhin-bert-base", # "sentence-transformers/paraphrase-TinyBERT-L6-v2", 
                                  save_dir = "data/emb", 
                                  h5_fileprefix = "toy_sample_"
                                )
```

### Step 3: Detect Co-Similar Posts

Run co-similarity detection on posts within a 180-second timeframe and a
cosine similarity threshold of 0.925.

``` r
# Get embedding file path
emb_file <- list.files(path = "data/emb", pattern = ".h5$", full.names = T)[1]

# Detect Posting similarites
sim_dt <- coorsim::detect_cosimilarity(
  data = tweets_df,
  embeddings = emb_file,
  time_window = 180, # 3 Minutes 
  min_simil = 0.925, 
  min_participation = 1,
  post_id = "post_id",
  account_id = "account_id",
  time = "created_at",
  content = "content",
  verbose = TRUE
)
#> ℹ [1/4]: Preprocessing.Embeddings provided by .h5 file.✔ [1/4]: Preprocessing. [3ms]
#> ℹ [2/4]: Matching posts published within 180s.✔ [2/4]: Matched posts published within 180s. [14ms]
#> Loading embeddings from the .h5 file.ℹ [3/4]: Querying embeddings and calculate similarities using C++.✔ [3/4]: Queried embeddings, calculated similarities using C++. [16ms]
#> ℹ [4/4]: Filter accounts by min_participation=1✔ [4/4]: Filtered accounts by min_participation=1 [17ms]

# Clean up the embeddings directory
if(dir.exists("data/emb")) unlink("data/emb", recursive = TRUE)
```

### Step 4: Detect Communities

Next we aggregate the observations of coordinated posting behavior on
account level. We create a network, where the nodes are accounts and the
edge weight is defined by the number of coordinated posting events.
Finally, we identify communities of accounts using ‘louvain’ clustering:

``` r
# Detect groups of accounts
coord <- coorsim::coorsim_detect_groups(
  simdt = sim_dt,
  user_data = users_df,
  account_id = "account_id",
  verbose = TRUE
)
#> ℹ [1/5]: Harmonizing user data.De-duplicating 'user_data'...✔ [1/5]: Harmonized user data. [9ms]
#> ℹ [2/5]: Create edge list.✔ [2/5]: Created edge list. [9ms]
#> ℹ [3/5]: Create node list and graph.✔ [3/5]: Created node list and graph. [38ms]
#> ℹ [4/5]: Finding communities.✔ [4/5]: Finding communities. [8ms]
#> ℹ [5/5]: Merge and prepare output data.✔ [5/5]: Prepared output data. [6ms]   
```

### Step 5: Plot Network

The package provides also a function to plot communities:

``` r
p1 <- coorsim::plot_communities(coord)
p1
#> Warning: annotation$theme is not a valid theme.
#> Please use `theme()` to construct themes.
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

### Step 5: Sample Texts for Labeling Users and Communities

Sample post content and metadata to generate concise community labels.

``` r

# Sampe users and posts
coord <- coorsim::sample_user_text( 
  groups_data = coord, 
  sampling_ratio_posts = .5, 
  sampling_ratio_users = .5) ## use all users 
```

### Step 6: Label Communities

Use a language model to generate labels for each identified community

``` r
# Generate user descriptions, using llama3.1:8b
coord <- coorsim::label_users(coord)
#> ▶ Ollama (v0.12.6) is running at <http://localhost:11434>!
#> Retry round 0. Querying 5 users...
#> ⠙ llama3.1:8b is thinking about 5/5 questions[ETA: ?]
#> ⠹ llama3.1:8b is thinking about 5/5 questions[ETA: ?]
#> ⠸ llama3.1:8b is thinking about 4/5 questions[ETA:  7m]
#> ⠼ llama3.1:8b is thinking about 3/5 questions[ETA:  4m]
#> ⠴ llama3.1:8b is thinking about 2/5 questions[ETA:  2m]
#> ⠦ llama3.1:8b is thinking about 1/5 questions[ETA:  1m]
#>                                                        
#> All answers parsed successfully.

# Generate community labels, using llama3.1:8b 
coord <- coorsim::label_communities(coord, model = "llama3.1:8b")
#> Returning 2 community texts to annotate.
#> ▶ Ollama (v0.12.6) is running at <http://localhost:11434>!
#> Retry round 0. Querying 2 communities...
#> ⠙ llama3.1:8b is thinking about 2/2 questions[ETA: ?]
#> ⠹ llama3.1:8b is thinking about 1/2 questions[ETA:  2m]
#>                                                        
#> All answers parsed successfully.
```

### Step 6: Visualize Community Network

``` r
# Plot Communities
p2 <- coorsim::plot_communities(coord)
p2
#> Warning: annotation$theme is not a valid theme.
#> Please use `theme()` to construct themes.
```

<img src="man/figures/README-plots-1.png" width="100%" />
