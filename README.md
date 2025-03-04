
<!-- README.md is generated from README.Rmd. Please edit that file -->

# coorsim <img src="man/figures/logo.png" align="right" height="134" alt="" />

<!-- badges: start -->
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
# install.packages("devtools")
devtools::install_github("thieled/coorsim")
```

If the ‘rhdf5’ library is causing error, please first run:

``` r
if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install("rhdf5")

devtools::install_github("thieled/coorsim")
```

## Example

Below is an example of how to use coorsim to detect and analyze
coordinated behavior in a set of Twitter data.

### Step 1: Load Data

Prepare data with tweets containing posts and users with user metadata.
Ensure a matrix of post embeddings is also available.

``` r
posts <- readRDS("/path/to/file")
users <- readRDS("/path/to/user_file")
```

### Step 2: Get embeddings (optional)

Next, we retrieve document embeddigs from transformer models. The
function automatically installs and sets up a conda environment with the
necessary libraries, if not yet present.

This step can also be skipped – In that case, the detect_cosimilarity()
function uses word-frequecies to compute similarites.

``` r
emb_matrix <- coorsim::get_embeddings(posts,
                                  post_id = "tweet_id", 
                                  time = "created_at",
                                  content = "text",
                                  batch_size = 16L, 
                                  max_length = 512L, 
                                  use_fp16 = T,
                                  model_name = "sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2")
```

### Step 2: Detect Co-Similar Posts

Run co-similarity detection on posts within a 60-second timeframe and a
cosine similarity threshold of 0.95.

``` r
sim_dt <- coorsim::detect_cosimilarity(
  data = posts,
  vector_matrix = post_embedding_matrix,
  time_window = 60,
  min_simil = 0.95,
  min_participation = 3,
  post_id = "tweet_id",
  account_id = "user_id",
  time = "created_at",
  content = "text",
  verbose = TRUE
)
```

### Step 3: Detect Communities

Identify communities of accounts using the FSA_V method to reveal groups
with coordinated posting behavior.

``` r
comm_dt <- coorsim::coorsim_detect_groups(
  simdt = sim_dt,
  user_data = users,
  cluster_method = "FSA_V",
  account_id = "user_id",
  theta = 0.7,
  verbose = TRUE
)
```

### Step 4: Prepare for Community Labeling

Sample post content and metadata to generate concise community labels.

``` r
comm_dt <- coorsim::prepare_community_texts(
  groups_data = comm_dt,
  sample_n = 5,
  min_n_char = 10,
  verbose = TRUE
)
```

### Step 5: Label Communities

Use a language model to generate labels for each identified community

``` r
instruction <- "Generate a concise label in English and a one-sentence description that summarizes the themes, tone, and regional focus of this community of Twitter users. The account names, locations, short bios, and sampled posts are provided below. Use '[LABEL:]' for the label and '[DESCRIPTION:]' for the description. Provide no additional output."

label_res <- coorsim::label_communities(
  groups_data = comm_dt,
  instruction = instruction,
  llm = "llama3.1:8b",
  retries = 3
)
```

### Step 6: Visualize Community Network

``` r
p1 <- coorsim::plot_communities(network_data = label_res, component_size_threshold = 3)
p2 <- coorsim::plot_coordinated_posts(network_data = label_res, by_community = TRUE)
```
