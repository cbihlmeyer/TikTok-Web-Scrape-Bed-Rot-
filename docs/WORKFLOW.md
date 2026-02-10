# Bed Rot TikTok Web Scrape Workflow

## Purpose
To conceptualize the term “bed rot,” quantitative content analysis is used to supplement qualitative elicitation methods (e.g., survey and focus groups).  

---

## High-Level Steps
- Zeeschuimer Web Scrape, #bedrot  
- Extract URL  
- Generate Whisper Transcripts  
- Derive Supplemental Text Content Areas (Description, On-screen Text)  
- Clean All Text Areas  
- Human Verification and Further Probing

---

## Step 1 — Zeeschuimer Web Scrape

**1a. Install extension**  
Download the Zeeschuimer extension for Firefox: https://github.com/digitalmethodsinitiative/zeeschuimer/releases  
See the Zeeschuimer Worksheet for detailed instructions: https://docs.google.com/document/d/19MAiqX7Vx1FcNJ44K-vSdnKDVC5gcFwtrSQiewnwW8A/edit  
*Note:* Zeeschuimer does not work in private browsing.

**1b. Design a search query**  
Manually navigate to a relevant TikTok page; everything the user sees will be recoded as metadata.  
Log in and use TikTok’s search feature to search for **“bed rot”** videos.  
You can also view the hashtag page without logging in: https://www.tiktok.com/tag/bedrot?lang=en

**1c. Turn on the extension**  
Toggle the Zeeschuimer extension **on**.

**1d. Refresh and collect**  
Refresh the page after toggling to clear any cached data.  
Scroll until you have captured the desired amount of data (e.g., ~50 TikTok videos).

**1e. Export**  
Download the collected data as an **.ndjson** file.

---

## Step 2 — Extract URL

**2a. Create permalinks in R**  
Use the raw Zeeschuimer metadata to derive a permalink for each video.

**2b. Build a CSV with columns**  
1) `author_id` — TikTok’s internal, stable account identifier  
2) `author_uniqueid` — author username/handle  
3) `item_id` — unique ID for each video  
4) `permalink` — canonical TikTok URL:  
`https://www.tiktok.com/@{handle}/video/{item_id}`

> 
```{r setup, include=FALSE, warning=FALSE, message=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, warning=FALSE, message=FALSE}
# ---- Load in  packages ----
library(ndjson)
library(dplyr)
library(purrr)
library(tidyr)
```

```{r}
# ---- Read NDJSON into a list of R lists ----
# stream_in() loads NDJSON as a list of nested R objects
raw <- ndjson::stream_in("bed-rot-zeeschuimer-export-tiktok.ndjson")
```

```{r}
# Step 1: Build the 4-column table in memory (no file writing yet)
stopifnot(all(c("data.author.id", "data.author.uniqueId", "item_id") %in% names(raw)))

# stopifnot(...): If the condition is not TRUE, it stops with an error.
# c("...", "...", "..."): Creates a character vector of the required column names.
# names(raw): Returns all column names of your raw object (a data.table/data.frame).
# all(...): Collapses that logical vector to a single TRUE only if all are TRUE.
```

```{r}
# Step 2: Build the minimal table
tiktok_min <- data.frame( # creating a df of author.id, author.uniqueId, and item_id
  author_id       = as.character(raw[["data.author.id"]]), 
  # Extracts the column named "data.author.id" as a vector.
  # Ensuring values are character strings (as.character)
  author_uniqueid = as.character(raw[["data.author.uniqueId"]]),
  item_id         = as.character(raw[["item_id"]]),
  stringsAsFactors = FALSE
)
```

```{r}
# Step 3: Compute permalink
tiktok_min$permalink <- paste0( # Concatenates strings without separators
  "https://www.tiktok.com/@",
  tiktok_min$author_uniqueid,
  "/video/",
  tiktok_min$item_id
)
```

```{r}
# Quick peek for verification
utils::head(tiktok_min, 10)
```

```{r}
# Step 4: Write the four columns to a CSV (UTF-8, no row names)
tiktok_min4 <- tiktok_min[, c("author_id","author_uniqueid","item_id","permalink"), drop = FALSE]

# Ensure all columns are character (avoids numeric/scientific notation issues)
for (nm in names(tiktok_min4)) tiktok_min4[[nm]] <- as.character(tiktok_min4[[nm]])

out_path <- "tiktok_bedrot_urls.csv"
utils::write.csv(tiktok_min4, file = out_path, row.names = FALSE, fileEncoding = "UTF-8", na = "")
```

---

## Step 3 — Generate Whisper Transcripts (Python)

**3a. Transcribe from URLs**  
Use the `permalink` values to generate transcripts with the Whisper model.


```python
!apt-get -y -qq install ffmpeg >/dev/null
!pip -q install yt-dlp faster-whisper tqdm pandas

import os, glob, subprocess, shlex
import pandas as pd
from tqdm import tqdm
from faster_whisper import WhisperModel

INPUT_CSV  = "/content/tiktok_bedrot_urls.csv"   # csv with tiktok urls, change if different
OUTPUT_CSV = "tiktok_bedrot_transcripts.csv"
AUDIOD  = "audio"

os.makedirs(AUDIOD, exist_ok=True)

df = pd.read_csv(INPUT_CSV, dtype=str)
if "verbatimtranscript" not in df.columns: df["verbatimtranscript"] = ""
if "transcriptionstatus" not in df.columns: df["transcriptionstatus"] = ""

model = WhisperModel("medium", device="cpu", compute_type="int8")  # simple defaults

def run(cmd):
    return subprocess.run(cmd if isinstance(cmd, list) else shlex.split(cmd),
                          stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, check=False)

def download_audio(page_url, out_prefix):
    # Download directly from the item page URL; yt-dlp resolves it for you.
    # Keep it simple: extract best audio to mp3.
    tmpl = f"{out_prefix}.%(ext)s"
    cmd = ["yt-dlp", "-x", "--audio-format", "mp3", "-o", tmpl, page_url]
    run(cmd)
    # return created file (mp3/m4a/etc.); we expect mp3
    for f in glob.glob(f"{out_prefix}.*"):
        if os.path.isfile(f) and os.path.getsize(f) > 0:
            return f
    return ""

def transcribe(path):
    segments, info = model.transcribe(path, beam_size=5, word_timestamps=False, vad_filter=True)
    return " ".join(s.text.strip() for s in segments if getattr(s, "text", "").strip())

for i, row in tqdm(df.iterrows(), total=len(df)):
    url = (row.get("permalink") or "").strip() # "permalink" is the column name holding URLs; change if needed
    if not url:
        df.at[i, "transcriptionstatus"] = "missingurl"
        continue
    outp = os.path.join(AUDIOD, f"audio_{i:06d}")
    audio = download_audio(url, outp)
    if not audio:
        df.at[i, "transcriptionstatus"] = "download_failed"
        continue
    try:
        txt = (transcribe(audio) or "").strip()
        if txt:
            df.at[i, "verbatimtranscript"] = txt
            df.at[i, "transcriptionstatus"] = "success"
        else:
            df.at[i, "transcriptionstatus"] = "emptyaudio"
    except Exception as e:
        df.at[i, "transcriptionstatus"] = f"failed:{type(e).__name__}"
    finally:
        # cleanup audio file(s)
        for f in glob.glob(f"{outp}.*"):
            try: os.remove(f)
            except OSError: pass

df.to_csv(OUTPUT_CSV, index=False)
print(f"✅ Transcription saved to {OUTPUT_CSV}")
```

---

## Step 4 — Derive Supplemental Text Content Areas

**4a. From raw TikTok metadata, derive:**
1) **Description** — caption text including hashtags and mentions  
2) **On‑screen text** — creator-added “stickers” overlayed on the video

(Proceed to Step 5 for the code that performs this.)

---

## Step 5 — Clean All Text Content Areas (R)

**5a. Scope**  
Clean transcripts, description, and on‑screen text.

**5b. Cleaning tasks**  
- Remove literal `//n` token if present  
- Convert literal backslash‑n (`\n`) to a single space  
- Flatten strings that look like R’s `c("…", "…")` printout into plain text  
- Drop leading `c(` or `c("` (with optional spaces)  
- Drop trailing `)`  
- Remove all double quotes (ASCII and curly)  
- Replace actual CR/LF newlines with a single space  
- Normalize apostrophes to plain ASCII to avoid CSV mojibake  
- Drop zero‑width characters (ZWSP, ZWJ, ZWNJ, BOM)  
- Replace raw URLs with placeholder `<URL>`  
- Trim and collapse whitespace  
- Unicode normalize (NFC)  
  - e.g., U+006E `n` followed by U+0303 `~` → `ñ`  
- (Optional) Lowercase for regex/search (Unicode‑safe)  
- HTML unescape (convert entities like `&amp;` to `&`)  
- Isolate emojis from description  
- Remove mentions (@) from description

 
```{r setup, include=FALSE, message=FALSE, warning=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r message=FALSE, warning=FALSE}
# === CHUNK 1: Libs & Config ===
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(readr)
library(readxl)

ZEESCHUIMER_PATH <- "bed-rot-zeeschuimer-export-tiktok.ndjson"  # or .jsonl
TRANSCRIPTS <- read_csv("tiktok_bedrot_transcripts.csv")
```

```{r}
# === CHUNK 2.1: Load & Flatten ===
# "jsonlite::stream_in" handles NDJSON/JSONL line-by-line when using file() with pagesize
##  by using a connection + stream_in(), you’re set up to parse .ndjson / .jsonl cleanly and efficiently, and you can control batch size with pagesize if needed. Safer for big files.
con <- file(ZEESCHUIMER_PATH, open = "r") # Opens a read-only connection to NDJSON/JSONL file.
on.exit(close(con)) # Deferred close the connection
raw_list <- stream_in(con, verbose = FALSE) # Each line becomes one record; bound row-wise
```

```{r}
# === CHUNK 2.2: Load & Flatten ===
meta <- as_tibble(raw_list)
```
```{r}
# === CHUNK 2.3: Load & Flatten ===
## Step: Widen the packed data column (only). 
## Turns inner columns of "data" into top-level columns with the prefix "data"

# Already have: meta <- as_tibble(raw_list) from CHUNK 2.2
# Safety check: confirm `data` is a data.frame column
stopifnot(is.data.frame(meta$data))

# Unnest just the first level: expand inner columns to top-level with a prefix
meta <- meta %>%
  unnest_wider(data, names_sep = ".")

# Quick peek to verify outcome
names(meta)[1:40]        # skim first ~40 names
str(meta, max.level = 1) # confirm list-cols (author, video, stats, etc.) still present but top-level

```
```{r}
# === CHUNK 2.3, con't: Load & Flatten ===
## Step: Next-level extraction
## Unnest: item_id, createTime, desc_raw
## Keep all the list‑columns (data.author, data.stats, data.textExtra, etc.) untouched for the following steps.

# Surface the most-used scalar fields; keep nested list-cols as-is for now
meta <- meta %>%
  mutate(
    item_id        = dplyr::coalesce(.data[["item_id"]], .data[["data.id"]]),
    createTime     = .data[["data.createTime"]],
    desc_raw       = .data[["data.desc"]],
    )
```
```{r}
# === CHUNK 2.3, con't: Load & Flatten ===
## Step: Next-level extraction
## Un‑nest data.author to expose author identifiers: author_id and author_handle

# Safety check: the column exists and is list-like
stopifnot("data.author" %in% names(meta))

# Controlled widen of just the `author` sub-object
meta <- meta %>%
  unnest_wider(col = data.author, names_sep = "." , names_repair = "unique")
# Resulting columns should include: data.author.id, data.author.uniqueId, etc.

# Safety check, all variables with prefix "data.author"
grep("^data\\.author\\.", names(meta), value = TRUE)

# Surface feature-map fields author_id and author_handle
meta <- meta %>%
  mutate(
    author_id      = .data[["data.author.id"]],
    author_handle  = .data[["data.author.uniqueId"]]
  )

```
```{r}
# === CHUNK 3.1, Join Transcripts to raw data ("meta") ===

##### Creating/Extracting item_id from TRANSCRIPTS ##### 
## TRANSCRIPTS will not have a clean item_id if formatted in Excel
## Excel mangles numbers which are too long (16+ nums)
## Extract the exact item_id as TEXT from the permalink
TRANSCRIPTS <- TRANSCRIPTS %>%
  mutate(
    item_id_str = stringr::str_match(permalink, "/video/(\\d+)")[, 2]
  )

# Quick verification (should show the 19-digit IDs as character, not scientific notation)
list(
  n_rows = nrow(TRANSCRIPTS),
  non_na_item_id_str = sum(!is.na(TRANSCRIPTS$item_id_str)),
  sample = head(TRANSCRIPTS$item_id_str, 5)
)

##### JOIN #####
# Ensure both keys are TEXT
meta <- meta %>% mutate(item_id = as.character(item_id))
TRANSCRIPTS <- TRANSCRIPTS %>% mutate(item_id_str = as.character(item_id_str))

# Enforce 1:1 keys (abort if any duplicates would cause a many-to-many)
stopifnot(all(dplyr::count(meta, item_id)$n == 1))
stopifnot(all(dplyr::count(TRANSCRIPTS, item_id_str)$n == 1))

# Preserve original meta order, then LEFT JOIN by item_id
meta$..row_id <- seq_len(nrow(meta))
meta_joined <- meta %>%  
  left_join(TRANSCRIPTS %>% select(item_id_str, verbatimtranscript),
            by = c("item_id" = "item_id_str")) %>%  
  arrange(..row_id) %>%  
  select(-..row_id)

# Add transcript column
meta_joined <- meta_joined %>%  
  mutate(transcript_text = verbatimtranscript)  # keep both if you want


# Confirmation
cat("Rows unchanged: ", nrow(meta_joined) == nrow(meta), "\n", sep = "")
cat("Attached transcripts: ", sum(!is.na(meta_joined$transcript_text)), " of ", 
    nrow(meta_joined), "\n", sep = "")

# (Optional view) show first matched rows only
meta_joined %>%  
  filter(!is.na(transcript_text)) %>%  
  select(item_id, transcript_text) %>%  
  head(5)

```

```{r}
# === CHUNK 3.2, Join Transcripts to raw data ("meta") ===

############# IDENTIIFY DUPLICATED VIDEOS BY ITEM_ID #############

# How many total rows vs. distinct item_id?
data.frame(
  total_rows    = nrow(meta_joined),
  distinct_ids  = dplyr::n_distinct(meta_joined$item_id)
)

# Which item_id values are duplicated, and how many times?
dup_ids <- meta_joined %>%
  count(item_id, name = "n") %>%
  filter(n > 1) %>%
  arrange(desc(n))

# Create DF with duplicated IDs
dup_ids  # inspect this
# (Optional) peek at the actual duplicated rows 
## (only works if there are dupes - move on if error/no dupes)
meta_joined %>%
  semi_join(dup_ids, by = "item_id") %>%
  select(item_id, video_url, verbatimtranscript, transcript_text) %>%
  arrange(item_id) %>%
  head(20)

############# DROP DUPLICATE ITEM_ID #############

# Keep the first occurrence of each item_id
meta_dedup <- meta_joined %>%
  distinct(item_id, .keep_all = TRUE)

# Quick confirmation
data.frame(
  rows_before = nrow(meta_joined),
  rows_after  = nrow(meta_dedup),
  distinct_ids = dplyr::n_distinct(meta_dedup$item_id)
)
```

```{r}
# === CHUNK 4.1, Capture On-Screen Text  ===

# Helper: safely coerce a stickers object to a data frame and extract candidate text fields
extract_sticker_texts <- function(x) {
  # Normalize empty/NA cases
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) {
    return(character(0))
  }
  # Try to coerce to a data frame of stickers
  df <- tryCatch(
    {
      # If x is already a data.frame / tibble OR a list of records, bind_rows handles both
      dplyr::bind_rows(x)
    },
    error = function(e) {
      # If coercion fails, return empty char vector
      tibble::tibble()
    }
  )

  if (nrow(df) == 0) return(character(0))

  # Candidate fields: primary 'stickerText', fallback 'text'
  vals <- character(0)
  if ("stickerText" %in% names(df)) vals <- c(vals, df$stickerText)
  if ("text"        %in% names(df)) vals <- c(vals, df$text)

  # Clean minimal: keep as-is (no normalization yet), just trim and drop empties
  vals <- vals[!is.na(vals)]
  vals <- as.character(vals)
  vals <- str_trim(vals)
  vals <- vals[nzchar(vals)]

  vals
}

# ---- Perform the extraction on meta_dedup (row order preserved) ----
meta_dedup <- meta_dedup %>%
  mutate(
    .stickers_raw   = `data.stickersOnItem`,
    .texts_list     = map(.stickers_raw, extract_sticker_texts),
    on_screen_text_present = map_lgl(.texts_list, ~ length(.x) > 0),
    on_screen_text_text    = map_chr(.texts_list, ~ if (length(.x) == 0) NA_character_ else paste(.x, collapse = "\n"))
  ) %>%
  select(-.stickers_raw, -.texts_list)

# ---- Minimal preview ----

# 1) Frequency table of on_screen_text_present
table(meta_dedup$on_screen_text_present)

# 2) First 5 rows where TRUE with item_id + on_screen_text_text
meta_dedup %>%
  filter(on_screen_text_present) %>%
  select(item_id, on_screen_text_text) %>%
  head(5)

```
```{r}
# === CHUNK 4.2, Clean On-Screen Text ===
## (1) remove the literal //n characters
## (2) flatten any strings that look like R’s c("…", "…") vector printout into plain text (joined with a single space)


meta_dedup <- meta_dedup %>%
  mutate(
    on_screen_text_text = case_when(
      is.na(on_screen_text_text) ~ NA_character_,
      TRUE ~ on_screen_text_text %>%
        str_replace_all(fixed("//n"), "") %>%   # remove the literal //n token if present
        str_replace_all('^c\\("', '') %>%     # drop leading c("
        str_replace_all("^\\s*c\\s*\\(\\s*", "") %>% # Drop leading 'c(' (with optional spaces)
        str_replace_all("\\s*\\)\\s*$", "") %>% # Drop trailing ')' (with optional spaces)
        str_replace_all(fixed("\\n"), " ") %>%  # remove literal backslash-n as a single space
        str_replace_all("[\"“”]", "") %>% # Remove all double quotes (ASCII and curly)
        str_replace_all("[\\r\\n]+", " ") %>%   # remove actual CR/LF newlines to a single space
        str_replace_all("\\\\", "") %>%
        # Normalize apostrophes to plain ASCII to avoid CSV mojibake
        str_replace_all("[\\u2018\\u2019]", "'")  %>%
        str_to_lower()%>% # optional: leave out for emotional/tone use of capitalization
        str_squish()
    )
  )


#Check
head(meta_dedup$on_screen_text_text)
```

```{r}
# === CHUNK 4.3, Clean Transcripts ===

# Add transcript_text_clean (leave transcript_text as-is for provenance)
stopifnot("transcript_text" %in% names(meta_dedup))

meta_dedup <- meta_dedup %>%
  mutate(
    # start from a character copy
    transcript_text_clean = as.character(transcript_text),
    # Unicode normalize (NFC)
    transcript_text_clean = stringi::stri_trans_nfc(transcript_text_clean),
    # HTML unescape: common entities
    transcript_text_clean = stringr::str_replace_all(
      transcript_text_clean,
      c("&amp;"="&", "&lt;"="<", "&gt;"=">", "&quot;"="\"", "&#39;"="'")
    ),
    # drop zero‑width characters (ZWSP, ZWJ, ZWNJ, BOM)
    transcript_text_clean = stringr::str_replace_all(
      transcript_text_clean, "[\\u200B-\\u200D\\uFEFF]", ""
    ),
    # replace raw URLs with placeholder
    transcript_text_clean = stringr::str_replace_all(
      transcript_text_clean, "https?://\\S+", " __URL__ "
    ),
    # trim and collapse whitespace
    transcript_text_clean = stringr::str_squish(transcript_text_clean),
   # lowercase for regex/search (Unicode‑safe)  
   # optional: leave out for emotional/tone use of capitalization
   transcript_text_clean = stringi::stri_trans_tolower(transcript_text_clean)  )

# quick confirmation (counts should match your 18 non-NA transcripts)
cat(
  "raw non-NA:",   sum(!is.na(meta_dedup$transcript_text)),
  " | clean non-NA:", sum(!is.na(meta_dedup$transcript_text_clean)), "\n"
)

#Check
head(meta_dedup$transcript_text_clean)

```
```{r}
# === CHUNK 4.4, Clean Description ===

stopifnot("desc_raw" %in% names(meta_dedup))

# Strict emoji pattern:
# - Keycaps (0-9, #, *) with optional VS16 + keycap mark
#  - Flags (two Regional Indicators)
#  - Emoji presentation / extended pictographs (pictorial)
emoji_pat <- paste0(  
  "(?:",    
  "(?:[\\#\\*0-9]\\uFE0F?\\u20E3)", #keycap emoji like 1️⃣ #️⃣ 
  "|(?:\\p{RI}{2})",                # flags 🇺🇸 etc.    
  "|\\p{Emoji_Presentation}",       # default-emoji characters    
  "|\\p{Extended_Pictographic}",    # pictographs that render as emoji  
  ")"
  )

meta_dedup <- meta_dedup %>%
  mutate( # Work from a character copy
    .desc = as.character(desc_raw),

    # ---- extract emojis to a separate column (space-separated string; NA if none) ----
    desc_emoji = stringi::stri_extract_all_regex(.desc, emoji_pat),
    desc_emoji = purrr::map_chr(
      desc_emoji,
      ~ if (length(.x) == 0 || all(is.na(.x))) NA_character_ else paste(.x, collapse = " ")
    ),
    # ---- build desc_clean (feature-map rules + emoji removal) ----
    desc_clean = .desc,
    # Unicode normalize (NFC)
    desc_clean = stringi::stri_trans_nfc(desc_clean),
    # HTML unescape (handle double-encoded & normal entities)
    desc_clean = stringr::str_replace_all(
      desc_clean,
      c("&amp;amp;"="&amp;", "&amp;lt;"="&lt;", "&amp;gt;"="&gt;", "&amp;quot;"="\"", "&amp;#39;"="'")
    ),
    desc_clean = stringr::str_replace_all(
      desc_clean,
     c("&amp;amp;amp;"="&amp;amp;", "&amp;amp;lt;"="&amp;lt;", "&amp;amp;gt;"="&amp;gt;", "&amp;amp;quot;"="\"", "&amp;amp;#39;"="'")   
     ),
    desc_clean = stringr::str_replace_all(      
      desc_clean,      
      c("&amp;amp;"="&amp;", "&amp;lt;"="&lt;", "&amp;gt;"="&gt;", "&amp;quot;"="\"", "&amp;#39;"="'")    
      ),
    # Remove emojis
    desc_clean = stringi::stri_replace_all_regex(desc_clean, emoji_pat, ""),
    # Remove zero‑width characters
    desc_clean = stringr::str_replace_all(desc_clean, "[\\u200B-\\u200D\\uFEFF]", ""),
    # Replace raw URLs with placeholder
    desc_clean = stringr::str_replace_all(desc_clean, "https?://\\S+", " __URL__ "),
     # Remove hashtags (avoid nuking HTML entities like &#39;)    
    desc_clean = stringr::str_replace_all(desc_clean, "(?<!&)#\\S+", " "),    
    # Remove mentions (symbol + following token)    
    desc_clean = stringr::str_replace_all(desc_clean, "@\\S+", " "),
    # Trim + collapse whitespace
    desc_clean = stringr::str_squish(desc_clean),
    # Lowercase (Unicode‑safe) for downstream regex/search
    desc_clean = stringi::stri_trans_tolower(desc_clean),
# Normalize curly quotes/apostrophes to plain ASCII to avoid CSV mojibake
desc_clean = stringr::str_replace_all(desc_clean, "[\\u2018\\u2019]", "'"),   # ‘ ’ -> '
desc_clean = stringr::str_replace_all(desc_clean, "[\\u201C\\u201D]", "\""), # “ ” -> "

    ) %>%
  select(-.desc)

# (optional) quick confirmation
cat(
  "desc_raw non-NA: ", sum(!is.na(meta_dedup$desc_raw)),
  " | desc_clean non-NA: ", sum(!is.na(meta_dedup$desc_clean)),
  " | with emojis found: ", sum(!is.na(meta_dedup$desc_emoji)),
  "\n", sep = ""
)

head(meta_dedup$desc_clean)

```

```{r}
# === CHUNK 5, Export Relevant Variables ===


# TRUE = list/packed; FALSE = flat/scalar (exportable)
is_list_col <- map_lgl(meta_dedup, is.list)

packed_cols     <- names(meta_dedup)[is_list_col]
exportable_cols <- names(meta_dedup)[!is_list_col]

# Quick peek
packed_cols
exportable_cols

# select only the columns of interest: item_id, desc_raw, author_id, author_handle, verbatimtranscript, transcript_text	on_screen_text_present, on_screen_text_text, transcript_text_clean, desc_emoji, desc_clean

meta_dedup_export <- meta_dedup %>%
  select(
    item_id,
    desc_raw,
    author_id,
    author_handle,
    verbatimtranscript,
    transcript_text,
    on_screen_text_present,
    on_screen_text_text,
    transcript_text_clean,
    desc_emoji,
    desc_clean
  )


writexl::write_xlsx(meta_dedup_export, "tiktok_bedrot_export_numX_date.xlsx")
```


---

## Step 6 — Human Verification and Further Probing

**6a. Purpose of the tool**  
This pipeline aggregates and surfaces unstructured text data from user‑generated content on the selected topic—quickly and at scale.

**6b. Content analytic methods**  
Design and apply content analysis to the cleaned text to detect the presence and contours of the phenomenon.

**6c. Validity checks (qualitative support)**  
- **Face Validity** — interpretive coherence within the concept; are units of analysis representative?  
  - *Practical step:* intercoder reliability with at least two coders.  
- **Semantic Validity** — do selected words/phrases share substantive meaning and differ from other concepts?  
  - *Practical step:* inspect cohesion and distinctiveness of the “bed rot” unit of analysis.  
- **Construct Validity** — does the text reflect the construct of interest?  
  - *Practical step:* triangulate with other studies and/or external sources (e.g., focus groups eliciting the “bed rot” concept).
