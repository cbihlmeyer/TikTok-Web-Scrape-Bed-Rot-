# TikTok Web Scrape Workflow: "Bed Rot" Example

Learn how to scrape TikTok for transcripts, on-screen text, and post descriptions. Example using the topic "bed rot"

*Purpose:* To conceptualize the term “bed rot,” quantitative content analysis is used to supplement qualitative elicitation methods, like survey and focus groups.


## Features

- TikTok collection via the Zeeschuimer Firefox extension producing `.ndjson` exports
- R pipeline to derive stable permalinks and build a clean URL CSV
- Python (Whisper) batch transcription from TikTok video permalinks
- Extraction of supplemental text: creator description (captions) and on‑screen text
- Unified text cleaning in R (normalization, newline/quote handling, emoji isolation, URL masking)
- Full, step‑by‑step workflow documented in **docs/WORKFLOW.md**

## Project Structure

- `README.md` — project overview
- `docs/WORKFLOW.md` — full step‑by‑step workflow
- `r/Bed Rot URL Extract.rmd` — derives stable TikTok permalinks and builds a URL CSV  
- `r/ Bed Rot Text Extraction.rmd` — derives/cleans description and on‑screen text  
- `python/bed_rot_transcripts.py` — generates Whisper transcripts from permalinks
- `data/` — example project dataset
- `LICENSE` — license details


## Getting Started
1. Clone the repo: `git clone https://github.com/cbihlmeyer/TikTok_Web_Scrape_Bed_Rot.git`
3. Install dependencies:
   
R (needed for the .Rmd files):

``` install.packages(c("tidyverse","jsonlite","readr","stringr","stringi","rmarkdown"))```

Python (needed for bed_rot_transcripts.py):

```pip install --upgrade pandas openai-whisper torch```
  
3. Run:
   
```
r/Bed Rot URL Extract.rmd

r/Bed Rot Text Extraction.rmd

python/bed_rot_transcripts.py
```

4. Outputs:
   
See sample in data/processed/.

Full workflow: docs/WORKFLOW.md.


## Documentation

- [Bed Rot TikTok Web Scrape Workflow](docs/WORKFLOW.md)


## Contributing
Pull requests welcome.

## Citations & Acknowledgments

This project uses the **Zeeschuimer** browser extension (Digital Methods Initiative, University of Amsterdam) to collect TikTok URLs and metadata. Please acknowledge and cite Zeeschuimer when using this repository or publishing results.

**Recommended citation:**

Peeters, S. (2025). *Zeeschuimer* (v1.13.4) [Software]. Zenodo. https://doi.org/10.5281/zenodo.18339720

Project home: https://github.com/digitalmethodsinitiative/zeeschuimer

> Note: This repository uses Zeeschuimer **outputs** only; no Zeeschuimer source code is redistributed here.

---

## License

- **This repository:** See the **LICENSE** file at the root of this repo for the license that applies to **all original code and documentation in this project**.
- **Third‑party tools:** **Zeeschuimer** is licensed under the **Mozilla Public License 2.0 (MPL‑2.0)**. If you copy or adapt Zeeschuimer source code in the future, you must comply with MPL‑2.0 terms. This repository does **not** include Zeeschuimer code.

