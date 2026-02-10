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
1. Clone the repo: `git clone https://github.com/<your-username>/<your-repo>.git`
2. Install dependencies: `...`
3. Run: `...`

## Documentation

- [Bed Rot TikTok Web Scrape Workflow](docs/WORKFLOW.md)

## Usage
Brief examples or commands to use the project.


## Contributing
Pull requests welcome.

## License
This project is licensed under the <Your License>.
