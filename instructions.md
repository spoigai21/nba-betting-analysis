# INSTRUCTIONS — Step by Step (for beginners)

A click-by-click walkthrough from nothing installed to your first NBA data loaded and
explored in R. Assumes zero R experience. Do these in order. Each step has a
"You'll know it worked when..." check.

> Work through Steps 1–8 first. Stop at Step 8 and confirm it works — then we do the
> data-cleaning step together (bring your column names). Phases beyond that are in
> README.md.

---

## Step 1 — Install R (the language)

1. Go to https://cran.r-project.org
2. Click the download link for your OS (macOS or Windows).
3. **Windows:** click "base" → "Download R for Windows" → run installer → accept defaults.
   **Mac:** download the `.pkg` (Apple Silicon for newer Macs, Intel for older) → run it
   → accept defaults.

**Worked when:** the installer finishes with no errors. (You won't open R directly —
you'll use RStudio next.)

---

## Step 2 — Install RStudio (the app you'll use)

1. Go to https://posit.co/download/rstudio-desktop/
2. Download **RStudio Desktop** (free version) and install (defaults are fine).
3. Open RStudio.

**Worked when:** RStudio opens and you see a **Console** pane (bottom-left) with a `>`
prompt. That `>` is where R waits for commands.

---

## Step 3 — Run your first line of R

1. Click in the **Console** next to `>`.
2. Type and press Enter:
   ```r
   1 + 1
   ```
3. You should see `[1] 2`.

**Worked when:** R prints `[1] 2`. (`[1]` is just R labeling the output — ignore it.)

---

## Step 4 — Install the packages this project needs

In the Console, paste and press Enter:
```r
install.packages(c("tidyverse", "hoopR", "lubridate", "readr"))
```
- Downloads several tools; may take a few minutes and print lots of text (normal).
- If asked to install from source or restart, you can decline / pick the binary.

**Worked when:** it finishes and returns to `>` with no red error at the very end.
(Yellow warnings are usually fine.)

---

## Step 5 — Create your project folder

1. Make a folder called `nba-market-model` (Desktop is fine).
2. Inside it, make subfolders: `data`, `R`, and `output`.
3. In RStudio: **File → New Project → Existing Directory →** browse to
   `nba-market-model` → **Create Project**.

**Worked when:** RStudio reloads and the top-right shows your project name. Working
inside a Project keeps file paths simple.

---

## Step 6 — Get the data (free)

1. Go to https://www.kaggle.com and make a free account.
2. Search **"MGM Grand NBA betting data"** (closing lines, 2021–2026) or
   **"NBA Betting Data 2007–2026"** (more history).
3. Download the CSV.
4. Move it into your project's `data/` folder; optionally rename to `nba_betting.csv`.

**Worked when:** the `.csv` is inside `nba-market-model/data/`.

---

## Step 7 — Load the data and look at it

1. **File → New File → R Script** (blank editor opens, top-left).
2. Type this (adjust the filename to match yours):
   ```r
   library(tidyverse)   # load the toolkit

   games <- read_csv("data/nba_betting.csv")   # read file into a table called "games"

   glimpse(games)   # show columns and types
   head(games)      # show first few rows
   ```
3. Run: select the lines and press **Ctrl+Enter** (Win) / **Cmd+Enter** (Mac).

**Worked when:** the Console prints the column summary (`glimpse`) and first rows
(`head`). You're now looking at real NBA data in R.

---

## Step 8 — Explore what you have

Run these one at a time in the Console:
```r
ncol(games)     # number of columns
nrow(games)     # number of rows (games)
names(games)    # column names — READ THESE
summary(games)  # quick stats per column
```
**Look at the column names** and identify which columns are:
- the two teams
- the final scores / result
- the spread, total, moneyline (the *lines* — critical)
- the date

Understanding your columns is the foundation. Don't rush past it.

**Worked when:** you can point to which columns hold the line, the result, and the
teams. Write these down.

---

## What's next (after Step 8)

Bring your column names and we'll do these together, in order (see README.md):
1. **Clean** into a tidy table (one row per game: pre-game info + line + result)
2. **Engineer features** (team strength, rest days, etc.)
3. **Build a simple model** (predict total or spread)
4. **Backtest** honestly vs. the lines (with vig)
5. **Forward-test** — log timestamped predictions to `output/track_record.csv`
6. **News & availability signals (advanced):** pull injury/lineup data via open-source
   APIs (not raw ESPN scraping); fold in coach-revealed rotation/rest news going-forward.
   Early on, apply this manually from your own NBA knowledge as you log predictions.

**Stop at Step 8 for now** and confirm it works.

---

## If you get stuck

- **Red error in Console:** copy the exact text and ask — usually a small fix (typo,
  wrong filename, package that didn't install).
- **"could not find function":** the package isn't loaded — run `library(tidyverse)`.
- **"cannot open file" / "No such file":** filename/path is wrong — check the file is in
  `data/` and the name matches exactly (including `.csv`).
- **Nothing happens:** make sure you pressed Enter (Console) or Ctrl/Cmd+Enter (script).

Setting up the environment is the hardest part for beginners. Once data loads, the rest
builds on it — one step at a time.