# CWML R Hands-on Workshop Series

An 8-session, hands-on R course (tidyverse plus base R foundations) from the Yale
Cushing/Whitney Medical Library. Published as a Quarto website on GitHub Pages:
https://cwml.github.io/CWML-R-Hands-on-Workshop-Series/

## Layout

| Path | What it is |
|---|---|
| `index.qmd`, `pages/` | Website source. Lessons in `pages/lessons/0N-<slug>.qmd`, instructor notes in `pages/instructor/session-0N.qmd`, plus setup, data, resources, troubleshooting, and the Base R ↔ tidyverse page |
| `student_scripts/` | `Session N - <Title>_STUDENT.R` — what learners type into |
| `solutions/` | `Session N - <Title>_SOLUTIONS.R` — the student script with every answer filled in |
| `raw_data/` | Workshop data (`linelist_raw.xlsx`, `case_info.csv`, `hosp_info.txt`), measles practice data (`measles_raw.csv`, `district_info.csv`), and lab substudy data |
| `processed_data/` | Ready-made files passed between sessions (`.rds`), so learners can join any session |
| `outputs/` | Where learners save plots and tables |
| `data-raw/make_lab_data.R` | Regenerates the lab substudy files (fixed seed) |
| `proposal/` | Draft redesign proposal and draft lesson — not on the website |
| `docs/`, `_freeze/` | **Generated** by `quarto render`. Never edit by hand; commit both |
| `Notes/`, `instructor scripts/` | Private, git-ignored. `instructor scripts/make_practice_data.R` regenerates the measles data; old material is archived in `instructor scripts/OLD/` |

Sessions: 1 Setup and first look · 2 Cleaning · 3 Missing data, joins, export ·
4 Data structures (vectors, lists, subsetting, pivoting) · 5 Summarizing ·
6 Plotting basics · 7 Plotting further · 8 Programming (optional).

## Keep these in sync

A change to a session usually touches **four files**: the lesson page, the student script,
the solutions script, and the instructor page. Exercise wording and ✅ Check values must
match across all of them.

## Conventions

- **Format:** each part is 👀 Follow along (workshop linelist) → ✏️ TRY IT (measles practice
  data) with a ✅ Check and a collapsed, **runnable** "Show solution". Collapsible
  "🤔 Think about it" questions, "⚠️ Common mistakes" warning boxes, "🔁 The base R way"
  boxes, ⭐ Bonus, and Key points. Student scripts use 📖 notes and `Pattern:` comment lines,
  never `_` / `FUNCTION_NAME` blanks.
- **Lesson pages speak to the learner** ("you"); timing, teaching notes, and "ask the class"
  prompts go on the instructor pages.
- **IDE is Positron**, not RStudio: open the folder with File → Open Folder; pane names are
  Variables and Explorer; mention RStudio only as a "works too" alternative.
- **No renv.** Packages: `install.packages(c("tidyverse", "here", "rio", "janitor", "skimr",
  "gtsummary", "flextable", "scales"))`. Word export uses
  `as_flex_table() %>% flextable::save_as_docx()`, not `gt::gtsave()` (needs pandoc).
- **Data between sessions is `.rds`** (CSV turns missing text into `""`). Import with
  `import(here(...), trust = TRUE)`. Factor order is not saved — re-set it with
  `fct_relevel()` after importing.
- **Plot colors:** `#2a78d6` blue, `#eb6834` orange, `#9a9a95` gray for Unknown
  (colorblind-safe; avoid red vs green).
- **Short output:** `import()` returns a plain data.frame that prints every row — use
  `head()`. No output longer than about 40 lines.
- Every number in a lesson, Check, or answer must match real output: run the code.

## Accessibility (required)

- Every plot chunk has `#| fig-alt:` (a list when a chunk makes several plots). Look at
  the plot before writing it; check quoted numbers against the data.
- `styles.css` contains the dark-mode fix for inline code in headings and callout titles —
  keep it.
- The Feedback link (navbar, footer, home page) goes to maximilian.wegener@yale.edu.

## Build and verify

1. Edit the `.qmd` / `.R` files, then run `quarto render`.
2. Run all solutions in order to check them (`Session 2` and `3` rewrite `processed_data/`,
   so test in a copy of the project if you don't want those files changed).
3. Check: no new error outputs (Session 8 has two intentional `error: true` demos),
   0 images without alt text in `docs/`, and light/dark screenshots of changed pages.
4. Ask before committing and pushing. Pushing to `main` updates the live site
   (GitHub Pages serves `main` / `docs`) within a minute or two.

The `/quarto-site` personal skill has the full checklist for new Quarto sites.
