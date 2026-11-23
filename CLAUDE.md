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
| `tools/build_student_zip.sh`, `.github/workflows/student-zip.yml` | Build and publish the **student download** `cwml-r-workshop.zip` (student scripts, solutions, the raw/processed data Sessions 1–8 use, `outputs/`, `.here`, `tools/student-README.md`) to the `student-files` GitHub release on every push. If a session starts using a new data file, add it to the script's file lists |
| `data-raw/make_lab_data.R` | Regenerates the lab substudy files (fixed seed) |
| `proposal/` | Draft redesign proposal and draft lesson — not on the website |
| `docs/`, `_freeze/` | **Generated** by `quarto render`. Never edit by hand; commit both |
| `Notes/`, `instructor scripts/` | Private, git-ignored. `instructor scripts/make_practice_data.R` regenerates the measles data; old material is archived in `instructor scripts/OLD/` |

Sessions: 1 R basics, importing, inspecting · 2 Cleaning · 3 Missing data, joins, export ·
4 Data structures (vectors, lists, subsetting, pivoting) · 5 Summarizing ·
6 Plotting basics · 7 Plotting further · 8 Programming (optional).

Session 1 order: R basics → packages and "error, warning, or message?" → project folders and
`here()` → importing → inspecting (`dim()`, `glimpse()`, `skim()`, `count()`) → "fix the broken
code" → problem list. **Session 1 uses no pipe**: one function at a time (`import(here(...))` is
the only nesting). The pipe `%>%`, `filter()`, `arrange()`, and `select()` are taught in
Session 2, Part 2; building a `data.frame()` in Session 4; missing values and duplicates in Session 3.

**Pacing:** plan about 75 minutes of content per 90-minute session, so there is time for
questions. Each instructor page has "Start by" times per part and a "Running behind? Skip in
this order" list. Base R 🔁 demos are reading only, except in Session 4 where base R is the topic.

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
- **Numbering:** Follow alongs are numbered by part (5.3 = Part 5, third one), TRY ITs 1, 2, 3…
  straight through the session. The same labels appear in the student script, the solutions,
  the lesson page (a `::: follow-along` box above the code, with the script's `Pattern:` line),
  and the instructor answer table.
- **Explanations are on the lesson page, for everyone** (librarians teach from it and patrons
  reread it), not on the instructor pages:
  - a `::: workflow` strip after "In this session" (Import → … → Automate, today's steps
    marked `.today` and named in the sentence above it); the home page has the full strip
  - an **open** "🔍 What this code does" callout after every Follow along's code
  - "📖 Good to know" callouts with background at the end of a part
  - one "🧩 Reading a function call" box per session, breaking down its key call
- **Lesson pages speak to the learner** ("you"); timing, teaching notes, and "ask the class"
  prompts go on the instructor pages.
- **IDE is Positron** for everyone, not RStudio: open the folder with File → Open Folder;
  pane names are Variables and Explorer. Don't offer RStudio as an alternative (the only
  RStudio mentions left are the pane-name note for instructors and the troubleshooting page).
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
