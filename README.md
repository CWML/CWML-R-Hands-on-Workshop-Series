# CWML R Hands-on Workshop Series

A hands-on introduction to data analysis in R with the **tidyverse**. Over eight
sessions you take a messy real-world-style outbreak dataset from raw file to clean
data, summary tables, and publication-ready plots, and finish by writing your own
functions and loops.

**Instructor:** Max Wegener

📖 **Course website:** <https://cwml.github.io/CWML-R-Hands-on-Workshop-Series/> — every lesson, with
explanations, code, TRY ITs with solutions, setup instructions, and the instructor guide.

## Sessions

| \# | Session | You will learn | Student script |
|----|---------|----------------|----------------|
| 1 | Setup and a first look at your data | project folders, `here()`, packages, R basics, the pipe, `import()`, inspecting data | `Session 1 - Setup and First Look_STUDENT.R` |
| 2 | Cleaning | `clean_names()`, `rename()`, `mutate()`, `if_else()`, `case_when()`, fixing types, new variables | `Session 2 - Cleaning_STUDENT.R` |
| 3 | Missing data, joins, and export | missing values, `distinct()`, `replace_na()`, `select()`, joins, `export()` | `Session 3 - Missing Data, Joins, and Export_STUDENT.R` |
| 4 | Data structures | vectors and types, vectorization, subsetting with `[ ]` and `$`, tidy data, `pivot_longer()` / `pivot_wider()` | `Session 4 - Data Structures_STUDENT.R` |
| 5 | Summarizing | factors, `count()`, `group_by()` + `summarise()`, `tabyl()`, a `gtsummary` Table 1 | `Session 5 - Summarizing_STUDENT.R` |
| 6 | Plotting basics | `ggplot2`: histograms, scatter plots, box plots, labels, themes | `Session 6 - Plotting Basics_STUDENT.R` |
| 7 | Plotting further | bar charts, choosing colors, epidemic curves, facets, saving plots | `Session 7 - Plotting Further_STUDENT.R` |
| 8 | Programming *(optional, advanced)* | writing functions, `if`/`else`, `for` loops, `map()` / `walk()` | `Session 8 - Programming_STUDENT.R` |

Each session builds on the one before: the data you clean in Sessions 2–3 is the data you
explore, summarize, and plot in Sessions 4–8. Missed a session? A ready-made copy of every in-between
file is already in `processed_data/`, so you can start any session.

## How each session works

Each session is split into short parts. Every part follows the same rhythm:

1. **👀 Follow along:** the instructor live-codes a new skill on the workshop
   dataset, and you type the same code into your student script in `student_scripts/`.
2. **✏️ TRY IT:** straight away, you do the same thing yourself on the practice
   dataset (`raw_data/measles_raw.csv`, a measles outbreak). Each TRY IT takes
   1–5 minutes and has a ✅ **Check** line so you can tell whether you got it right.
3. Put up your **green sticky note** when you're done, or your **red** one if
   you're stuck and a helper will come over.

The student scripts also have **📖 What you need to know** notes for each part
and **⚠️ Common mistakes** to watch for, so they work as a reference after the
workshop too.

**Check your work** in the `solutions/` folder: each session has a copy of the
student script with every Follow along and TRY IT filled in. Try it yourself first!

## Pre-workshop checklists

Please do these **before** each session. Setup problems take a long time to fix
during class, so every session starts with everyone ready to code. Each
checklist ends with a test: if the test prints the expected answer, you're ready.

### Before Session 1 (one-time setup, about 30 minutes)

- [ ] **Install R** from [CRAN](https://cran.r-project.org/) (version 4.2 or newer)
- [ ] **Install Positron** (free) from [Posit](https://positron.posit.co/download.html)
      (install R first). Positron is the program you write and run R code in.
- [ ] **Download the workshop folder:** on the
      [GitHub page](https://github.com/CWML/CWML-R-Hands-on-Workshop-Series), click
      **Code → Download ZIP**. Unzip it somewhere easy to find (Documents, not
      Downloads). This one download has everything for all six sessions.
- [ ] **Open the workshop folder in Positron:** **File → Open Folder...**, select the
      unzipped `CWML-R-Hands-on-Workshop-Series` folder, and click **Open** (click **Yes** if
      asked whether you trust the authors). An R **Console** starts at the bottom.
- [ ] **Install the packages.** Copy this into the Console (bottom) and press Enter:

  ```r
  install.packages(c("tidyverse", "here", "rio", "janitor", "skimr", "gtsummary", "flextable", "scales"))
  ```

  It can take 5–10 minutes. If you see compiler errors, see
  [Setup troubleshooting](#setup-troubleshooting).
- [ ] **Test:** copy this into the Console and press Enter:

  ```r
  library(tidyverse); library(here); library(rio); library(janitor); library(skimr); library(gtsummary); library(flextable)
  nrow(import(here("raw_data", "measles_raw.csv")))
  ```

  ✅ **Ready if it prints `[1] 1215`.** Red messages that don't start with
  "Error" are normal. If you see an error, email the instructor before the session.

**Already use RStudio?** That works too: open the workshop by double-clicking
`CWML-R-Hands-on-Workshop-Series.Rproj` instead of using Open Folder.

### Before every session (2 minutes)

- [ ] Open the workshop **folder** in Positron (**File → Open Recent** lists it),
      not a script on its own
- [ ] Open that session's student script from the `student_scripts/` folder
- [ ] Run that session's test below

| Session | Bring / have ready | Test (copy into the Console) | ✅ Ready if it prints |
|---|---|---|---|
| 1 | The one-time setup above | See above | `[1] 1215` |
| 2 | Your measles problem list from Session 1 | `nrow(rio::import(here::here("raw_data", "linelist_raw.xlsx")))` | `[1] 6604` |
| 3 | — | `nrow(rio::import(here::here("processed_data", "linelist_clean.rds"), trust = TRUE))` | `[1] 6604` |
| 4 | — | `nrow(rio::import(here::here("processed_data", "combined_linelist.rds"), trust = TRUE))` | `[1] 6451` |
| 5 | — | `nrow(rio::import(here::here("processed_data", "measles_final.rds"), trust = TRUE))` | `[1] 1163` |
| 6 | — | `nrow(rio::import(here::here("processed_data", "measles_final.rds"), trust = TRUE))` | `[1] 1163` |
| 7 | — | `dir.exists(here::here("outputs"))` | `[1] TRUE` |
| 8 | — | `purrr::map_dbl(1:3, ~ .x * 2)` | `[1] 2 4 6` |

**Missed a session, or did your saved file get broken?** Every file a session
starts from is already in `processed_data/`. When you save your own version in
class, it replaces that copy. If yours stops working, download a fresh copy of
the file from GitHub and put it back in `processed_data/`.

**Bringing your own data?** Put the file in `raw_data/` before the session. Every
session ends with a ⭐ BONUS for your own data.

## Folder structure

```
CWML-R-Hands-on-Workshop-Series/
├── CWML-R-Hands-on-Workshop-Series.Rproj   # only needed if you use RStudio
├── README.md                               # this file
├── student_scripts/                        # student scripts, one per session
├── solutions/                              # the student scripts with the code filled in
├── pages/, index.qmd, _quarto.yml          # source of the course website (Quarto)
├── docs/                                   # the built website (published by GitHub Pages)
├── raw_data/                               # original data files (never edit these)
├── processed_data/                         # cleaned data saved during the sessions
└── outputs/                                # plots and tables you save (Sessions 5, 7, and 8)
```

## Data

All datasets are fictional and created for teaching.

| File | Description |
|------|-------------|
| `raw_data/linelist_raw.xlsx` | Workshop dataset: an outbreak case list with 6,604 cases and plenty of problems to clean |
| `raw_data/case_info.csv` | Symptoms and temperature for each case, joined to the linelist in Session 3 |
| `raw_data/hosp_info.txt` | Lookup table of correct hospital names |
| `raw_data/measles_raw.csv` | Practice dataset for the "Your turn" exercises: a measles outbreak with similar problems |
| `raw_data/district_info.csv` | Lookup table of district populations, for the practice join exercises |

---

# Setup troubleshooting

## Windows users – installing Rtools (required for compiling R packages)

Some R packages are available only as source code. On Windows you need
Rtools (a collection of compilers and build utilities) so that R can
install those packages. macOS and Linux already ship suitable build
tools; Windows does not.

1)  Do I already have Rtools?

In a fresh R session (the Positron Console) run:

```r
Sys.which("make")
```

-   If you get a blank result (""), R cannot find make ⇒ Rtools is
    missing. • If you see a full path containing
    rtools…\usr\bin\make.exe, you’re all set! NO NEED TO GO ANY FURTHER!

2)  Find the Rtools version that matches your R

Check your R version:

```r
R.version.string
```

-   Your R version Install this Rtools 4.5.x Rtools45 4.4.x Rtools44
    4.3.x Rtools43 4.2.x Rtools42 (no longer needed if you upgraded R)

3)  Download Rtools

-   Go to <https://cran.r-project.org/bin/windows/Rtools/> Click the
    folder for your version (e.g. rtools43 for 4.3; rtools44 for 4.4;
    rtools45 for 4.5). Download the .exe installer (≈ 200 MB).

4)  Install Rtools

-   Close Positron (or RStudio). Run the downloaded installer. When prompted, tick
    “Add Rtools to system PATH.” Accept the default install path
    (C:\rtools45, C:\rtools44, …). Finish the installation (it may take
    a minute).

5)  Verify the installation

-   After installation open a new R session (or reboot) and run:

```r
Sys.which("make")
#>                  make 
#> "C:\\rtools45\\usr\\bin\\make.exe"   # or rtools44 / rtools43
```

---

## Mac users – installing gfortran and Xcode Command Line Tools (required for compiling R packages)

-   macOS users – installing gfortran (required for compiling R
    packages)

-   Some R packages require a Fortran compiler to install from source.

-   macOS does not include one by default.

-   You need to install gfortran manually and the version must match
    your R version.

1.  Do I already have gfortran?

In a fresh R session (the Positron Console) run:

```r
Sys.which("gfortran") 
```

-   If you get a blank result (""), gfortran is missing ⇒ you need to
    install it.

-   If you see a path like /opt/gfortran/bin/gfortran, you're all set!
    NO NEED TO GO ANY FURTHER!

2.  Find the gfortran version that matches your R Check your R version:

-   Look at the top of the R console window OR

-   copy/paste this into your terminal

```
R.version.string 
```

-   If your R version is 4.5.x Install gfortran

-   If your R version is 4.3 to 4.4.x install gfortran 12.2

-   If you R version is older then no need to install gfortran

3.  Download gfortran

-   <https://mac.r-project.org/tools/gfortran-12.2-universal.pkg>

-   <https://mac.r-project.org/tools/gfortran-14.2-universal.pkg>

4.  Install gfortran

-   Close Positron (or RStudio).

-   Double-click the downloaded .pkg file.

-   Follow the installer prompts and accept the default install path.
    Enter your Mac password when prompted. Finish the installation.

5.  Verify the installation After installation open a new R session
    and run:

```
Sys.which("gfortran") 
#\> gfortran 
#\> "/opt/gfortran/bin/gfortran" 
```

-   If you see the path above, gfortran is installed correctly ✅

-   If you still get a blank result, try restarting your Mac and running
    again.

6.  One more check - verify R and gfortran are in sync

-   Run this in R:

```
system("gfortran --version") 
```

You should see output like:

```
GNU Fortran (GCC) 14.2.0 # if you are on R 4.5 
GNU Fortran (GCC) 12.2.0 # if you are on R 4.4 or 4.3 
```

-   If the version number does not match your R version, you have the
    wrong gfortran installed and need to download the correct version
    from <https://mac.r-project.org/tools/>

-   macOS users – fixing missing C++ header files (required for
    compiling R packages)

If you see errors like these when installing packages:

```
fatal error: 'cmath' file not found 
fatal error: 'cstdlib' file not 
found fatal error: 'cstring' file not found 
```

This means your Xcode Command Line Tools are missing or out of date.
This is common after a macOS upgrade.

a.  Do I have this problem?

In R run:

```
system("xcode-select -p")
```

-   If you get an error or a blank result ⇒ Command Line Tools are
    missing. • If you see /Library/Developer/CommandLineTools ⇒ they are
    installed. NO NEED TO GO ANY FURTHER!

b.  Fix the problem Close Positron (or RStudio) completely. Open the Mac
    Terminal application and run these commands one at a time:

#### Step 1 - Remove the old Command Line Tools:

```bash
sudo rm -rf /Library/Developer/CommandLineTools 
```

-   You will be prompted for your Mac password.
-   Type it and press Enter (you will not see the password as you type -
    this is normal).

#### Step 2 - Reinstall the Command Line Tools:

```bash
xcode-select --install 
```

-   A popup window will appear.
-   Click Install and wait for it to finish.
-   **This may take 5 to 10 minutes depending on your internet
    connection**

#### Step 3 - Verify the installation was successful:

```bash
xcode-select -p 
```

-   You should see:

```
/Library/Developer/CommandLineTools
```

a.  Try installing your packages again

-   Once the Command Line Tools are installed, reopen Positron and re-run
    the `install.packages()` line in Part 2 of the Session 1 student
    script.

#### ⚠️ Note for macOS beta users:

If you are running a beta version of macOS, package compilation errors
are more likely because R and its tools may not yet be fully compatible
with the newest macOS releases. If you continue to have problems after
reinstalling the Command Line Tools, the most reliable solution is to
use the stable (non-beta) release of macOS.

#### 🔑 Quick Reference Card

Your Setup R Version Need

Windows 4.5.x = Rtools45

Windows 4.4.x = Rtools44

Windows 4.3.x = Rtools43

Mac 4.5.x = gfortran 14.2

Mac 4.4.x or 4.3.x = gfortran 12.2

Mac (any version) = any Xcode Command Line Tools
