# CWML R Hands-on Workshop — your workshop folder

Welcome! This folder has everything you need for all eight sessions.

**Course website:** <https://cwml.github.io/CWML-R-Hands-on-Workshop-Series/>
(lessons, setup instructions, and help)

## How to open it

1. Open **Positron**.
2. Choose **File → Open Folder...**, select this `cwml-r-workshop` folder, and click **Open**.
3. Open the student script for today's session from the `student_scripts/` folder.

Always open the **folder**, not a script on its own, so that `here()` can find your data.

## What's inside

```
cwml-r-workshop/
├── student_scripts/   # the scripts you type into, one per session
├── solutions/         # the same scripts with every answer filled in
├── raw_data/          # the original data files (never edit these!)
├── processed_data/    # cleaned data saved during the sessions
├── outputs/           # plots and tables you save
└── .here              # a hidden file that tells here() where this folder starts
```

## Missed a session, or a file stopped working?

Every file a session starts from is already in `processed_data/`. When you save your own
version in class, it replaces that copy. If yours breaks, download the workshop folder again
from the Setup page and copy the file back into `processed_data/`.

All datasets are fictional and were made for teaching.

Questions or feedback: maximilian.wegener@yale.edu
