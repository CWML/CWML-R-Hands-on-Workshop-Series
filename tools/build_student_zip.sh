#!/usr/bin/env bash
# Builds the student download: dist/cwml-r-workshop.zip
#
# It contains only what learners need (scripts, solutions, data, an empty outputs
# folder, the .here file, and a short README). The website, drafts, and proposal
# files are left out. Run from the repo root:   bash tools/build_student_zip.sh
# The GitHub Action in .github/workflows/student-zip.yml runs this on every push.

set -euo pipefail

NAME="cwml-r-workshop"
OUT="dist/$NAME"

rm -rf dist
mkdir -p "$OUT/student_scripts" "$OUT/solutions" "$OUT/raw_data" "$OUT/processed_data" "$OUT/outputs"

# Scripts and solutions (only the .R files, no history or system files)
cp student_scripts/*.R "$OUT/student_scripts/"
cp solutions/*.R "$OUT/solutions/"

# Raw data used by Sessions 1-8 (the lab and Connecticut files belong to drafts in proposal/)
for f in linelist_raw.xlsx case_info.csv hosp_info.txt measles_raw.csv district_info.csv; do
  cp "raw_data/$f" "$OUT/raw_data/"
done

# Ready-made files that sessions start from
for f in linelist_clean.rds measles_clean.rds combined_linelist.rds measles_final.rds; do
  cp "processed_data/$f" "$OUT/processed_data/"
done

# Empty outputs folder (zip drops empty folders, so keep a placeholder)
touch "$OUT/outputs/.gitkeep"

# here() finds the top of the folder through this file
touch "$OUT/.here"

cp tools/student-README.md "$OUT/README.md"

(cd dist && zip -qr "$NAME.zip" "$NAME" -x "*.DS_Store")
echo "Built dist/$NAME.zip ($(du -h "dist/$NAME.zip" | cut -f1))"
