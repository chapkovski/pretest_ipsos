# pretest_ipsos

This repository contains preregistration analysis scripts, Quarto reports, and derived outputs for the pretest study.

## Data

Raw input files belong in [data](data).

Expected files currently used by the analysis scripts:

- `data/POLITSOLID_Pretest_interim_06032026.sav`
- `data/POLITSOLID_Pretest_interim_11032026.sav`
- `data/ESS11e04_1.dta`

Notes:

- The repository keeps [data/.keep](data/.keep) so the directory exists in Git.
- All other files inside [data](data) are ignored by Git and will not be included in commits.
- If you add new raw files to [data](data), they will also stay ignored automatically.

## Repository Layout

- [analysis_helpers.R](analysis_helpers.R): shared data cleaning and helper functions.
- [univariate_checks.R](univariate_checks.R): script version of the univariate checks workflow.
- [univariate_checks.qmd](univariate_checks.qmd): Quarto report for univariate checks.
- [core1_prereg_analysis.qmd](core1_prereg_analysis.qmd): Quarto analysis for core preregistration 1.
- [core2_prereg_analysis.qmd](core2_prereg_analysis.qmd): Quarto analysis for core preregistration 2.
- [income_prereg_analysis.qmd](income_prereg_analysis.qmd): Quarto analysis for the income preregistration.
- [output](output): generated summary tables and exported check results.
- [preregs](preregs): preregistration source documents.

## Git Notes

The ignore rules are set up to exclude local R and Quarto working files, including cache directories and machine-specific metadata, while still allowing analysis code and generated outputs to be tracked if you want them in the repository.

## Usage

Typical workflow:

1. Put the raw `.sav` and `.dta` input files in [data](data).
2. Render or run the analysis from the project root so relative paths resolve correctly.
3. Review generated tables in [output](output) and rendered reports in the project root.

Examples:

```bash
quarto render univariate_checks.qmd
quarto render core1_prereg_analysis.qmd
quarto render core2_prereg_analysis.qmd
quarto render income_prereg_analysis.qmd
```

For the script workflow:

```bash
Rscript univariate_checks.R
```

## License

This repository is currently intended to remain private. No open-source license is attached at this stage, so reuse and redistribution should be treated as restricted unless a separate license file is added later.