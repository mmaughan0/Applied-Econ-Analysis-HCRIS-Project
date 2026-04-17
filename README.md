# HCRIS Analysis Project

This project imports HCRIS hospital cost report data, extracts selected variables, and builds an analytic file for downstream analysis.

## Folder structure

- `code/` — scripts for importing, cleaning, and constructing the analytic file
- `source/` — raw source data files
- `reference docs/` — HCRIS documentation and supporting files
- `intermediate/` — temporary or intermediate constructed datasets
- `output/` — final analytic files and outputs

## Workflow

1. Place raw HCRIS files in `source/`
2. Run import and cleaning scripts from `code/`
3. Save intermediate files in `intermediate/`
4. Save final outputs in `output/`

## Notes

Raw source data should generally not be uploaded to GitHub. The repository should mainly contain code, documentation, and possibly small final outputs.