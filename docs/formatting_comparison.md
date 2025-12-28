# Formatting comparison (ANOVA vs Regression)

Goal: quick side-by-side to harmonize numeric formatting across verbatim and DOCX outputs.

| Area | Output type | Module | Table | Numeric formatting | p-value formatting | Notes |
|---|---|---|---|---|---|---|
| Regression | Verbatim | LM/LMM | ANOVA Type III | 4 decimals via `formatC(..., digits=4, drop0trailing=TRUE)` on all numeric cols except p | `<0.0001` or 4-decimal `formatC(..., digits=4, drop0trailing=TRUE)` | F column also formatted to 4 decimals; residual F blanked. | 
| Regression | Verbatim | LM/LMM | Coefficients | 6 decimals fixed (`formatC(..., digits=6, drop0trailing=FALSE)`) | `format.pval(..., digits=3, eps=1e-04)` | Matches legacy console output. |
| Regression | Verbatim | LMM | ICC | 6 decimals fixed (`formatC(..., digits=6, drop0trailing=FALSE)`) | n/a | Printed only when present. |
| Regression | Verbatim | LMM | Random effects | Base `print(VarCorr)` defaults | n/a | No explicit digits. |
| Regression | DOCX | LM/LMM | ANOVA Type III | `round(..., 4)` + flextable `digits=4` | `<.0001` or `sprintf("%.4f")` | Table renders fixed 4-decimal numeric columns. |
| Regression | DOCX | LM/LMM | Coefficients | `round(..., 4)` + flextable `digits=4` | `<.0001` or `sprintf("%.4f")` | p-values stored as strings. |
| Regression | DOCX | LM/LMM | Random effects | `round(..., 4)` for Variance/StdDev | n/a | ICC rounded to 4. |
| ANOVA | Verbatim | One/Two-way | ANOVA Type III | `round(..., 4)` for numeric cols (except p) | `<.0001` or `sprintf("%.4f")` | Base `print()` may drop trailing zeros. |
| ANOVA | Verbatim | One/Two-way | Post-hoc | emmeans defaults (no explicit rounding in UI) | emmeans defaults | Printed raw from emmeans. |
| ANOVA | DOCX | One/Two-way | ANOVA Type III | `round(..., 4)` + flextable `digits=4` | `<.0001` or `sprintf("%.4f")` | Fvalue label uses `sprintf("%.4f")`. |
| ANOVA | DOCX | One/Two-way | Post-hoc contrasts | `round(..., 4)` (except p) + flextable `digits=4` | `<.0001` or `sprintf("%.4f")` | p-values stored as labels. |

Potential harmonization targets (not applied yet):
- Standardize p-value string style: `<0.0001` vs `<.0001`.
- Decide whether verbatim output should always show fixed 4 decimals (avoid base print trimming).
- Align regression coefficient p-value formatting with ANOVA p-value labels (4 decimals + threshold).
