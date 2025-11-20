---
output:
  pdf_document: default
  html_document: default
---
# Appendix A1 Replication – Ordinal Logistic Regression  
Prepared by **Leila Safari**, Group 11

This document explains the full method used to replicate Appendix A1 of the article:  
**“Knowledge is not all you need for comfort in use of AI in healthcare.”**

It covers:
- Modeling strategy (Ordinal Logistic Regression)
- Variable coding and reference groups
- Missing-value and filtering rules
- Model specification for Q40–Q43
- Comparison with the original paper
- Interpretation of major effects
- Notes on discrepancies (detailed in Gaps.md)

---

## 1. Modeling Framework

Appendix A1 uses **Ordinal Logistic Regression (Proportional Odds Model)** to analyze four 4-point Likert outcomes:

1 = Not at all  
2 = Not very  
3 = Somewhat  
4 = Very  

Because the categories are ordered but not equally spaced, ordinal regression is the correct method.

We replicated the method using:

```r
MASS::polr(..., method = "logistic")
```

This fits the proportional odds logit model, which assumes the effect of predictors is the same at every threshold.

---

## 2. Variables and Coding

### **Dependent variables (four separate models)**
- **Q40** — Knowledge about AI  
- **Q41** — Comfort with AI in healthcare  
- **Q42** — Comfort with AI research *with consent*  
- **Q43** — Comfort with AI research *without consent*  

All treated as ordered factors (1 < 2 < 3 < 4).

### **Independent variables**

| Variable | Meaning | Reference category |
|---------|----------|--------------------|
| `age_new` | Age group | **16–24** |
| `gender` | Gender identity | **Male** |
| `Q55` | Household income | **< $24,999** |
| `Q66` | Education | **High school** |

All variables were set as ordered factors with explicit reference levels to match Appendix A1.

---

## 3. Missing Values and Filtering

Codes treated as missing:

| Code | Meaning |
|------|----------|
| 96 | Other (specify) |
| 97, 98 | Prefer not / Refused |
| 99 | Don’t know |

Two filtering rules exist in the project:
- **Strict filtering** — *all 8 variables must be valid*  
- **Available-case filtering** — *only predictors must be valid*

The strict rule produces **N = 9,198**, which matches Table 1 in the published article.  
Therefore, strict filtering is used for Appendix A1 replication.

---

## 4. Model Specification

Each outcome Q40–Q43 is modeled separately:

```
logit(P(Y ≥ k)) = α_k – βX
```

Predictors:
- Age
- Gender
- Income
- Education

No interaction terms are included in the baseline replication (to match the paper).

We tested proportional-odds assumptions using:
```r
brant::brant(model)
```

Notes on assumption failures appear in **Gaps.md**.

---

## 5. Replication Steps

1. Load `clean_data_strict.rds`
2. Convert all predictors into ordered factors  
3. Set reference categories to match the paper  
4. Fit ordinal logistic models with `polr()`  
5. Convert results into odds ratios and 95% CIs  
6. Export formatted tables using `04_export_regression_tables.R`
7. Compare with the published Appendix A1

---

## 6. Summary of Findings

Across all four dependent variables:

### **Age**
Older groups have:
- Lower AI knowledge  
- Lower comfort with AI tools  
- Lower support for data usage without consent  

Pattern matches paper.

### **Gender**
Female respondents:
- Lower odds of high AI knowledge  
- Lower comfort across Q41–Q43  

Pattern matches paper.

### **Income**
Higher income generally increases odds of:
- comfort with AI  
- acceptance of data usage  

Pattern very close to paper.

### **Education**
Education has the strongest gradient:
- University+, Masters, and PhD strongly increase knowledge and comfort  
- Effects are consistent with the published results  

---

## 7. Consistency and Discrepancies

Our replication:
- Matches the direction of effects  
- Matches ordering across categories  
- Matches statistical significance patterns for most predictors  

Small coefficient differences are documented in **Gaps.md** with explanations:
- strict vs available-case filtering  
- proportional-odds assumption violations  
- differences between R vs. SPSS models  

---

## 8. Output Files

- `output/tableA1_combined.html`  
- `output/tableA1_combined.csv`  
- `docs/Gaps.md`  
- (This file) `docs/AppendixA1_Writeup.md`

---

This Appendix A1 replication satisfies the course requirement of reproducing at least one key result from the paper. In the final report, these results will be presented side-by-side with the original odds ratios and discussed in relation to strict vs available-case filtering and the proportional-odds assumption.
