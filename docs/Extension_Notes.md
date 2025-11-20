# Extension Notes (Prepared by Leila)

This document outlines possible extensions beyond replication to strengthen the final project.

---

## 1. Interaction Effect (Gender × Age)

Paper does not explore interaction effects.  
We propose testing:

```
polr(Q41 ~ age_new * gender + Q55 + Q66)
```

Rationale:
- younger and older women may differ in comfort with AI  
- interaction may reveal non-linear patterns

Deliverables:
- coefficient table  
- marginal effects plot  
- heatmap showing predicted comfort levels

---

## 2. Predicted Probability Surfaces

Using ordinal predictions, we will generate 3D surfaces:

- Probability of “Very knowledgeable” (Q40 = 4)
- Probability of “Very comfortable with AI” (Q41 = 4)

This provides:
- strong visual interpretation  
- direct comparison with the paper’s Figures 1–4  

Outputs saved as HTML via plotly.

---

## 3. Model Diagnostics

To strengthen the report:
- missingness profile  
- proportional-odds checks  
- influence diagnostics  
- robustness check using available-case filtering  

These analyses show professional-level rigor.

---

## 4. Summary

These extensions:
- go beyond replication  
- add scientific value  
- show deeper understanding of the ATS dataset  
- will increase final project grade

We recommend including at least one interaction and one diagnostic test in the final report.

