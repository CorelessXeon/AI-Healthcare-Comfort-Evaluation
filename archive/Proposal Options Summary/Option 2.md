# Replication Feasibility Assessment (Paper 2)

**Title**: *Knowledge is not all you need for comfort in use of AI in healthcare*  
**Link**: https://doi.org/10.1016/j.puhe.2024.11.019  

---

## 1. Influence  
*(Omitted)*  

---

## 2. Data Availability  
- **Source**: 2023 national survey conducted by Environics Research (commissioned by CIFAR, publicly available via Canada Health Infoway).  
- **Survey structure**: Four core questions (Q1–Q4), all measured on 1–4 Likert scales. Control variables include age, gender, education, and income.  
- **Data status**: The paper does not provide raw data directly but clearly states that the dataset is publicly available. Replication requires downloading the microdata from Infoway or OSF.  
- **Conclusion**: Apart from access permission, no significant obstacles exist. Once the raw survey data is obtained, replication is fully feasible.  

---

## 3. Suggested Replication Scope (Q1–Q4)  
- **Baseline objectives**:  
  - **Q1**: Knowledge about AI  
  - **Q2**: Comfort with AI use in healthcare  

- **Extended coverage**:  
  - **Q3**: Comfort with data use *with informed consent*  
  - **Q4**: Comfort with data use *without informed consent but de-identified*  

- **Reference results**: Appendix Table A1 provides all regression outputs (odds ratios, confidence intervals, significance levels) as direct benchmarks.  

---

## 4. Replication Difficulty (Low–Moderate)  
- **Methodological difficulty**:  
  - All analyses rely on *ordinal logistic regression*, which can be fully implemented in R.  
  - Statistically straightforward, models are transparent.  

- **Data processing difficulty**:  
  - Exclude "Don’t know / Unsure" responses.  
  - Recode categorical variables such as education and income into factors.  
  - Sample size: ~10,000–12,000 respondents, moderate cleaning workload.  

- **Overall difficulty**: Low-to-moderate.  
  - Q1–Q2 are easiest to replicate.  
  - Q3–Q4 slightly more complex due to response distribution, but no essential differences.  

---

## 5. Extension Difficulty (Varies)  
Potential directions for course-level extensions:  

- **Extension 1: Full Q1–Q4 coverage**  
  - Replicate Q1/Q2, then expand to Q3/Q4 to demonstrate pipeline generalizability.  
  - Difficulty: Low, but adds replication depth.  

- **Extension 2: Subsample analysis**  
  - Run regressions by subgroups (e.g., gender, income, education).  
  - Feasible with existing data, manageable workload.  

- **Extension 3: Temporal / cross-sectional comparison**  
  - If past Infoway surveys (CDHS) are accessible, compare 2023 vs. earlier waves.  
  - Higher difficulty due to additional data requirements.  

- **Extension 4: Methodological expansion**  
  - Explore polynomial regressions (author used Excel plug-ins for non-linear patterns).  
  - Add interaction terms (e.g., Gender × Education) in R.  
  - Technically feasible, shows methodological flexibility.  

---
