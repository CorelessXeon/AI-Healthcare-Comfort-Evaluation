# Replication Feasibility Assessment (Paper 2)

**Title**: *Knowledge is not all you need for comfort in use of AI in healthcare*  
**Link**: https://doi.org/10.1016/j.puhe.2024.11.019  

---

## 1. Data Availability  
- **Dataset**: Canadian Digital Health Survey (CDHS) 2021, commissioned by *Canada Health Infoway* and conducted by Leger.  
- **Sample size**: ~12,052 respondents aged 16+ from across Canada.  
- **Demographics collected**: Gender, Age, Income, Education.  
- **Core AI-related questions**:  
  1. Knowledge about AI  
  2. Comfort with AI in healthcare  
  3. Comfort with scientists using personal health data *with informed consent*  
  4. Comfort with scientists using *de-identified health data without consent*  
- **Access**: Publicly available through Canada Health Infoway’s Insights and Borealis Dataverse repositories (e.g., DOI: 10.5683/SP3/CEYG42).  
- **Conclusion**: Data access is straightforward, with no proprietary restrictions.  

---

## 2. Data Processing Pipeline  
1. Remove “Don’t know” or “Uncertain” responses for AI-related questions.  
2. Convert Likert responses (1–4) into **ordinal factors**.  
3. Recode demographics into categorical variables:  
   - Age grouped into 5 bands (16–24, 25–34, 35–54, 55–64, 65+).  
   - Gender as Male (ref.), Female, Other.  
   - Education: 7 levels from High School (ref.) to PhD/Medical.  
   - Income: 7 brackets (<$25k up to $250k+).  
4. Prepare factors with correct reference categories for regression analysis.  

---

## 3. Scope of Reproduction  
- **Primary analyses**: Four **ordinal logistic regressions** predicting AI knowledge and comfort (Q1–Q4) based on demographics.  
- **Outputs**: Odds ratios, confidence intervals, and p-values comparable to Appendix Table A1.  
- **Additional reproducible elements**: Descriptive statistics, cross-tabulations, and subgroup means.  
- **Optional**: Polynomial/interaction models shown in Figures 1–4 (more complex, originally done in Excel DOE Pro).  

---

## 4. Reproduction Difficulty  
- **Data acquisition**: Low – datasets are public and well-documented.  
- **Data cleaning**: Moderate – categorical recoding and exclusion of uncertain responses.  
- **Modeling methods**: Moderate – standard ordinal logistic regressions in R (using `MASS::polr` or `ordinal::clm`).  
- **Overall**: Low-to-moderate difficulty; feasible for graduate-level replication.  

---

## 5. Extension Potential  
- **Subgroup analysis**: Run regressions by gender, age group, or healthcare professionals vs. general population.  
- **Interaction effects**: Add interaction terms (e.g., Age × Gender, Knowledge × Demographics) to explore moderation.  
- **Longitudinal comparison**: Extend analysis using **CDHS 2023** (Environics Research + CIFAR, n ≈ 10,130). Compare 2021 vs. 2023 to assess changes in AI knowledge and comfort.  
- **Additional variables**: Incorporate other survey items (e.g., digital health usage, regional variation) to enrich the model.  

---

**Summary**:  
The replication of this paper is highly feasible: the data is publicly available, the analysis uses standard methods in R, and both replication and extension can be completed within a graduate course project. Extensions—such as subgroup analysis and longitudinal comparisons with 2023 CDHS—offer clear added value.
