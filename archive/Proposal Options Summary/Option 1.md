# Replication Feasibility Assessment (Paper 1)

**Title:** High-skilled immigration enhances regional entrepreneurship  
**Link:** [https://www.pnas.org/doi/10.1073/pnas.2402001121](https://www.pnas.org/doi/10.1073/pnas.2402001121)
**DatasetLink:** [https://osf.io/hrx94/files/osfstorage](https://osf.io/hrx94/files/osfstorage)
---

## 1. Influence
(Omitted)

---

## 2. Data Availability
Most of the datasets used in the paper are publicly accessible:

- **H-1B visa and LCA data**: available from USCIS.  
- **ACS population characteristics**: accessible via IPUMS / Census.  
- **Patent data**: provided by USPTO.  
- **SCP entrepreneurship index**: available from the Startup Cartography Project.  

The only exception is the **PitchBook venture investment data**, which is proprietary and not included in the OSF package. However, this does not affect the core results of the paper, so the main replication objectives remain feasible.

---

## 3. Suggested Replication Scope
- **Table 1**: Baseline regression results  
- **Table 2**: Instrumental variable regressions (Bartik)  
- **Table 3**: Placebo regressions using H-2B visas  
- **Figure 1**: H-1B time trends  
- **Figure 2**: Event study plots (leads and lags)  

---

## 4. Replication Difficulty
**Overall rating:** Moderate, with data cleaning as the main challenge.  

**Methods and Models:**
- Multivariate linear regression  
- Fixed effects models  
- Event study with leads/lags  
- Bartik-style instrumental variables  

**Difficulty Breakdown:**
- Data acquisition: Low (datasets are available).  
- Data cleaning: Moderate-to-high (if reconstructing from raw data).  
- Result replication: Low (directly runnable via `.Rdata` provided).  
- Technical implementation: Moderate (scripts are written in R, manageable for students).  

The appendix (**Table S1**) includes a full list of variables and descriptive statistics, essentially serving as a variable dictionary, which greatly reduces uncertainty in replication.

---

## 5. Extension Difficulty
**Rating:** Moderate-to-high  

**Extension path:** Rebuild the MSA-level panel from raw data (H-1B, ACS, patent data), then rerun baseline regressions.  

**Difficulty:** Moderate, requiring data processing and crosswalks (ZIP → CBSA), but manageable within a course project.  

**Extension value:** Demonstrates not only the ability to run code but also an understanding of data construction, adding significant depth and academic value to the course project.
