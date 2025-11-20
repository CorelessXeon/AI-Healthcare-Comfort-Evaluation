# Replication Feasibility Assessment (Paper 3)

**Title**: *A comparison of political violence by left-wing, right-wing, and Islamist extremists in the United States and the world*  
**Link**: https://www.pnas.org/doi/full/10.1073/pnas.2122593119  
**Dataset link**: https://osf.io/5rhwf/files/osfstorage  

---

## 1. Influence
- Published in **PNAS** — a top interdisciplinary journal, ensuring high academic authority and broad visibility.  
- **42 citations (Crossref)** and **over 100,000 views**. Recently, attention surged again following the political assassination of Charlie Kirk, which highlighted the relevance of this 2022 paper.  
- Widely disseminated beyond academia: cited by **54 news outlets**, policy documents, and heavily shared on social media, reflecting strong societal and policy impact.  

---

## 2. Data Availability
- **Core datasets**:  
  - **PIRUS v3.2** (Profiles of Individual Radicalization in the United States): Individual-level data, including demographics, group affiliation, and violent behavior.  
  - **GTD (Global Terrorism Database)**: Event-level data (1970–2021), including attack type, location, fatalities, and organizations.  

- **Auxiliary files**:  
  - `GTD.groups.coding`: Classification of groups into left-wing, right-wing, and Islamist categories.  
  - `country.level.vars.csv`: Macro-level country variables.  
  - R scripts (`PIRUS 04 22.R`, `GDT 04.22.R`) providing cleaning and variable construction logic.  

- **Accessibility**: All included in the OSF repository, publicly downloadable, no special request required.  
- **Conclusion**: Data access is straightforward and complete.  

---

## 3. Suggested Replication Scope
- **Tables 1–3**: Descriptive statistics (demographics such as gender, education, age, and group type).  
- **Violent vs. non-violent comparisons**: Replicating group-level contrasts in extremist behaviors.  

---

## 4. Replication Difficulty (Low–Moderate)
- **Data acquisition**: Low (all datasets provided).  
- **Data cleaning**: Moderate (straightforward at the individual level, somewhat more complex when merging GTD events).  
- **Modeling methods**: Moderate (logit regression; common methods, but careful alignment with authors’ variable definitions is required).  
- **Overall difficulty**: Moderate. Core replication is feasible with low barriers, though extended analyses require more effort.  

---

## 5. Extension Difficulty (Moderate–High)
- **Extension 1: Full coverage of PIRUS + GTD**  
  - After replicating Tables 1–4 at the individual level, extend with GTD to produce event-based trend figures (e.g., yearly counts of left-wing, right-wing, and Islamist incidents).  
  - *Difficulty*: Moderate-to-high (requires cleaning and aggregation of large GTD data).  

- **Extension 2: Subsample analysis**  
  - Run separate logit regressions within left-wing, right-wing, and Islamist subsamples.  
  - *Difficulty*: Moderate (models can reuse existing scripts with subgroup filtering).  

---
