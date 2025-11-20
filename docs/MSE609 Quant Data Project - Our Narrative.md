# **🌿 How to Present the Entire Project (Full Narrative)**

# 

[**🌿 How to Present the Entire Project (Full Narrative)	3**](#🌿-how-to-present-the-entire-project-\(full-narrative\))

[1\. What Our Project Is About	3](#1.-what-our-project-is-about)

[2\. What Data We Used	3](#2.-what-data-we-used)

[3\. The Two Key Data Cleaning Approaches	4](#3.-the-two-key-data-cleaning-approaches)

[A. Available-case filtering	4](#a.-available-case-filtering)

[B. Strict complete-case filtering (the main one)	4](#b.-strict-complete-case-filtering-\(the-main-one\))

[4\. What the Pipeline Does (Conceptually)	5](#4.-what-the-pipeline-does-\(conceptually\))

[Step 1 — Data Cleaning	5](#step-1-—-data-cleaning)

[Step 2 — Descriptive Statistics (Table 1\)	5](#step-2-—-descriptive-statistics-\(table-1\))

[Step 3 — Regression Models (Appendix A1)	5](#step-3-—-regression-models-\(appendix-a1\))

[Step 4 — Exporting Appendix Table A1	5](#step-4-—-exporting-appendix-table-a1)

[Step 5 — Regression-Based 3D Plots	6](#step-5-—-regression-based-3d-plots)

[5\. Our Completed Outputs (What We Actually Produced)	6](#5.-our-completed-outputs-\(what-we-actually-produced\))

[✔ Cleaned dataset (strict \+ available-case versions: Kai & Gary)	6](#✔-cleaned-dataset-\(strict-+-available-case-versions:-kai-&-gary\))

[✔ Reproduced descriptive Table 1 (Kai & Gary)	6](#✔-reproduced-descriptive-table-1-\(kai-&-gary\))

[✔ Reproduced Appendix A1 (Leila, ordinal logistic regression)	6](#✔-reproduced-appendix-a1-\(leila,-ordinal-logistic-regression\))

[✔ Gap documentation (Leila, differences between our replication and the paper)	6](#✔-gap-documentation-\(leila,-differences-between-our-replication-and-the-paper\))

[✔ Midterm Report (Gary \+ Leila contributions)	6](#✔-midterm-report-\(gary-+-leila-contributions\))

[✔ Full project README \+ run\_all.R (Aymen \+ Leila)	6](#✔-full-project-readme-+-run_all.r-\(aymen-+-leila\))

[6\. What Our Replicated Results Show (Narrative)	7](#6.-what-our-replicated-results-show-\(narrative\))

[Q40 – Knowledge about AI	7](#q40-–-knowledge-about-ai)

[Q41 – Comfort with AI in healthcare	7](#q41-–-comfort-with-ai-in-healthcare)

[Q42 – Comfort with AI research with consent	7](#q42-–-comfort-with-ai-research-with-consent)

[Q43 – Comfort with AI research without consent	8](#q43-–-comfort-with-ai-research-without-consent)

[7\. Our Extension (Planned)	8](#7.-our-extension-\(planned\))

[A. Predicting comfort using machine learning	8](#a.-predicting-comfort-using-machine-learning)

[B. Building interactive dashboards in R Shiny	8](#b.-building-interactive-dashboards-in-r-shiny)

[C. Adding more demographic variables	8](#c.-adding-more-demographic-variables)

[D. Comparing 2021 data with 2024 CDHS dataset	8](#d.-comparing-2021-data-with-2024-cdhs-dataset)

[8\. For the Presentation — Simple Structure	8](#8.-for-the-presentation-—-simple-structure)

[1\. What the paper studied & why it matters	8](#1.-what-the-paper-studied-&-why-it-matters)

[2\. What data we used	9](#2.-what-data-we-used-1)

[3\. Our pipeline (explain like a story)	9](#3.-our-pipeline-\(explain-like-a-story\))

[4\. What we replicated	9](#4.-what-we-replicated)

[5\. What we found	9](#5.-what-we-found)

[6\. What’s next	9](#6.-what’s-next)

[7\. What each member contributed	9](#7.-what-each-member-contributed)

# 

# **🌿 How to Present the Entire Project (Full Narrative)** {#🌿-how-to-present-the-entire-project-(full-narrative)}

(*For professor \+ class. No code. Just concepts.*)

---

## **1\. What Our Project Is About** {#1.-what-our-project-is-about}

Our team replicated a published research paper called:  
 **“Knowledge is not all you need for comfort in use of AI in healthcare.”**

The goal of the replication was:

* to **take the same dataset** the authors used,

* **reproduce their descriptive statistics**,

* **reproduce their regression results (Appendix A1)**,

* **reproduce or approximate their 3D regression figures**, and

* optionally propose an **extension** to go beyond the paper.

We worked as a team of 6, each contributing differently to the pipeline, documentation, and replication accuracy.

---

## **2\. What Data We Used** {#2.-what-data-we-used}

The dataset is public on Borealis Dataverse:  
 **ATS 2021 Survey Dataset (N \= 12,052)**

It includes Canadians’ responses about:

* how knowledgeable they feel about AI

* how comfortable they are with AI in healthcare

* how comfortable they are sharing health data

* with or without consent

These appear in the variables **Q40–Q43**, all on a **1 to 4 Likert scale**.

We also used demographic variables:

* Age group

* Gender

* Household income

* Education level

These were our predictors for all models.

---

## **3\. The Two Key Data Cleaning Approaches** {#3.-the-two-key-data-cleaning-approaches}

We applied the same filtering strategies used in the paper:

### **A. Available-case filtering** {#a.-available-case-filtering}

* keep all valid demographic responses

* allow missing response on Q40–Q43

* every question has a different N

### **B. Strict complete-case filtering (the main one)** {#b.-strict-complete-case-filtering-(the-main-one)}

* keep only respondents with **valid answers**  
   for **all 4 demographic \+ all 4 dependent variables**

* final sample: **N \= 9,198**, same as the paper

This gave us a clean dataset used for all replication.

This dataset is saved as:

`data_clean/clean_data_strict.rds`  

---

## **4\. What the Pipeline Does (Conceptually)** {#4.-what-the-pipeline-does-(conceptually)}

Our project relies on a **pipeline**—a sequence of steps that turns raw data into the final report figures and tables.

### **Step 1 — Data Cleaning** {#step-1-—-data-cleaning}

Script: `01_data_cleaning.R`  
 Output: cleaned data files (`clean_data*.rds`)

### **Step 2 — Descriptive Statistics (Table 1\)** {#step-2-—-descriptive-statistics-(table-1)}

Script: `02_descriptives_table1_strict.R`  
 Output: `output/table1_combined_strict.html`

This reproduces the demographic breakdown shown in the paper’s Table 1\.

### **Step 3 — Regression Models (Appendix A1)** {#step-3-—-regression-models-(appendix-a1)}

Script: `03_models_Q40_to_Q43.R`  
 Output: four model result files, plus a saved model object.

These model:

* Q40 Knowledge

* Q41 Comfort with AI

* Q42 Comfort with AI research *with* consent

* Q43 Comfort with AI research *without* consent

The predictors are age, gender, income, education.

### **Step 4 — Exporting Appendix Table A1** {#step-4-—-exporting-appendix-table-a1}

Script: `04_export_regression_tables.R`  
 Output:  
 `output/tableA1_combined.html`

This table contains:

* Odds ratios

* Confidence intervals

* p-values

* Sample sizes

We matched the structure of the published Appendix A1.

### **Step 5 — Regression-Based 3D Plots** {#step-5-—-regression-based-3d-plots}

Script: `05_plots_Q40_to_Q43.R`  
 Output (when done):  
 `output/ai_comfort_surface.html`

These plots show how comfort changes as demographic variables change.

---

## **5\. Our Completed Outputs (What We Actually Produced)** {#5.-our-completed-outputs-(what-we-actually-produced)}

So far, our team completed:

### **✔ Cleaned dataset (strict \+ available-case versions: Kai & Gary)** {#✔-cleaned-dataset-(strict-+-available-case-versions:-kai-&-gary)}

### **✔ Reproduced descriptive Table 1 (Kai & Gary)** {#✔-reproduced-descriptive-table-1-(kai-&-gary)}

### **✔ Reproduced Appendix A1 (Leila, ordinal logistic regression)** {#✔-reproduced-appendix-a1-(leila,-ordinal-logistic-regression)}

### **✔ Gap documentation (Leila, differences between our replication and the paper)** {#✔-gap-documentation-(leila,-differences-between-our-replication-and-the-paper)}

### **✔ Midterm Report (Gary \+ Leila contributions)** {#✔-midterm-report-(gary-+-leila-contributions)}

### **✔ Full project README \+ run\_all.R (Aymen \+ Leila)** {#✔-full-project-readme-+-run_all.r-(aymen-+-leila)}

Next in progress:

* 3D regression plots

* Extension analysis

* Final report

* Final presentation

---

## **6\. What Our Replicated Results Show (Narrative)** {#6.-what-our-replicated-results-show-(narrative)}

### **Q40 – Knowledge about AI** {#q40-–-knowledge-about-ai}

* Older people know less about AI.

* Females report lower knowledge than males.

* Higher income → more knowledge

* Higher education → much higher knowledge

### **Q41 – Comfort with AI in healthcare** {#q41-–-comfort-with-ai-in-healthcare}

* Comfort decreases with age

* Females less comfortable than males

* High incomes and high education groups more comfortable

### **Q42 – Comfort with AI research with consent** {#q42-–-comfort-with-ai-research-with-consent}

Similar pattern:

* age ↓

* female ↓

* income ↑

* education ↑

### **Q43 – Comfort with AI research without consent** {#q43-–-comfort-with-ai-research-without-consent}

Same direction but stronger effects:

* Age sharply decreases comfort

* Gender gap is large

* Education and income strongly increase comfort

These patterns match the general findings in the paper.

---

## **7\. Our Extension (Planned)** {#7.-our-extension-(planned)}

We have several directions, but the team is considering:

### **A. Predicting comfort using machine learning** {#a.-predicting-comfort-using-machine-learning}

(e.g., random forest, SVM, gradient boosting)

### **B. Building interactive dashboards in R Shiny** {#b.-building-interactive-dashboards-in-r-shiny}

### **C. Adding more demographic variables** {#c.-adding-more-demographic-variables}

(ethnicity, region, etc.)

### **D. Comparing 2021 data with 2024 CDHS dataset** {#d.-comparing-2021-data-with-2024-cdhs-dataset}

The extension is small but meaningful.

---

## **8\. For the Presentation — Simple Structure** {#8.-for-the-presentation-—-simple-structure}

### **1\. What the paper studied & why it matters** {#1.-what-the-paper-studied-&-why-it-matters}

AI trust, healthcare, public understanding.

### **2\. What data we used** {#2.-what-data-we-used-1}

ATS 2021, Q40–Q43, demographics.

### **3\. Our pipeline (explain like a story)** {#3.-our-pipeline-(explain-like-a-story)}

Raw data → clean → descriptives → regression → tables → plots.

### **4\. What we replicated** {#4.-what-we-replicated}

Table 1, Appendix A1, patterns.

### **5\. What we found** {#5.-what-we-found}

The demographic patterns.

### **6\. What’s next** {#6.-what’s-next}

3D plots \+ extension work.

### **7\. What each member contributed** {#7.-what-each-member-contributed}

---

