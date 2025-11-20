# Speaker Notes for Final Project Presentation

------------------------------------------------------------------------

## Page 1 (Title)

Hello everyone,\
We are **Group 11**, and the paper we selected for our research project is:\
**"Knowledge is not all you need for comfort in use of AI in healthcare."**\
Our team members are: **Group 11 Kaichen Sun, Deep Agam, Aymen Mehrez, Zayn Dhillon, Wentao Zang, & Leila Safari**.

------------------------------------------------------------------------

## Page 2 (Paper Info)

The paper we’ve chosen for our final project is *“Knowledge is not all you need for comfort in using AI in healthcare,”* a survey research paper by Li et al.\
It was recently published in **Public Health**, an established international, multi-disciplinary, and peer-reviewed academic journal.\
This timely study, which explores the intersection of healthcare and AI adoption, has already been cited by three other publications in a similar field.

------------------------------------------------------------------------

## Page 3 (Data Source)

Here is an overview of our data source, which we obtained from the **2021 Canadian Digital Health Survey**. Since it is public, we can access it freely.

The dataset contains approximately **12,052 respondents from Canada**, each providing four basic demographic characteristics: **gender, age, income, and education level**.

In addition to these demographics, the survey included four core questions related to **AI in healthcare**:\
1. **Knowledge about AI** – self-reported familiarity with artificial intelligence.\
2. **Comfort with AI in healthcare** – level of comfort with AI being used as a tool in healthcare.\
3. **Comfort with scientists using personal health data with informed consent.**\
4. **Comfort with scientists using de-identified health data without consent.**

------------------------------------------------------------------------

## Page 4 (Scope of Replication)

Our replication will focus on:\
- **Table 1** – Descriptive statistics. \
- **Table A1 (Appendix)** – Ordinal logistic regression results for Question 1.\
- **Figures 1–4** – Multivariate polynomial regressions.

------------------------------------------------------------------------

## Page 5 (Feasibility Check)

Here’s an overview of our feasibility check for the selected paper:

-   We’ve developed a rough plan for cleaning the raw data and making it usable for our replication project.\
-   We’ve identified the exact variables used by the authors: **AI knowledge and comfort** (measured using a Likert scale), and demographics such as **age, gender, education, and income**.\
-   We plan to use the same methods as the paper, employing **R functions (e.g., `polr`, `clm`)** to replicate ordinal logistic regressions and **Excel** for descriptive statistics.\
-   In terms of difficulty, reproducing the **main tables and appendix** will be relatively straightforward. However, replicating the **figures** (polynomial DOE regression plots) may present a greater challenge.
