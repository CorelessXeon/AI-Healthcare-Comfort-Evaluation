##############################################
# 05_different_dataset.R
# Purpose:
#   clean datasets of 2024 and 2023 surveys
# Input:
#   data/data_raw/CDHS2023_Dataset_Dataverse-posting.sav
#   data/data_raw/Infoway CDHS 2024 SPSS Raw Data_for Dataverse.sav
# Output:
#   data/cleaned_dataset_2023.rds
#   data/cleaned_dataset_2024.rds
#############################################
# nolint start


# the pipelines are in common utils
source("R/utils/common_utils.R")

process_different_dataset(dataset_path= here("data", "data_raw", "CDHS2023_Dataset_Dataverse-posting.sav"),
                          question_mapping = c(
                            "q40_un" = "q31",
                            "q41_un" = "q32",
                            "q42_un" = "q33",
                            "q43_un" = "q34",
                            "age_new" = "q1_age_cat", #note age is 16-24 then 25-34 then 35-54 then 55-64 then 65+
                            "gender" = "q5_recoded", # note gender is 1 for female, 2 for male 3 for other
                            "q66_un" = "q48", # education is 
                            # 1 no certificate, diplona or degree, 
                            # 2 Secondary (high) school diploma or equivalency certificate,
                            # 3 Apprenticeship or trades certificate or diploma, College,
                            # 4 CEGEP or other non-university certificate or diploma,
                            # 5 University certificate or diploma below bachelor level,
                            # 6 University certificate, diploma or degree at bachelor level or above
                            "q55_un" = "q40"),
                            gender_levels = c("female", "male", "other"),
                            income_levels = c("< $50,000", "$50,000-$60,000", "$60,000-$70,000", "$70,000-$80,000", "$80,000-$90,000", "$100,000-$150,000", ">$150,000"),
                            education_levels = c("No certificate, diploma or degree",
                                                 "Secondary (high) school diploma or equivalency certificate",
                                                 "Apprenticeship or trades certificate or diploma",
                                                 "College, CEGEP or other non-university certificate or diploma",
                                                 "University certificate or diploma below bachelor level",
                                                 "University certificate, diploma or degree at bachelor level or above"),
                            age_levels = c("16–24 years", "25–34 years", "35–44 years", "45–54 years", "55–64 years", "65+ years"),
                            filename = "cleaned_dataset_2023")


# note that answers are out of 5 not 4 here
process_different_dataset(dataset_path= here("data", "data_raw", "Infoway CDHS 2024 SPSS Raw Data_for Dataverse.sav"), 
                          question_mapping = c(
                            "q40_un" = "q28_a1", 
                            "q41_un" = "q28_a3",
                            "q42_un" = "q28_a5",
                            "q43_un" = "q28_a6", # note this is totally different from q43: it says The use of AI by health care providers improves my confidence in my disease diagnosis.
                            "age_new" = "q2_age_groups", #note age is 16-24 then 25-34 then 35-54 then 55-64 then 65+
                            "gender" = "q5b_gender", # note gender is 1 for female, 2 for male 3 for other
                            "q66_un" = "q46", # education is 1 no certificate, diplona or degree,
                            # 2 Secondary (high) school diploma or equivalency certificate,
                            # 3 Apprenticeship or trades certificate or diploma, College,
                            # 4 CEGEP or other non-university certificate or diploma,
                            # 5 University certificate or diploma below bachelor level,
                            # 6 University certificate, diploma or degree at bachelor level or above
                            "q55_un" = "q38"),
                            gender_levels = c("female", "male", "other"),
                            income_levels = c("< $40,000", "$40,000-$59,000", "$60,000-$79,000", "$80,000-$99,000", "$100,000-$149,000", ">$150,000"),
                            education_levels = c("No certificate, diploma or degree",
                                                 "Secondary (high) school diploma or equivalency certificate",
                                                 "Apprenticeship or trades certificate or diploma",
                                                 "College, CEGEP or other non-university certificate or diploma",
                                                 "University certificate or diploma below bachelor level",
                                                 "University certificate, diploma or degree at bachelor level",
                                                 "University certificate, diploma or degree at the graduate level"),
                            age_levels = c("16–17 years", "18–24 years", "25–34 years", "35–44 years", "45–54 years", "55–64 years", "65–74 years", "75+ years"),
                            filename = "cleaned_dataset_2024",
                            age_adjust = 1,
                            factorize_to = 5)

                            # increment age category by 1
                            # answers out of 5 not 4
