cohorts_field_of_study <- read_csv("Most-Recent-Cohorts-Field-of-Study.csv")
institutions <- cohorts_field_of_study %>%
  mutate(num_ind = ifelse(DEBT_ALL_PP_ANY_MEAN == "PrivacySuppressed", NA, as.numeric(DEBT_ALL_PP_ANY_MEAN))) %>%
  filter(CREDLEV == 3) %>%
  group_by(INSTNM) %>%
  summarize(count = sum(!is.na(num_ind))) %>%
  filter(count >= 10)

majors <- cohorts_field_of_study %>%
  mutate(num_ind = ifelse(DEBT_ALL_PP_ANY_MEAN == "PrivacySuppressed", NA, as.numeric(DEBT_ALL_PP_ANY_MEAN))) %>%
  filter(CREDLEV == 3) %>%
  group_by(CIPDESC) %>%
  summarize(count = sum(!is.na(num_ind))) %>%
  filter(count >= 10)
#
cohorts_institutions <- cohorts_field_of_study %>%
  select(INSTNM, CONTROL, CIPCODE, CIPDESC, CREDLEV, CREDDESC, starts_with("DEBT")) %>%
  filter(INSTNM %in% institutions$INSTNM, CREDLEV == 3)
#
cohorts_majors <- cohorts_field_of_study %>%
  select(INSTNM, CONTROL, CIPCODE, CIPDESC, CREDLEV, CREDDESC, starts_with("DEBT")) %>%
  filter(CIPDESC %in% majors$CIPDESC, CREDLEV == 3)

variables_to_study <- c(
  "Parent PLUS Loan Debt (all)" = "ALL_PP_ANY",
  "Parent PLUS Loan Debt Males (all)"= "MALE_PP_ANY",
  "Parent PLUS Loan Debt Not Males (all)" = "NOTMALE_PP_ANY",
  "Parent PLUS Loan Debt to Pell Recipients (all)"= "PELL_PP_ANY",
  "Parent PLUS Loan Debt to Non Pell Recipients (all)"= "NOPELL_PP_ANY",
  "Parent PLUS Loan Debt (this)"= "ALL_PP_EVAL",
  "Parent PLUS Loan Debt Males (this)" = "MALE_PP_EVAL",
  "Parent PLUS Loan Debt Not Males (this)"= "NOTMALE_PP_EVAL",
  "Parent PLUS Loan Debt to Pell Recipients (this)"= "PELL_PP_EVAL",
  "Parent PLUS Loan Debt to Non Pell Recipients (this)"= "NOPELL_PP_EVAL",
  "Stafford and Grad PLUS Loan Debt (all)" = "ALL_STGP_ANY",
  "Stafford and Grad PLUS Loan Debt Males (all)"= "MALE_STGP_ANY",
  "Stafford and Grad PLUS Loan Debt Not Males (all)"= "NOTMALE_STGP_ANY",
  "Stafford and Grad PLUS Loan Debt to Pell Recipients (all)"= "PELL_STGP_ANY",
  "Stafford and Grad PLUS Loan Debt to Non Pell Recipients (all)"= "NOPELL_STGP_ANY",
  "Stafford and Grad PLUS Loan Debt (this)"= "ALL_STGP_EVAL",
  "Stafford and Grad PLUS Loan Debt Males (this)" = "MALE_STGP_EVAL",
  "Stafford and Grad PLUS Loan Debt Not Males (this)" = "NOTMALE_STGP_EVAL",
  "Stafford and Grad PLUS Loan Debt to Pell Recipients (this)" = "PELL_STGP_EVAL",
  "Stafford and Grad PLUS Loan Debt to Non Pell Recipients (this)" = "NOPELL_STGP_EVAL"
)

save.image("read_data.RData")
