#' A subset of the \code{framingham} data
#'
#' A subset of the \code{framingham} dataset containing the following changes:
#' \itemize{
#' \item{removal of all observations where PERIOD == 2 or PERIOD == 3 (i.e. keep only PERIOD == 1)}
#' \item{removal of all observations where PREVCHD == 1 (i.e. all patients with coronary heart disease)}
#' \item{created a new variable, \code{cvd_dth} signifying an outcome of cardiovascular disease OR death (i.e. if the patient had either CVD or DEATH, this new variable is 1, otherwise 0)}
#' \item{created a new variable, \code{timeout}, which calculates the number of days from the start of the study to cardiovascular disease or death}
#' \item{created a new variable, \code{logpdays}, which is the log of \code{timeout}}
#' \item{created a new variable, \code{nhosp}, which is a simulated number of hospitalizations}
#' }
#' @format A data frame with 4240 rows and 31 variables:
#' \describe{
#' \item{RANDID}{Unique identification number for each participant.}
#' \item{SEX}{Participant sex. 1 = Male, 2 = Female.}
#' \item{TOTCHOL}{Serum Total Cholesterol (mg/dL).}
#' \item{AGE}{Age at exam (years).}
#' \item{SYSBP}{Systolic Blood Pressure (mean of last two of three measurements) (mmHg).}
#' \item{DIABP}{Diastolic Blood Pressure (mean of last two of three measurements) (mmHg).}
#' \item{CURSMOKE}{Current cigarette smoking at exam. 0 = Not current smoker, 1 = Current smoker.}
#' \item{CIGPDAY}{Number of cigarettes smoked each day. 0 = Not current smoker.}
#' \item{BMI}{Body Mass Index, weight in kilograms/height meters squared.}
#' \item{DIABETES}{Diabetic according to criteria of first exam treated or first exam with casual glucose of 200 mg/dL or more. 0 = Not a diabetic, 1 = Diabetic.}
#' \item{BPMEDS}{Use of Anti-hypertensive medication at exam. 0 = Not currently used, 1 = Current use.}
#' \item{HEARTRTE}{Heart rate (Ventricular rate) in beats/min.}
#' \item{GLUCOSE}{Casual serum glucose (mg/dL).}
#' \item{educ}{}
#' \item{PREVSTRK}{Prevalent Stroke. 0 = Free of disease, 1 = Prevalent disease.}
#' \item{PREVHYP}{Prevalent Hypertensive. Subject was defined as hypertensive if treated or if second exam at which mean systolic was >=140 mmHg or mean Diastolic >=90 mmHg. 0 = Free of disease, 1 = Prevalent disease.}
#' \item{DEATH}{Death from any cause. 0 = Did not occur during followup, 1 = Did occur during followup.}
#' \item{ANGINA}{Angina Pectoris. 0 = Did not occur during followup, 1 = Did occur during followup.}
#' \item{HOSPMI}{Hospitalized Myocardial Infarction. 0 = Did not occur during followup, 1 = Did occur during followup.}
#' \item{MI_FCHD}{Hospitalized Myocardial Infarction or Fatal Coronary Heart Disease. 0 = Did not occur during followup, 1 = Did occur during followup.}
#' \item{ANYCHD}{Angina Pectoris, Myocardial infarction (Hospitalized and silent or unrecognized), Coronary Insufficiency (Unstable Angina), or Fatal Coronary Heart Disease. 0 = Did not occur during followup, 1 = Did occur during followup.}
#' \item{STROKE}{Atherothrombotic infarction, Cerebral Embolism, Intracerebral Hemorrhage, or Subarachnoid Hemorrhage or Fatal Cerebrovascular Disease. 0 = Did not occur during followup, 1 = Did occur during followup.}
#' \item{CVD}{Myocardial infarction (Hospitalized and silent or unrecognized), Fatal Coronary Heart Disease, Atherothrombotic infarction, Cerebral Embolism, Intracerebral Hemorrhage, or Subarachnoid Hemorrhage or Fatal Cerebrovascular Disease. 0 = Did not occur during followup, 1 = Did occur during followup.}
#' \item{HYPERTEN}{Hypertensive. Defined as the first exam treated for high blood pressure or second exam in which either Systolic is 6 140 mmHg or Diastolic 6 90mmHg. 0 = Did not occur during followup, 1 = Did occur during followup.}
#' \item{cvd_dth}{Cardiovascular disease OR death. 0 = Did not occur during followup, 1 = Did occur during followup.}
#' \item{timeout}{Number of days from the start of the study to cardiovascular disease or death.}
#' \item{drop}{}
#' \item{glucoseyear6}{}
#' \item{logpdays}{Nautral log of \code{timeout}.}
#' \item{bmicat}{}
#' \item{nhosp}{Simulated number of hospitalizations over 24 months.}
#' }
#' @name cvdd
#' @docType data
#' @usage data(cvdd)
#'
NULL