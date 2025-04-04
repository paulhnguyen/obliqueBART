# Author: Paul Nguyen
# Date: July 31, 2024
# Purpose: create file to set simulation setting for one hot data
# Details: 
# Dependencies:

# model index
m = c(
  "xgboost",
  "extra_trees",
  "rand_forest"
)
# dataset
d = c("banknote_authentication", "blood_transfusion", "breast_cancer_diagnostic",
      "breast_cancer_original", "breast_cancer_prognostic", 
      "climate_model_crashes", "connectionist_bench_sonar", "credit_approval",
      "echocardiogram", "fertility", 
      "hepatitis", "ILPD", "ionosphere", "ozone_detection_level_1", 
      "ozone_detection_level_8", "parkinsons", "planning_relax",
      "qsar_biodegradable", "seismic_bumps", "spambase", "spectf_heart",
      "statlog_german_credit")

# test set
s = 1:20

# save settings
settings = expand.grid(model = m, data = d, split = s)
