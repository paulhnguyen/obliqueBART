# Author: Paul Nguyen
# Date: July 31, 2024
# Purpose: create file to set simulation setting for one hot data
# Details: 
# Dependencies:

# model index
m = c(
  "rot_extra_trees",
  "rot_rand_forest",
  "rot_pbart",
  "rot_xgboost",
  "obart1.5a",
  "rotation_forest"
)
# dataset
d = c("banknote_authentication", "blood_transfusion", 
      "breast_cancer_diagnostic",
      "breast_cancer_original", "breast_cancer_prognostic", 
      "climate_model_crashes", "connectionist_bench_sonar", "credit_approval",
      "echocardiogram", "fertility", 
      "hepatitis", "ILPD", "ionosphere", "ozone_detection_level_1", 
      "ozone_detection_level_8", "parkinsons", "planning_relax",
      "qsar_biodegradable", "seismic_bumps", "spambase", "spectf_heart",
      "statlog_german_credit", "thoraric_surgery")

num_rota <- c(20, 40, 80, 160, 200)

# test set
s = 1:20

base_settings = expand.grid(model = m[5:6], data = d, split= s, num_rota = 200)
expanded_settings = expand.grid(model = m[1:4], data = d, split= s, num_rota = num_rota)

# save settings
settings = rbind(base_settings, expanded_settings)
