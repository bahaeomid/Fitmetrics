# ────────────────────────────────────────────────
###This is essentially "fitness R2.6 - Notion (pre-Shiny implementation)" file with the following changes to suit shiny:
#### a) The Notion Pre-Processing section stripped off and moved to shiny app.R so that DBs don't get re-fetched every single time the core_processing.R is called from Shiny app.R
#### b) The hard coded selected_identity = "Bahae Omid" in core_processing.R has been commented out (as this is now set by user in Shiny app.R)
#### c) the remainder of the whole core_processing.R script has been wrapped in process_all function for shiny to be able to give it two inputs:
##### c-1: db_list : after fetching Notion DBs from Notion inside app.R
##### c-2: selected_identity: as set  by user in app.R (default = Bahae Omid)
# ────────────────────────────────────────────────


#a wrapper for shiny app.R to call to override the selected_identity and re-run the script per user interaction
process_all <- function(db_list, selected_identity = "Bahae Omid") {
  
  # assign DBs from the db_list and run the rest of the script
  list2env(db_list, envir = environment())

# ────────────────────────────────────────────────────────────────────────
# --------------------------BASE COMPUTATIONS-----------------------------
# ────────────────────────────────────────────────────────────────────────

# ────────────────────────────────
# JOIN PRIMARY DATA 
# ────────────────────────────────
weekly_data_raw <- db_list$Training_Weekly_Log$tibble %>%
  left_join(db_list$Training_Periodization$tibble, by = "Period_Name") %>%
  left_join(db_list$Training_Profiles$tibble, by = c("Profile" = "Name")) %>%
  left_join(db_list$Training_Parameters$tibble, by = c("Preset_Goal_Template"))
  
# ────────────────────────────────
# ADD IN TRAINING VOLUME DATA (BY PROGRAM + MUSCLE)
# ────────────────────────────────

# 1. Clean programs table
programs_clean <- db_list$Training_Programs$tibble %>%
  filter(!str_detect(tolower(Exercise), "dummy")) %>%
  mutate(
    Primary_Muscle_Group = str_trim(Primary_Muscle_Group),
    Sets = as.numeric(Sets),
    Reps = as.numeric(Reps)
  )

# 2. Identify all programs appearing in weekly_data_raw
program_list <- weekly_data_raw %>%
  distinct(Program) %>% 
  pull(Program)

# 3. Filter programs_clean so we only compute volume for relevant programs
programs_clean_filtered <- programs_clean %>%
  filter(Program %in% program_list)

# 4. Compute training volume BY program AND muscle group
training_volume <- programs_clean_filtered %>%
  group_by(Program, Primary_Muscle_Group) %>%
  summarise(
    Weekly_Training_Volume = sum(Sets * Reps, na.rm = TRUE),
    Weekly_Training_Sets = sum(Sets, na.rm = TRUE),
    .groups = "drop"
  )

# 5. Pivot wider so each program has one row, with muscle-specific columns
training_volume_wide <- training_volume %>%
  pivot_wider(
    names_from = Primary_Muscle_Group,
    values_from = c(Weekly_Training_Volume, Weekly_Training_Sets),
    values_fill = 0
  )

# 6. LEFT JOIN program-level training volume back to weekly_data_raw
weekly_data_raw <- weekly_data_raw %>%
  left_join(training_volume_wide, by = "Program")

# ────────────────────────────────
# FILTER FOR IDENTITY AND ACTIVE PROFILE 
# ────────────────────────────────
#set by user
#selected_identity <- "Bahae Omid"  #now set by user in shiny app

# Filter to only entries belonging to this identity/person
weekly_data <- weekly_data_raw %>% 
  filter(Identity == selected_identity)

#Auto detect active profile for the identity selected by the user (will be used for relevant sections later)
active_profile <- weekly_data %>%
  arrange(desc(Week_Start)) %>%
  slice(1) %>%
  pull(Profile)

# Apply default sorting & grouping (sorting & grouping is carried over to the rest of the code unless explicitly undone)
weekly_data <- weekly_data %>%
  arrange(Profile, Period_Name, Week_Start) %>%  
  group_by(Profile, Period_Name)  

# ────────────────────────────────
# HELPER FUNCTIONS
# ────────────────────────────────
#BMR Calc
calc_bmr <- function(weight, height, age, gender) {
  10 * weight + 6.25 * height - 5 * age + ifelse(gender == "Male", 5, -161)
}

#Activity Factor calc to support TDEE later on
activity_factor <- function(steps, sessions_per_week, session_duration_min) {
  
  # ---------- 1. Base from steps (NEAT) ----------
  base <- dplyr::case_when(
    steps < 5000           ~ 1.375,   # mostly sedentary
    steps < 7500           ~ 1.50,    # light walking / activity
    steps < 10000          ~ 1.60,    # moderate activity
    TRUE                   ~ 1.725    # highly active
  )
  
  # ---------- 2. Training MET-min ----------
  # Resistance training intensity: 3.5 MET (realistic for moderate-heavy lifting)
  MET_value <- 3.5
  weekly_MET_min <- sessions_per_week * session_duration_min * MET_value
  
  # ---------- 3. Training bump ----------
  # Based on weekly MET-min thresholds used by elite coaches
  training_bump <- dplyr::case_when(
    weekly_MET_min < 200       ~ 0.00,  # negligible
    weekly_MET_min < 400       ~ 0.05,  # light/moderate
    weekly_MET_min < 600       ~ 0.10,  # moderate/high
    TRUE                        ~ 0.15  # very high volume
  )
  
  # ---------- 4. Total activity factor ----------
  AF <- base + training_bump
  
  # Cap realistic upper bound
  AF <- min(AF, 1.9)
  AF
}


#Helper:rounding
r <- function(x, digits = 2) ifelse(is.na(x), NA, round(x, digits))

# ────────────────────────────────
# PHYSIOLOGY & INITIAL DEVIATIONS
# ────────────────────────────────
weekly_data <- weekly_data %>%
  
  mutate(
    #Unit Conversion (age & height)
    Age = year(Sys.Date()) - year(DOB),
    Height_m = Height_cm / 100,
    
    # Baseline physiology
    BMR = calc_bmr(Start_Weight_kg, Height_cm, Age, Gender),
    Activity_Factor_Target = activity_factor(Baseline_Daily_Steps, Target_Sessions_per_wk, Target_Session_Duration_min),
    
    # Target TDEE and calorie intake
    Baseline_Period_TDEE = BMR * Activity_Factor_Target,
    Target_Calorie_Intake_for_Period = Baseline_Period_TDEE * (1 + Calorie_Adjustment_perc / 100),
    
    # Deviations (default direction: ideal/baseline/target minus observed/actual/measured)
    Calorie_Deviation = Target_Calorie_Intake_for_Period - Observed_Avg_Daily_Calorie_Intake,
    Sleep_Deviation_hr = Baseline_Sleep_hr - Observed_Avg_Sleep_hr,
    Steps_Deviation = Baseline_Daily_Steps - Observed_Avg_Steps,
    
    # Distance to Ideals (default direction: ideal/baseline/target minus observed/actual/measured)
    BF_Distance_perc = Ideal_BF_perc - Observed_Avg_BF_perc, #how far you are currently from ideal BF%
    Waist_Distance_in = Ideal_Waist_in - Observed_Waist_in, #how far you are currently from ideal waist

    Snapshot_Date = Sys.Date()
  )

# ────────────────────────────────
# LBM & LBM-BASED IDEAL WEIGHT
# ────────────────────────────────# Helper: FFMI Score bands
classify_ffmi <- function(ffmi, gender) {
  
  gender <- tolower(gender)
  
  dplyr::case_when(
    
    # MALE
    gender == "male"   & ffmi < 18 ~ "Below average muscularity",
    gender == "male"   & ffmi >= 18 & ffmi < 20 ~ "Average trained",
    gender == "male"   & ffmi >= 20 & ffmi < 21 ~ "Strongly trained",
    gender == "male"   & ffmi >= 21 & ffmi < 22 ~ "Advanced natural",
    gender == "male"   & ffmi >= 22 & ffmi < 23 ~ "Very advanced natural",
    gender == "male"   & ffmi >= 23 & ffmi < 24 ~ "Elite natural",
    gender == "male"   & ffmi >= 24 & ffmi < 25 ~ "Near-natural limit",
    gender == "male"   & ffmi >= 25 ~ "Above natural limit (PED likely)",
    
    # FEMALE
    gender == "female" & ffmi < 14 ~ "Below average muscularity",
    gender == "female" & ffmi >= 14 & ffmi < 16 ~ "Average trained",
    gender == "female" & ffmi >= 16 & ffmi < 17 ~ "Strongly trained",
    gender == "female" & ffmi >= 17 & ffmi < 18 ~ "Advanced natural",
    gender == "female" & ffmi >= 18 & ffmi < 19 ~ "Very advanced natural",
    gender == "female" & ffmi >= 19 & ffmi < 20 ~ "Elite natural",
    gender == "female" & ffmi >= 20 & ffmi < 21 ~ "Near-natural limit",
    gender == "female" & ffmi >= 21 ~ "Above natural limit (PED likely)",
    
    TRUE ~ "Unknown"
  )
}

# --- Calculate Observed LBM ---
weekly_data <- weekly_data %>%
  mutate(
    Observed_LBM_Raw_kg = Observed_Avg_Weight_kg * (1 - Observed_Avg_BF_perc / 100),
    
    # --- Smoothed LBM using 3-week rolling average ---
    Observed_LBM_Smoothed_kg = as.numeric(
      zoo::rollapply(
        Observed_LBM_Raw_kg,
        width = 3,
        FUN = function(x) mean(x, na.rm = TRUE),
        align = "right",
        partial = TRUE
      )
    ),
    
    # Observed FFMI
    Observed_FFMI = Observed_LBM_Smoothed_kg / (Height_m^2),
    Observed_FFMI_band = classify_ffmi(Observed_FFMI,Gender),
    FFMI_Distance = Ideal_FFMI - Observed_FFMI, #how far you are currently from ideal FFMI
  )

# --- Ideal Weight Based on LBM using FFMI formula ---
weekly_data <- weekly_data %>%
  mutate(
    Ideal_FFMI_band = classify_ffmi(Ideal_FFMI,Gender),
    Ideal_LBM_kg = Ideal_FFMI * (Height_m^2),  #Based on FFMI formula
    Ideal_Weight_FFMI = Ideal_LBM_kg / (1 - Ideal_BF_perc / 100),
    Weight_Distance_kg = Ideal_Weight_FFMI - Observed_Avg_Weight_kg, #how far you are currently from ideal weight
  )

# ────────────────────────────────
# WEIGHT PROGRESSION & WEEKS TO REACH IDEAL WEIGHT
# ─────────────────────────────────
weekly_data <- weekly_data %>%
  
  mutate(
    # Weekly weight change (raw delta)
    ΔWeight_per_Week_raw = Observed_Avg_Weight_kg - lag(
      Observed_Avg_Weight_kg,
      default = first(Observed_Avg_Weight_kg)
    ),
    
    # Smoothed 3-week rolling average (to reduce fluctuation and provide a more realistic time projection)
    ΔWeight_per_Week_smoothed = as.numeric(
      zoo::rollapply(
        ΔWeight_per_Week_raw,
        width = 3,
        FUN = function(x) mean(x, na.rm = TRUE),
        align = "right",
        partial = TRUE
      )
    ),
    
    # Weeks to ideal weight — only if trend is in the right direction (i.e. bulk & gained, or cut & lost) 
    Weeks_to_Ideal_Weight = case_when(
      abs(ΔWeight_per_Week_smoothed) >= 0.01 & (Ideal_Weight_FFMI - Observed_Avg_Weight_kg) * ΔWeight_per_Week_smoothed > 0 ~  # epsilon 0.01  prevents weirdly large projections when the smoothed delta is tiny.
        abs(Ideal_Weight_FFMI - Observed_Avg_Weight_kg) / abs(ΔWeight_per_Week_smoothed),
      TRUE ~ NA_real_
    ),
    
    # Flag the direction
    Weeks_to_Ideal_Flag = if_else(
      is.na(Weeks_to_Ideal_Weight),
      "Opposite direction / stalled",
      "On track"
    )
  ) 

# ────────────────────────────────
# TDEE COMPUTATIONS
# ─────────────────────────────────
weekly_data <- weekly_data %>%
  mutate(
    # Observed TDEE (raw)
    Observed_TDEE_Raw = Observed_Avg_Daily_Calorie_Intake - 7700 * (ΔWeight_per_Week_raw / 7),
    
    # Observed TDEE (raw)
    Observed_TDEE_Smoothed = Observed_Avg_Daily_Calorie_Intake - 7700 * (ΔWeight_per_Week_smoothed / 7),
    
    # TDEE deviations (raw & smoothed)
    TDEE_Deviation_Raw      = Baseline_Period_TDEE - Observed_TDEE_Raw,
    TDEE_Deviation_Smoothed = Baseline_Period_TDEE - Observed_TDEE_Smoothed
  )

# ────────────────────────────────
# MACRO TARGETS
# ────────────────────────────────
#Helper: lb to kg conversion
lb_per_kg <- 2.20462

#based on preset parameters in the Training Parameters db
weekly_data <- weekly_data %>%
  mutate(
    Target_Protein_Intake_for_Period = Protein_g_lb * Start_Weight_kg * lb_per_kg,
    Target_Carbs_Intake_TD_for_Period = Carbs_g_lb_TD * Start_Weight_kg * lb_per_kg,
    Target_Carbs_Intake_NTD_for_Period = Carbs_g_lb_NTD * Start_Weight_kg * lb_per_kg,
    Target_Carbs_Intake_Avg_for_Period = (Target_Carbs_Intake_TD_for_Period + Target_Carbs_Intake_NTD_for_Period) / 2,
    Target_Fat_Intake_for_Period = (Target_Calorie_Intake_for_Period - 
                                      (Target_Protein_Intake_for_Period * 4 + Target_Carbs_Intake_Avg_for_Period * 4)) / 9
  )

# Reference macros for sanity check (based on rule of thumb of 12-13g per lb of body weight for cut, and 17-18g for bulk)
weekly_data <- weekly_data %>%
  mutate(
    Reference_Calorie_Intake_Bulk_Lower = 17 * Start_Weight_kg * lb_per_kg,
    Reference_Calorie_Intake_Bulk_Upper = 18 * Start_Weight_kg * lb_per_kg,
    Reference_Calorie_Intake_Cut_Lower  = 12 * Start_Weight_kg * lb_per_kg,
    Reference_Calorie_Intake_Cut_Upper  = 13 * Start_Weight_kg * lb_per_kg
  )

# ────────────────────────────────
# COMPLIANCE INSIGHTS
# ────────────────────────────────
weekly_data <- weekly_data %>%
  mutate(
    # More Deviations (Behavioral)
    Protein_Deviation_g = Target_Protein_Intake_for_Period - Observed_Avg_Daily_Protein_Intake,
    Session_Deviation = Target_Sessions_per_wk - Observed_Sessions,
    Duration_Deviation = Target_Session_Duration_min - Observed_Session_Duration_min,
    
    # Reference range flags (comparing to rule of thumb benchmark calculated above)
    Out_Of_Reference_Range = case_when(
      str_detect(Goal_Type, "Bulk") &
        (Target_Calorie_Intake_for_Period < Reference_Calorie_Intake_Bulk_Lower |
           Target_Calorie_Intake_for_Period > Reference_Calorie_Intake_Bulk_Upper) ~ TRUE,
      str_detect(Goal_Type, "Cut") &
        (Target_Calorie_Intake_for_Period < Reference_Calorie_Intake_Cut_Lower |
           Target_Calorie_Intake_for_Period > Reference_Calorie_Intake_Cut_Upper) ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # ---------- Short-term (Period) Consistency check ----------
    # Direction check vs period goal
    Weight_Direction_OK = case_when(
      str_detect(Goal_Type, "Bulk") & ΔWeight_per_Week_raw > 0 ~ TRUE,
      str_detect(Goal_Type, "Cut") & ΔWeight_per_Week_raw < 0 ~ TRUE,
      str_detect(Goal_Type, "Maintenance|Recomp") & abs(ΔWeight_per_Week_raw) <= 0.2 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # ---------- Long-term (Ideal) Consistency check ----------
    At_Ideal = if_else(!is.na(Weight_Distance_kg) & abs(Weight_Distance_kg) <= 0.2, TRUE, FALSE),
    
    # What does the long-term direction *need* to be? (TRUE = needs to gain, FALSE = needs to lose, NA = at ideal)
    LongTerm_Needs_Gain = case_when(
      is.na(Weight_Distance_kg) ~ NA,
      Weight_Distance_kg > 0  ~ TRUE,   # Ideal > Observed -> needs to gain
      Weight_Distance_kg < 0  ~ FALSE,  # Ideal < Observed -> needs to lose
      TRUE ~ NA
    ),
    
    # Use the smoothed 3-week rate to test long-term trend alignment (NA-safe)
    LongTerm_Trend_Sign = sign(ΔWeight_per_Week_smoothed),  # -1,0,1 or NA
    
    # LongTerm_Direction_OK: does the smoothed trend move in the direction needed to approach the ideal?
    LongTerm_Direction_OK = case_when(
      At_Ideal ~ TRUE,  # already at ideal => trivially OK
      is.na(LongTerm_Needs_Gain) | is.na(LongTerm_Trend_Sign) ~ NA, # unknown
      LongTerm_Needs_Gain == TRUE  & LongTerm_Trend_Sign > 0  ~ TRUE,  # needs gain & positive trend
      LongTerm_Needs_Gain == FALSE & LongTerm_Trend_Sign < 0  ~ TRUE,  # needs loss & negative trend
      TRUE ~ FALSE
    ),
    
    # For projection gating: require smoothed delta to have same sign as needed and be non-trivial
    Projection_Allowed = case_when(
      At_Ideal ~ FALSE,
      is.na(Weeks_to_Ideal_Weight) ~ FALSE,
      is.na(ΔWeight_per_Week_smoothed) ~ FALSE,
      abs(ΔWeight_per_Week_smoothed) < 0.01 ~ FALSE,        # epsilon to avoid huge projections
      !is.na(LongTerm_Needs_Gain) & ((LongTerm_Needs_Gain & ΔWeight_per_Week_smoothed > 0) |
                                       (!LongTerm_Needs_Gain & ΔWeight_per_Week_smoothed < 0)) ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # ─────────────────────────────
    # 1) PRIMARY INSIGHT
    # ─────────────────────────────
    Primary_Insight = purrr::pmap_chr(
      list(
        ΔWeight_per_Week_raw,
        ΔWeight_per_Week_smoothed,
        Goal_Type,
        Observed_Avg_Weight_kg,
        Ideal_Weight_FFMI,
        Weight_Direction_OK,
        Weeks_to_Ideal_Weight,
        Weight_Distance_kg,
        At_Ideal,
        LongTerm_Direction_OK,
        Projection_Allowed
      ),
      function(delta_raw, delta_smooth, goal, current_wt, ideal_wt,
               direction_ok, weeks_to_ideal, dist_raw, at_ideal,
               longterm_ok, proj_allowed) {
        
        # friendly numbers & wording
        delta <- ifelse(is.na(delta_raw), NA, round(delta_raw, 2))
        delta_smooth_r <- ifelse(is.na(delta_smooth), NA, round(delta_smooth, 2))
        current <- ifelse(is.na(current_wt), "NA", round(current_wt, 1))
        ideal   <- ifelse(is.na(ideal_wt), "NA", round(ideal_wt, 1))
        dist_abs <- ifelse(is.na(dist_raw), NA, round(abs(dist_raw), 1))
        dist_dir <- ifelse(is.na(dist_raw), "from your ideal", ifelse(dist_raw > 0, "below", "above"))
        
        # readable delta phrase for current week
        delta_phrase <- if (is.na(delta_raw)) {
          "had no weight change recorded"
        } else if (delta_raw > 0) {
          paste0("gained ", delta, " kg")
        } else if (delta_raw < 0) {
          paste0("lost ", abs(delta), " kg")
        } else {
          "maintained your weight"
        }
        
        ## ----- VARIOUS SCENARIOS OF WEIGHT CHANGE & APPROPRIATE MESSAGING -----##
        # ----- If at or extremely close to ideal -----
        if (isTRUE(at_ideal)) {
          return(paste0(
            "🏁 You are at (or very close to) your ideal weight. Your current weight is ", current, 
            " kg and your ideal weight is ", ideal, " kg. Small week-to-week changes are normal."
          ))
        }
        
        # ----- Period goal OK + long-term OK + projection allowed -----
        if (isTRUE(direction_ok) && isTRUE(longterm_ok) && isTRUE(proj_allowed) && !is.na(weeks_to_ideal)) {
          return(paste0(
            "✅ You ", delta_phrase, " this week, which aligns with your Period goal (", goal, "). ",
            "Your average trend over the past 3 weeks is ", delta_smooth_r, " kg/week. ",
            "Your current weight (", current, "kg) is ",dist_dir," ideal (", ideal, "kg) by ",dist_abs, "kg."
          ))
        }
        
        # ----- Period goal OK, but long-term trend NOT moving toward ideal -----
        if (isTRUE(direction_ok) && !is.na(longterm_ok) && !isTRUE(longterm_ok)) {
          return(paste0(
            "⚠️ You ", delta_phrase, " this week, which aligns with your Period goal (", goal, "). ",
            "However, your average trend over the past 3 weeks of ", delta_smooth_r,
            " kg/week is not moving toward your long-term ideal. ",
            "Your current weight (", current, "kg) is ",dist_dir," ideal (", ideal, "kg) by ",dist_abs, "kg."
          ))
        }
        
        # ----- Period goal OK but projection NOT allowed (noisy trend / sign mismatch) -----
        if (isTRUE(direction_ok) && !isTRUE(proj_allowed)) {
          return(paste0(
            "⚠️ You ", delta_phrase, " this week, which aligns with your Period goal (", goal, "). ",
            "However, your average trend over the past 3 weeks of ", delta_smooth_r,
            " kg/week is not moving toward your long-term ideal. ",
            "Your current weight (", current, "kg) is ",dist_dir," ideal (", ideal, "kg) by ",dist_abs, "kg."
          ))
        }
        
        # ----- Period goal NOT OK but long-term trend OK + projection allowed -----
        if (!isTRUE(direction_ok) && isTRUE(longterm_ok) && isTRUE(proj_allowed) && !is.na(weeks_to_ideal)) {
          return(paste0(
            "⚠️ You ", delta_phrase, " this week, which does not align with your Period goal (", goal, "). ",
            "However, your average trend over the past 3 weeks of ", delta_smooth_r,
            " kg/week is moving toward your long-term ideal. ",
            "Your current weight (", current, "kg) is ",dist_dir," ideal (", ideal, "kg) by ",dist_abs, "kg."
          ))
        }
        
        # ----- Neither period goal nor long-term trend OK -----
        if (!isTRUE(direction_ok) && (!isTRUE(longterm_ok) || is.na(longterm_ok))) {
          return(paste0(
            "⚠️ You ", delta_phrase, " this week, which does not align with your Period goal (", goal, "). ",
            "Your average trend over the past 3 weeks of ", delta_smooth_r,
            " kg/week is not moving toward your long-term ideal. ",
            "Your current weight (", current, "kg) is ",dist_dir," ideal (", ideal, "kg) by ",dist_abs, "kg."
          ))
        }
        
        # ----- Fallback message -----
        paste0(
          "ℹ️ You ", delta_phrase, ". Current weight: ", current, " kg; Ideal: ", ideal, " kg. ",
          "No further insight could be generated for this week."
        )
      }),
    
    # ─────────────────────────────
    # 2) BEHAVIORAL ADHERENCE
    # ─────────────────────────────
    Behavioral_Adherence = pmap_chr(
      list(
        Calorie_Deviation, Protein_Deviation_g,
        Sleep_Deviation_hr, Steps_Deviation,
        Session_Deviation, Duration_Deviation
      ),
      function(Calorie_Deviation, Protein_Deviation_g, Sleep_Deviation_hr,
               Steps_Deviation, Session_Deviation, Duration_Deviation) {
        
        # --- Nutrition ---
        nutrition_msgs <- c()
        if (!is.na(Calorie_Deviation) && abs(Calorie_Deviation) > 50) {
          nutrition_msgs <- c(nutrition_msgs,
                              if (Calorie_Deviation > 0) "💡 Calorie intake was below target" else "💡 Calorie intake was above target")
        }
        if (!is.na(Protein_Deviation_g) && abs(Protein_Deviation_g) > 20) {
          nutrition_msgs <- c(nutrition_msgs,
                              if (Protein_Deviation_g > 0) "🍗 Protein intake was below target" else "🍗 Protein intake exceeded target")
        }
        
        # --- Lifestyle ---
        lifestyle_msgs <- c()
        if (!is.na(Sleep_Deviation_hr) && abs(Sleep_Deviation_hr) >= 0.5) {
          lifestyle_msgs <- c(lifestyle_msgs,
                              if (Sleep_Deviation_hr > 0) "🛌 Sleep was below profile baseline" else "🛌 Sleep was above profile baseline")
        }
        if (!is.na(Steps_Deviation) && abs(Steps_Deviation) >= 1000) {
          lifestyle_msgs <- c(lifestyle_msgs,
                              if (Steps_Deviation > 0) "🚶 Daily steps were below profile baseline" else "🚶 Daily steps exceeded profile baseline")
        }
        
        # --- Training ---
        training_msgs <- c()
        
        # Session frequency deviation
        if (!is.na(Session_Deviation) && Session_Deviation != 0) {
          training_msgs <- c(
            training_msgs,
            if (Session_Deviation > 0)
              "🏋️‍♂️ Completed fewer sessions than planned"
            else
              "🏋️‍♂️ Completed more sessions than planned"
          )
        }
        
        # Session duration deviation
        if (!is.na(Duration_Deviation) && Duration_Deviation != 0) {
          training_msgs <- c(
            training_msgs,
            if (Duration_Deviation > 0)
              "⏱️ Average session duration was below target"
            else
              "⏱️ Average session duration exceeded target"
          )
        }
        
        # --- Combine all categories ---
        paragraph <- c()
        if (length(nutrition_msgs) > 0) paragraph <- c(paragraph, paste(nutrition_msgs, collapse = "; "))
        if (length(lifestyle_msgs) > 0) paragraph <- c(paragraph, paste(lifestyle_msgs, collapse = "; "))
        if (length(training_msgs) > 0) paragraph <- c(paragraph, training_msgs)
        
        if (length(paragraph) == 0) "" else paste(paragraph, collapse = " | ")
      }
    ),
    
    # ─────────────────────────────
    # 3) FURTHER OBSERVATIONS
    # ─────────────────────────────
    Further_Observed = pmap_chr(
      list(
        Out_Of_Reference_Range,
        BF_Distance_perc,
        FFMI_Distance,
        Waist_Distance_in
      ),
      function(out_of_range, bf_dist, ffmi_dist, waist_dist) {
        
        paragraph <- c()
        
        # --- Reference Calories ---
        if (!is.na(out_of_range) && out_of_range) {
          paragraph <- c(paragraph,
                         "⚠️ Period target calorie intake is outside the heuristic-based recommended reference range.")
        }
        
        
        # --- Distance from Ideal BF% ---
        if (!is.na(bf_dist)) {
          dir <- ifelse(bf_dist > 0, "below", "above")
          paragraph <- c(
            paragraph,
            paste0("📉 Your current body fat percentage is ",
                   abs(round(bf_dist, 1)), "% ", dir, " your ideal BF%")
          )
        }
        
        # # --- Distance from Ideal FFMI ---
        # if (!is.na(ffmi_dist)) {
        #   dir <- ifelse(ffmi_dist > 0, "below", "above")
        #   paragraph <- c(
        #     paragraph,
        #     paste0("📊 Your current FFMI (",Observed_FFMI_band,")", " is ",
        #            abs(round(ffmi_dist, 1)), " points ", dir, " your ideal FFMI (",Ideal_FFMI_band,").")
        #   )
        # }
        
        # --- Distance from Ideal Waist ---
        if (!is.na(waist_dist)) {
          dir <- ifelse(waist_dist > 0, "below", "above")
          
          # Base sentence
          text_line <- paste0(
            "📏 Your Waist is ",
            abs(round(waist_dist, 1)), 
            " in ", dir, " your ideal"
          )
          
          # If waist is ABOVE ideal by more than 1 inch → append (consider cutting)
          if (waist_dist < -1) {
            text_line <- paste0(text_line, " (consider cutting)")
          }
          
          paragraph <- c(paragraph, text_line)
        }
        
        # If nothing to report → ""
        if (length(paragraph) == 0) "" else paste(paragraph, collapse = " | ")
      }
    )
  )

# ─────────────────────────────
# FLAGGING PERSISTENT ISSUES
# ─────────────────────────────
# HELPER: returns a logical vector of same length: TRUE where there is a run of n TRUEs ending at that index
persistent_run_row <- function(x, n = 3) {
  # ensure x is logical and NAs -> FALSE for purposes of streaks
  xv <- ifelse(is.na(x), FALSE, as.logical(x))
  out <- rep(FALSE, length(xv))
  if (length(xv) < n) return(out)
  for (i in seq_along(xv)) {
    if (i < n) { out[i] <- FALSE; next }
    vals <- xv[(i - n + 1):i]
    out[i] <- all(vals)  # because NAs are already turned to FALSE this is safe
  }
  out
}

# compute weekly flags and persistent flags 
weekly_data <- weekly_data %>%
  
  mutate(
    # weekly boolean flags (NA-safe)
    Flag_Calorie  = ifelse(is.na(Calorie_Deviation), FALSE, abs(Calorie_Deviation) > 100),
    Flag_Protein  = ifelse(is.na(Protein_Deviation_g), FALSE, abs(Protein_Deviation_g) > 20),
    Flag_Session  = ifelse(is.na(Session_Deviation), FALSE, Session_Deviation != 0),
    Flag_Duration = ifelse(is.na(Duration_Deviation), FALSE, abs(Duration_Deviation) > 20),
    Flag_Steps    = ifelse(is.na(Steps_Deviation), FALSE, abs(Steps_Deviation) > 1000),
    Flag_Sleep    = ifelse(is.na(Sleep_Deviation_hr), FALSE, abs(Sleep_Deviation_hr) > 0.5),
    Flag_Weight_Direction = ifelse(is.na(Weight_Direction_OK), FALSE, !Weight_Direction_OK),
    
    # Weight distance: use absolute distance comparison
    Flag_Weight_Distance = {
      prev_abs = dplyr::lag(abs(Weight_Distance_kg))
      this_abs = abs(Weight_Distance_kg)
      f = ifelse(is.na(prev_abs) | is.na(this_abs), FALSE, this_abs > prev_abs)
      f
    }
    
  ) %>%
  
  # compute persistent (3-consecutive-week) flags using helper
  mutate(
    Persistent_Calorie = persistent_run_row(Flag_Calorie, 3),
    Persistent_Protein = persistent_run_row(Flag_Protein, 3),
    Persistent_Session = persistent_run_row(Flag_Session, 3),
    Persistent_Duration= persistent_run_row(Flag_Duration, 3),
    Persistent_Steps   = persistent_run_row(Flag_Steps, 3),
    Persistent_Sleep   = persistent_run_row(Flag_Sleep, 3),
    Persistent_Weight_Direction = persistent_run_row(Flag_Weight_Direction, 3),
    Persistent_Weight_Distance  = persistent_run_row(Flag_Weight_Distance, 3)
  ) %>%
  
  # build short human text for persistent issues (only show from week >= 3)
  mutate(
    Persistent_Issues = purrr::pmap_chr(
      list(Persistent_Calorie, Persistent_Protein,
           Persistent_Session, Persistent_Duration, Persistent_Steps,
           Persistent_Sleep, Persistent_Weight_Direction,
           Persistent_Weight_Distance,
           row_number()),
      function(cal, prot, sess, dur, stps, slp ,wd, wdist, rn) {
        if (rn < 3) return("")
        msgs <- c()
        if (isTRUE(cal))  msgs <- c(msgs, "📌 Persistent calorie deviation over the last 3wk")
        if (isTRUE(prot)) msgs <- c(msgs, "📌 Persistent protein deviation over the last 3wk")
        if (isTRUE(sess)) msgs <- c(msgs, "📌 Missed training sessions persistently over the last 3wk")
        if (isTRUE(dur))  msgs <- c(msgs, "📌 Training duration consistently off target over the last 3wk")
        if (isTRUE(stps)) msgs <- c(msgs, "📌 Persistently low/excessive steps over the last 3wk")
        if (isTRUE(slp))  msgs <- c(msgs, "📌 Persistent sleep deviation over the last 3wk")
        if (isTRUE(sess)) msgs <- c(msgs, "📌 Missed training sessions persistently over the last 3wk")
        if (isTRUE(wd))   msgs <- c(msgs, "📌 Weight has been persistently trending away from goal over the last 3wk")
        if (isTRUE(wdist))msgs <- c(msgs, "📌 Distance from ideal weight has been presistently increasing over the last 3wk")
        if (length(msgs) == 0) return("")
        paste(msgs, collapse = " | ")
      }
    )
  )

# ────────────────────────────────
# REORDER COLUMNS
# ────────────────────────────────
weekly_data <- weekly_data %>%
  select(
    # ───────────────────────────────
    # Basic info
    # ───────────────────────────────
    Identity, Profile, Gender, DOB, Age, Height_cm, Height_m,
    Period_Name, Target_Start_Date, Target_End_Date, Target_Duration_wk, Start_Weight_kg, Preset_Goal_Template,Goal_Type, Intensity, 
    Week_Counter, Week_Start,
    
    # ───────────────────────────────
    # Body 
    # ───────────────────────────────
    Ideal_LBM_kg,Ideal_Weight_FFMI,Observed_LBM_Raw_kg,Observed_LBM_Smoothed_kg,Observed_Avg_Weight_kg,Weight_Distance_kg, 
    ΔWeight_per_Week_raw, ΔWeight_per_Week_smoothed,
    Weeks_to_Ideal_Weight, Weeks_to_Ideal_Flag,
    Ideal_BF_perc, Observed_Avg_BF_perc,BF_Distance_perc,
    Ideal_FFMI,Ideal_FFMI_band, Observed_FFMI,Observed_FFMI_band, FFMI_Distance,
    Ideal_Waist_in,Observed_Waist_in,Waist_Distance_in,
    
    # ───────────────────────────────
    # Nutrition
    # ───────────────────────────────
    BMR, Activity_Factor_Target,
    Baseline_Period_TDEE, Observed_TDEE_Raw,TDEE_Deviation_Raw, Observed_TDEE_Smoothed, TDEE_Deviation_Smoothed,  
    #calories & ref ranges
    Calorie_Adjustment_perc,
    Target_Calorie_Intake_for_Period,
    Reference_Calorie_Intake_Bulk_Lower, Reference_Calorie_Intake_Bulk_Upper,
    Reference_Calorie_Intake_Cut_Lower, Reference_Calorie_Intake_Cut_Upper,
    Out_Of_Reference_Range,
    Observed_Avg_Daily_Calorie_Intake, Calorie_Deviation,
    
    #macros
    Protein_g_lb, Carbs_g_lb_TD, Carbs_g_lb_NTD,
    Target_Protein_Intake_for_Period, Observed_Avg_Daily_Protein_Intake,Protein_Deviation_g,
    Target_Carbs_Intake_TD_for_Period, Target_Carbs_Intake_NTD_for_Period,Target_Carbs_Intake_Avg_for_Period,Target_Fat_Intake_for_Period,
    
    # ───────────────────────────────
    #  Lifestyle
    # ───────────────────────────────
    Baseline_Daily_Steps, Observed_Avg_Steps,Steps_Deviation,
    Baseline_Sleep_hr, Observed_Avg_Sleep_hr,Sleep_Deviation_hr,
    
    # ───────────────────────────────
    # Training 
    # ───────────────────────────────
    Target_Sessions_per_wk,Observed_Sessions,Session_Deviation, 
    Target_Session_Duration_min, Observed_Session_Duration_min,Duration_Deviation,
    #training program info
    Training_Programs, Program, 
    #volumes and sets info
    Weekly_Training_Volume_Accessories, Weekly_Training_Volume_Back, Weekly_Training_Volume_Biceps,
    Weekly_Training_Volume_Chest, Weekly_Training_Volume_Legs, Weekly_Training_Volume_Shoulders,
    Weekly_Training_Volume_Triceps,
    Weekly_Training_Sets_Accessories, Weekly_Training_Sets_Back, Weekly_Training_Sets_Biceps,
    Weekly_Training_Sets_Chest, Weekly_Training_Sets_Legs, Weekly_Training_Sets_Shoulders,
    Weekly_Training_Sets_Triceps,
    
    # ───────────────────────────────
    # Flags & Checks
    # ───────────────────────────────
    #week over week
    Flag_Calorie, Flag_Protein, Flag_Session,
    Flag_Duration, Flag_Steps, Flag_Sleep,
    Flag_Weight_Direction, Flag_Weight_Distance,
    At_Ideal, LongTerm_Needs_Gain, LongTerm_Trend_Sign, Projection_Allowed,
    #persistent
    Persistent_Calorie, Persistent_Protein,
    Persistent_Session, Persistent_Duration, Persistent_Steps,
    Persistent_Sleep, Persistent_Weight_Direction,
    Persistent_Weight_Distance,
    #direction checks
    Weight_Direction_OK,LongTerm_Direction_OK,
    
    # ───────────────────────────────
    # Narrative
    # ───────────────────────────────
    Primary_Insight, Behavioral_Adherence, Further_Observed, Persistent_Issues,
    
    # ───────────────────────────────
    # Notes
    # ───────────────────────────────
    Notes.x, Notes.y, Notes,
    
    # ───────────────────────────────
    # Snapshot date
    # ───────────────────────────────
    Snapshot_Date
  )

## Quick Check that all columns are included
#length(colnames(weekly_data))           # Original
#length(colnames(weekly_data_reordered)) # Reordered
#setdiff(colnames(weekly_data), colnames(weekly_data_reordered))  # Should return character(0)

#FROM HERE ON WE'RE JUST COMPILING METRICS AND VARIABLES ALREADY COMPUTED ABOVE
# ────────────────────────────────────────────────────────────────────────
# --------------------------REPORTING & ANALYSIS-------------------------- 
# ────────────────────────────────────────────────────────────────────────
# ‼️ Define a new tibble (filtered for the active profile based on Identity selected at the top) 
#this returns a tibble containing multiple periods for the current profile (i.e. historical profiles are discarded)
#use this tibble when required to do multi-period analysis (within the active profile)
weekly_data_active_profile <- weekly_data %>%
  filter(Profile == active_profile)    

# ────────────────────────────────
# A) FINAL PERIOD SUMMARY
# ────────────────────────────────
# ---------------------------
# 0) Helper functions
# ---------------------------

# Helper: concatenate user notes
concat_notes <- function(row) {
  notes <- c(row$Notes.x, row$Notes.y, row$Notes)
  notes <- notes[!is.na(notes) & notes != ""]
  if(length(notes) == 0) return("")
  paste(notes, collapse = " | ")
}

# Helper: Determine reference calorie ranges based on Goal_Type
get_ref_cal <- function(goal_type, row) {
  if (grepl("bulk", goal_type, ignore.case = TRUE)) {
    paste0(r(row$Reference_Calorie_Intake_Bulk_Lower), 
           "-", r(row$Reference_Calorie_Intake_Bulk_Upper), " kcal")
  } else if (grepl("cut", goal_type, ignore.case = TRUE)) {
    paste0(r(row$Reference_Calorie_Intake_Cut_Lower), 
           "-", r(row$Reference_Calorie_Intake_Cut_Upper), " kcal")
  } else {
    "No reference"
  }
}

# Helper: returns a df containing the LAST observed week for each profile-period
last_week_per_period <- function(
    weekly_data,
    active_profile = NULL,
    use_observed_bounds = TRUE
) {
  df <- weekly_data %>% ungroup()   # important
  
  if (use_observed_bounds) {
    
    period_bounds <- df %>%
      group_by(Profile, Period_Name) %>%
      summarise(
        observed_period_start = min(Week_Start, na.rm = TRUE),
        observed_period_end   = max(Week_Start, na.rm = TRUE),
        .groups = "drop"
      )
    
    df2 <- df %>%
      inner_join(period_bounds, by = c("Profile", "Period_Name")) %>%
      filter(Week_Start >= observed_period_start,
             Week_Start <= observed_period_end) %>%
      arrange(Profile, Period_Name, Week_Start) %>%
      group_by(Profile, Period_Name) %>%
      slice_tail(n = 1) %>%
      ungroup()
    
    return(df2)
    
  } else {
    df2 <- df %>%
      filter(Week_Start >= Target_Start_Date &
               Week_Start <= Target_End_Date) %>%
      arrange(Profile, Period_Name, Week_Start) %>%
      group_by(Profile, Period_Name) %>%
      slice_tail(n = 1) %>%
      ungroup()
    
    return(df2)
  }
}

#Helper: get the last week of the latest period (i.e. current week)
get_current_week <- function(weekly_data, active_profile) {
  last_week_per_period(weekly_data, active_profile) %>%
    ungroup() %>%                     # safety
    slice_max(Week_Start, n = 1, with_ties = FALSE)
}

# Helper: Universal formatting helper for stats
format_stat <- function(x, unit = "", digits = 2, na_text = "Insufficient data") {
  if (length(x) == 0 || all(is.na(x))) return(na_text)
  if (is.na(x)) return(na_text)
  paste0(round(x, digits), unit)
}

# ---------------------------
# 1) CURRENT WEEK STATS
# ---------------------------
#‼️ Define a new tibble containing one row only (filters the weekly_data_active_profile further for the current week)
#Using the helpers above, this returns a tibble containing the last week of the current period (i.e. historical weeks within the period are discarded)
#use this tibble when required to report on the current week only
weekly_data_current_week <- get_current_week(weekly_data_active_profile, active_profile)

#‼️ Define a new tibble (filters the weekly_data_active_profile further for the active/current period)
#It uses a the weekly_data_current_week above to identify the the period to which the current week belongs
#then, it filters the weekly_data_active_profile to return a tibble containing multiple weeks for the current period (i.e. historical periods are discarded)
#use this tibble when required to do multi-week analysis (within the current period)
weekly_data_current_period <- weekly_data_active_profile %>%
  filter(
    Profile == weekly_data_current_week$Profile,
    Period_Name == weekly_data_current_week$Period_Name,
    Week_Start <= weekly_data_current_week$Week_Start
  ) %>% ungroup() # to undo global default grouping for plotting purposes

#compile summary stats for the active profile and current week
current_week_summary_stats <- list(
  #Header Info
  Header_Info = list(
    Current_Period_Name = weekly_data_current_week$Period_Name,
    Current_Period_Goal = weekly_data_current_week$Preset_Goal_Template,
    Period_Starting_Weight = weekly_data_current_week$Start_Weight_kg,
    Period_Target_Start_Date = weekly_data_current_week$Target_Start_Date,
    Period_Target_End_Date = weekly_data_current_week$Target_End_Date,
    Current_Week_Start_Date = weekly_data_current_week$Week_Start
  ),
  
  # A. Body Metrics
  Body = list(
    Current_Weight = r(weekly_data_current_week$Observed_Avg_Weight_kg),
    Last_Week_Weight = r(lag(weekly_data_current_period$Observed_Avg_Weight_kg) %>% last(na_rm = TRUE)),
    Delta_Weight_per_wk_Raw = r(weekly_data_current_week$`ΔWeight_per_Week_raw`),
    
    Ideal_Weight_FFMI = r(weekly_data_current_week$Ideal_Weight_FFMI),
    Weight_Distance_kg = r(weekly_data_current_week$Weight_Distance_kg),
    Delta_Weight_per_wk_Smoothed3 = r(weekly_data_current_week$`ΔWeight_per_Week_smoothed`),
    
    Observed_FFMI = r(weekly_data_current_week$Observed_FFMI),
    Observed_FFMI_band = weekly_data_current_week$Observed_FFMI_band,
    Ideal_FFMI = r(weekly_data_current_week$Ideal_FFMI),
    Ideal_FFMI_band = weekly_data_current_week$Ideal_FFMI_band,
    FFMI_Distance = r(weekly_data_current_week$FFMI_Distance),
    
    Current_BF_perc = r(weekly_data_current_week$Observed_Avg_BF_perc),
    Ideal_BF_perc = r(weekly_data_current_week$Ideal_BF_perc),
    BF_Distance_perc = r(weekly_data_current_week$BF_Distance_perc),
    
    Current_Waist_in = r(weekly_data_current_week$Observed_Waist_in),
    Ideal_Waist_in = r(weekly_data_current_week$Ideal_Waist_in),
    Waist_Distance_in = r(weekly_data_current_week$Waist_Distance_in)
  ),
  
  # B. Nutrition Metrics
  Nutrition = list(
    Baseline_Period_TDEE = r(weekly_data_current_week$Baseline_Period_TDEE),
    Calorie_Adjustment_perc = r(weekly_data_current_week$Calorie_Adjustment_perc),
    Target_Calorie_Intake = r(weekly_data_current_week$Target_Calorie_Intake_for_Period),
    Reference_Calories = get_ref_cal(weekly_data_current_week$Goal_Type, weekly_data_current_week),
    
    Observed_TDEE_Raw = r(weekly_data_current_week$Observed_TDEE_Raw),
    Delta_TDEE_Raw = r(weekly_data_current_week$TDEE_Deviation_Raw),
    Delta_TDEE_Smoothed3 = r(weekly_data_current_week$TDEE_Deviation_Smoothed),
    
    Current_Calorie_Intake = r(weekly_data_current_week$Observed_Avg_Daily_Calorie_Intake),
    Calorie_Deviation = r(weekly_data_current_week$Calorie_Deviation),
    
    Target_Protein_Intake = r(weekly_data_current_week$Target_Protein_Intake_for_Period),
    Current_Protein_Intake = r(weekly_data_current_week$Observed_Avg_Daily_Protein_Intake),
    Protein_Deviation = r(weekly_data_current_week$Protein_Deviation_g),
    
    Target_Carbs_Intake_NTD = r(weekly_data_current_week$Target_Carbs_Intake_NTD_for_Period),
    Target_Carbs_Intake_TD = r(weekly_data_current_week$Target_Carbs_Intake_TD_for_Period),
    Target_Fat_Intake = r(weekly_data_current_week$Target_Fat_Intake_for_Period)
  ),
  
  # C. Training Metrics
  Training = list(
    Target_Sessions = r(weekly_data_current_week$Target_Sessions_per_wk),
    Current_Sessions = r(weekly_data_current_week$Observed_Sessions),
    Session_Deviation = r(weekly_data_current_week$Session_Deviation),
    
    Target_SessionDuration = r(weekly_data_current_week$Target_Session_Duration_min),
    Current_SessionDuration = r(weekly_data_current_week$Observed_Session_Duration_min),
    SessionDuration_Deviation = r(weekly_data_current_week$Duration_Deviation),
    
    Current_Weekly_Training_Sets_Chest = weekly_data_current_week$Weekly_Training_Sets_Chest,
    Current_Weekly_Training_Sets_Shoulders = weekly_data_current_week$Weekly_Training_Sets_Shoulders,
    Current_Weekly_Training_Sets_Back = weekly_data_current_week$Weekly_Training_Sets_Back,
    Current_Weekly_Training_Sets_Triceps = weekly_data_current_week$Weekly_Training_Sets_Triceps,
    Current_Weekly_Training_Sets_Biceps = weekly_data_current_week$Weekly_Training_Sets_Biceps,
    Current_Weekly_Training_Sets_Legs = weekly_data_current_week$Weekly_Training_Sets_Legs
  ),
  
  # D. Lifestyle & Recovery
  Lifestyle = list(
    Baseline_Daily_Steps = r(weekly_data_current_week$Baseline_Daily_Steps),
    Current_Steps = r(weekly_data_current_week$Observed_Avg_Steps),
    Steps_Deviation = r(weekly_data_current_week$Steps_Deviation),
    
    Baseline_Sleep_hr = r(weekly_data_current_week$Baseline_Sleep_hr),
    Current_Sleep_hr = r(weekly_data_current_week$Observed_Avg_Sleep_hr),
    Sleep_Deviation_hr = r(weekly_data_current_week$Sleep_Deviation_hr)
  )
  
)

# ---------------------------
# 2) INSIGHTS (commentary)
# ---------------------------

current_week_summary_insights <- list(
  Week_Primary_Insights = weekly_data_current_week$Primary_Insight,
  Week_Behavioral_Insights = weekly_data_current_week$Behavioral_Adherence,
  Week_Further_Observations = weekly_data_current_week$Further_Observed,
  Persistent_Issues_3wk = weekly_data_current_week$Persistent_Issues,
  Week_User_Notes = concat_notes(weekly_data_current_week)
)

# ---------------------------
# 3) FINAL WEEKLY SUMMARY OBJECT
# ---------------------------

current_week_final_summary <- list(
  Current_Period_WeeklyData = weekly_data_current_period, # all weekly data within the current period
  Current_Week_Stats = current_week_summary_stats,
  Current_Week_Insights = current_week_summary_insights
)

# ────────────────────────────────
# B) FINAL PROFILE SUMMARY (MULTI-PERIOD ANALYSES)
# ────────────────────────────────
# -----------------------------
# Simplified Multi-period Analysis (Fitness-focused)
# Input: weekly_data_active_profile (already joined upstream)
# Output: list(Period_Table, Profile_Stats, Profile_Insights, Profile_Plots)
# -----------------------------
# -----------------------------
# 0. HELPERS
# -----------------------------
#Helper: convert date
safe_date <- function(df, date_col = "Week_Start") {
  if (!inherits(df[[date_col]], "Date")) df[[date_col]] <- as.Date(df[[date_col]])
  df
}

#Helper:handling NA values
first_non_na <- function(x) {
  xi <- x[!is.na(x)]
  if (length(xi) == 0) return(NA_real_)
  xi[1]
}

#Helper: Classify weight velocity (%/week) + absolute kg/week
classify_velocity <- function(delta_kg_per_week, current_weight) {
  if (is.na(delta_kg_per_week) || is.na(current_weight) || current_weight <= 0) return(NA_character_)
  pct_week <- (delta_kg_per_week / current_weight) * 100
  
  vel_class <- case_when(
    pct_week <= -1.3 ~ "Extreme Loss (>1.3%/wk)",
    pct_week <= -1.0 ~ "Deep Cut (1.0–1.3%/wk)",
    pct_week <= -0.7 ~ "Moderate Cut (0.7–1.0%/wk)",
    pct_week <= -0.4 ~ "Mild Cut (0.4–0.7%/wk)",
    abs(pct_week) <= 0.25 ~ "Stable / Recomp (±0.25%/wk)",
    pct_week <= 0.5 ~ "Lean Bulk (0.25–0.5%/wk)",
    pct_week <= 0.8 ~ "Moderate Bulk (0.5–0.8%/wk)",
    TRUE ~ "Aggressive Bulk (>0.8%/wk)"
  )
  
  vel_class
}

#Helper: interpreting ffmi change alongside BF, waist and weight
trend_interpretation <- function(df,
                                 eps_ffmi = 0.05,
                                 eps_bf   = 0.2,
                                 eps_waist = 0.1,
                                 eps_weight = 0.2) {
  # ---- Extract safely ----
  safe_first <- function(x) first_non_na(x)
  safe_last  <- function(x) last(na.omit(x))
  
  wt_first  <- safe_first(df$Observed_Avg_Weight_kg)
  wt_last   <- safe_last(df$Observed_Avg_Weight_kg)
  waist_first <- safe_first(df$Observed_Waist_in)
  waist_last  <- safe_last(df$Observed_Waist_in)
  ffmi_first <- safe_first(df$Observed_FFMI)
  ffmi_last  <- safe_last(df$Observed_FFMI)
  bf_first   <- safe_first(df$Observed_Avg_BF_perc)
  bf_last    <- safe_last(df$Observed_Avg_BF_perc)
  
  metrics <- c(wt_first, wt_last, waist_first, waist_last,
               ffmi_first, ffmi_last, bf_first, bf_last)
  
  # ---- If everything NA → insufficient ----
  if (all(is.na(metrics))) {
    return("Insufficient data")
  }
  
  # ---- Compute changes (NA-safe) ----
  change_or_na <- function(a, b) {
    if (is.na(a) || is.na(b)) return(NA_real_)
    b - a
  }
  
  weight_change <- change_or_na(wt_first, wt_last)
  waist_change  <- change_or_na(waist_first, waist_last)
  ffmi_change   <- change_or_na(ffmi_first, ffmi_last)
  bf_change     <- change_or_na(bf_first, bf_last)
  
  # ---- If key signals are missing → incomplete inference ----
  if (any(is.na(c(weight_change, waist_change, ffmi_change, bf_change)))) {
    return("Insufficient data for reliable interpretation")
  }
  
  # ---- Define plateau logic ----
  plateau <- function(x, eps) abs(x) <= eps
  
  ffmi_plateau  <- plateau(ffmi_change, eps_ffmi)
  bf_plateau    <- plateau(bf_change, eps_bf)
  waist_plateau <- plateau(waist_change, eps_waist)
  weight_plateau <- plateau(weight_change, eps_weight)
  
  # ---- Scoring system ----
  score <- list(
    lean_gain   = 0,
    fat_gain    = 0,
    fat_loss    = 0,
    muscle_loss = 0
  )
  
  # ---- FFMI logic (strongest indicator) ----
  if (!ffmi_plateau && ffmi_change > 0) score$lean_gain   <- score$lean_gain + 4
  if (!ffmi_plateau && ffmi_change < 0) score$muscle_loss <- score$muscle_loss + 4
  
  # Plateau FFMI = training plateau
  if (ffmi_plateau) {
    score$muscle_loss <- score$muscle_loss + 0
    score$lean_gain   <- score$lean_gain + 0
  }
  
  # ---- Body Fat % logic (correct column) ----
  if (!bf_plateau && bf_change < 0) score$fat_loss <- score$fat_loss + 3
  if (!bf_plateau && bf_change > 0) score$fat_gain <- score$fat_gain + 3
  
  # ---- Synergy rules ----
  if (ffmi_change > 0 && bf_change < 0) score$lean_gain <- score$lean_gain + 2
  if (bf_change < 0 && waist_change < -eps_waist) score$fat_loss <- score$fat_loss + 2
  if (bf_change > 0 && weight_change > eps_weight) score$fat_gain <- score$fat_gain + 1
  
  # ---- Waist ----
  if (!waist_plateau && waist_change < 0) score$fat_loss <- score$fat_loss + 2
  if (!waist_plateau && waist_change > 0) score$fat_gain <- score$fat_gain + 2
  
  # ---- Weight contextual ----
  if (!weight_plateau && weight_change > 0) {
    if (ffmi_change > 0) score$lean_gain <- score$lean_gain + 1
    if (bf_change > 0) score$fat_gain <- score$fat_gain + 1
  }
  
  if (!weight_plateau && weight_change < 0) {
    if (bf_change < 0) score$fat_loss <- score$fat_loss + 1
    if (ffmi_change < 0) score$muscle_loss <- score$muscle_loss + 1
  }
  
  # ---- Identify winning signal ----
  scores_numeric <- unlist(score)
  top_score <- max(scores_numeric)
  winners <- names(scores_numeric[scores_numeric == top_score])
  
  # ---- Mixed / unclear scenario ----
  if (length(winners) > 1 || top_score == 0) {
    return("Mixed signals — no clear interpretation")
  }
  
  outcomes <- c(
    lean_gain   = "Likely lean-mass gain",
    fat_gain    = "Likely fat gain (or mixed soft bulk)",
    fat_loss    = "Likely fat loss",
    muscle_loss = "Likely muscle loss"
  )
  
  return(outcomes[[winners]])
}

#Helper: Weeks to Milestone Calculator
calc_weeks_to_target <- function(current_value, target_value, slope_per_week,min_abs_slope = 0.01) {
  if (is.na(current_value) || is.na(target_value) || is.na(slope_per_week)) return(NA_real_)
  if (abs(slope_per_week) < min_abs_slope) return(NA_real_)  # slope too flat to project
  
  weeks <- (target_value - current_value) / slope_per_week
  
  # negative weeks → target is in the opposite direction of the slope
  if (weeks < 0) return(NA_real_)
  
  weeks
}

# -----------------------------
# 1) Per-period summary
# -----------------------------
compute_period_summary <- function(df) {
  df <- df %>% ungroup() %>% safe_date()
  
  period_bounds <- df %>%
    group_by(Profile, Period_Name) %>%
    summarise(
      observed_period_start = min(Week_Start, na.rm = TRUE),
      observed_period_end   = max(Week_Start, na.rm = TRUE),
      .groups = "drop"
    )
  
  df %>%
    inner_join(period_bounds, by = c("Profile", "Period_Name")) %>%
    filter(Week_Start >= observed_period_start, Week_Start <= observed_period_end) %>%
    arrange(Profile, Period_Name, Week_Start) %>%
    group_by(Profile, Period_Name) %>%
    summarise(
      Period_Start = min(Week_Start, na.rm = TRUE),
      Period_End = max(Week_Start, na.rm = TRUE),
      Period_Weeks = n(),
      Preset_Goal_Template = first(Preset_Goal_Template),
      
      Period_Baseline_TDEE = r(first(Baseline_Period_TDEE)),
      Calorie_Adjustment_perc = r(first(Calorie_Adjustment_perc)),
      Period_Target_Calorie_Intake = r(first(Target_Calorie_Intake_for_Period)),
      Period_Avg_Observed_Calorie_Intake = r(mean(Observed_Avg_Daily_Calorie_Intake, na.rm = TRUE)), 
      Period_Avg_Calorie_Deviation = r(mean(Calorie_Deviation, na.rm = TRUE)), #behavioral adherence
      
      Period_Earliest_Observed_Weight = r(first(Observed_Avg_Weight_kg)),       
      Period_Latest_Observed_Weight = r(last(Observed_Avg_Weight_kg)),
      Period_Weight_Change = r(Period_Latest_Observed_Weight - Period_Earliest_Observed_Weight), #outcome for period
      
      Period_Earliest_Observed_BF = r((first_non_na(df$Observed_Avg_BF_perc))),      
      Period_Latest_Observed_BF = r(last(na.omit(df$Observed_Avg_BF_perc))), 
      Period_BF_change = r(Period_Latest_Observed_BF -  Period_Earliest_Observed_BF), #outcome for period
      
      Period_Earliest_Observed_Waist = r(first(Observed_Waist_in)),      
      Period_Latest_Observed_Waist = r(last(Observed_Waist_in)), 
      Period_Waist_Change = r(Period_Latest_Observed_Waist - Period_Earliest_Observed_Waist), #outcome for period
      
      Period_Earliest_Observed_FFMI = r(first(Observed_FFMI)), 
      Period_Earliest_Observed_FFMI_band = first(Observed_FFMI_band),      
      Period_Latest_Observed_FFMI = r(last(Observed_FFMI)), 
      Period_Latest_Observed_FFMI_band = last(Observed_FFMI_band), 
      Period_FFMI_Change = r(Period_Latest_Observed_FFMI - Period_Earliest_Observed_FFMI), #outcome for period
      
      Period_Earliest_Observed_TDEE = r(first(Observed_TDEE_Raw)),
      Period_Latest_Observed_End_TDEE = r(last(Observed_TDEE_Raw)),
      Period_TDEE_change = r(Period_Latest_Observed_End_TDEE - Period_Earliest_Observed_TDEE), #change in metabolism 
      
      Period_Target_Protein_Intake = r(first(Target_Protein_Intake_for_Period)),
      Period_Observed_Avg_Protein_Intake = r(mean(Observed_Avg_Daily_Protein_Intake, na.rm = TRUE)), 
      
      Period_Target_Carbs_Intake_NTD = r(first(Target_Carbs_Intake_NTD_for_Period)),
      Period_Target_Carbs_Intake_TD = r(first(Target_Carbs_Intake_TD_for_Period)),
      Period_Target_Fat_Intake = r(first(Target_Fat_Intake_for_Period)),
      
      Period_Basline_Steps = r(first(Baseline_Daily_Steps)), 
      Period_Avg_Observed_Steps = r(mean(Observed_Avg_Steps, na.rm = TRUE)),  #average activity level
      
      Period_Basline_Sleep = r(first(Baseline_Sleep_hr)),
      Period_Avg_Observed_Sleep = r(mean(Observed_Avg_Sleep_hr, na.rm = TRUE)), #average recovery level
      
      Period_Target_Sessions_per_wk = r(first(Target_Sessions_per_wk)),
      Period_Avg_Observed_Sessions_per_wk = r(mean(Observed_Sessions, na.rm = TRUE)),
      
      Period_Target_SessionDuration= r(first(Target_Session_Duration_min)), 
      Period_Avg_Observed_SessionDuration = r(mean(Observed_Session_Duration_min, na.rm = TRUE)),
      
      Period_Avg_Weekly_Training_Sets_Chest = mean(Weekly_Training_Sets_Chest, na.rm = TRUE),
      Period_Avg_Weekly_Training_Sets_Shoulders = mean(Weekly_Training_Sets_Shoulders, na.rm = TRUE),
      Period_Avg_Weekly_Training_Sets_Back = mean(Weekly_Training_Sets_Back, na.rm = TRUE),
      Period_Avg_Weekly_Training_Sets_Triceps = mean(Weekly_Training_Sets_Triceps, na.rm = TRUE),
      Period_Avg_Weekly_Training_Sets_Biceps = mean(Weekly_Training_Sets_Biceps, na.rm = TRUE),
      Period_Avg_Weekly_Training_Sets_Legs = mean(Weekly_Training_Sets_Legs, na.rm = TRUE),
      .groups = "drop"
    )
}

# -----------------------------
# 2) Profile-level summary
# -----------------------------
compute_profile_summary <- function(df) {
  df <- df %>% ungroup() %>% safe_date() %>% arrange(Week_Start)
  if (nrow(df) == 0) return(list(stats = list(), insights = list(), plots = list()))
  
  total_periods <- length(unique(df$Period_Name))
  total_weeks <- nrow(df)
  earliest_profile_date <- first_non_na(df$Week_Start)
  latest_profile_date <-  last(na.omit(df$Week_Start))
  
  earliest_weight <- first_non_na(df$Observed_Avg_Weight_kg)
  latest_weight <- last(na.omit(df$Observed_Avg_Weight_kg))
  total_weight_change <- latest_weight - earliest_weight
  delta_weeks <- nrow(df)
  
  #rate of weight change (overall profile)
  slope_kg_per_week_profile <- ifelse(delta_weeks > 0, total_weight_change / delta_weeks, NA_real_)
  #rate of weight change (recent smoothed 3wk weight change)
  slope_kg_per_week_recent <- last(na.omit(df$ΔWeight_per_Week_smoothed))  
  
  # -----weeks to milestones (1kg & ideal)-----
  # latest goal type
  latest_goal_type <- last(na.omit(df$Goal_Type))
  
  trend <- case_when(
    is.na(slope_kg_per_week_recent) | abs(slope_kg_per_week_recent) < 0.01 ~ "flat",
    slope_kg_per_week_recent > 0 ~ "gain",
    TRUE ~ "loss"
  )
  
  weeks_to_1 <- case_when(
    trend == "gain" ~ calc_weeks_to_target(latest_weight, latest_weight + 1.0, slope_kg_per_week_recent),
    trend == "loss" ~ calc_weeks_to_target(latest_weight, latest_weight - 1.0, slope_kg_per_week_recent),
    TRUE ~ NA_real_
  )
  
  #Helper: friendly output format
  pretty_weeks <- function(w) {
    if (is.na(w)) return(NA_character_)
    if (w < 1) return(paste0(round(w*7), " days"))
    paste0(round(w,1), " weeks")
  }
  
  # Helper: check ideal-weight milestone availability
  ideal_weeks <- last(df$Weeks_to_Ideal_Weight)
  ideal_dist  <- last(df$Weight_Distance_kg)
  has_ideal   <- !is.na(ideal_weeks) && !is.na(ideal_dist)
  
  # Milestone insight
  milestone_insights <- dplyr::case_when(
    
    trend == "flat" | is.na(trend) | abs(slope_kg_per_week_recent) < 0.01 ~
      "⏲️ Weight change is minimal; cannot project milestone.",
    
    latest_goal_type == "Bulk" & trend == "gain" ~
      paste0(
        "⏲️ ", pretty_weeks(weeks_to_1),
        if (has_ideal) {
          paste0(
            " to gain 1kg, and ",
            r(ideal_weeks, 2), " weeks to gain ",
            r(ideal_dist, 2), "kg."
          )
        } else {
          " to gain 1kg."
        },
        " (based on 3wk average rate of ", r(slope_kg_per_week_recent, 2), "kg/week)"
      ),
    
    latest_goal_type == "Cut" & trend == "loss" ~
      paste0(
        "⏲️ ", pretty_weeks(weeks_to_1),
        if (has_ideal) {
          paste0(
            " to lose 1kg, and ",
            r(ideal_weeks, 2), " weeks to lose ",
            r(ideal_dist, 2), "kg."
          )
        } else {
          " to lose 1kg."
        },
        " (based on 3wk average rate of ", r(slope_kg_per_week_recent, 2), "kg/week)"
      ),
    
    latest_goal_type == "Maintenance" ~
      paste0("3wk average is ", ifelse(trend == "gain", "+", "-"),
             r(abs(slope_kg_per_week_recent), 2),
             " kg/week, but your current goal is Maintenance. No milestone projection applicable."),
    
    latest_goal_type %in% c("Bulk", "Cut") ~
      paste0("Current trend (", trend, ") is inconsistent with your goal of ", latest_goal_type, "."),
    
    TRUE ~ "⏲️ Cannot project milestone for current data/goal."
  )
  
  
  # -----velocity and waist_weight interpretation-----
  velocity_class <- classify_velocity(slope_kg_per_week_profile, latest_weight)
  trend_msg <- paste0("📈 ",trend_interpretation(df))
  
  #Stats
  profile_stats <- list(
    Header_Info = list(
      Profile_Name = first(df$Profile) ,
      Profile_Total_Periods = total_periods,
      Profile_Earliest_Wk_Start_Date = earliest_profile_date,
      Profile_Latest_Wk_Start_Date = latest_profile_date,
      Profile_Total_Weeks = total_weeks
    ),
    Body = list(
      Profile_Earliet_Observed_Weight = r(earliest_weight,2),
      Profile_Latest_Observed_Weight = r(latest_weight,2),
      Profile_Total_Weight_Change = r(total_weight_change,2),
      Profile_Total_Weight_Change_Velocity = velocity_class,
      
      Profile_Earliest_Observed_FFMI = r(first_non_na(df$Observed_FFMI),2),
      Profile_Latest_Observed_FFMI = r(last(na.omit(df$Observed_FFMI)),2),
      
      Profile_Earliest_Observed_BF = r(first_non_na(df$Observed_Avg_BF_perc),2),
      Profile_Latest_Observed_BF = r(last(na.omit(df$Observed_Avg_BF_perc)),2),
      
      Profile_Earliest_Observed_Waist = r(first_non_na(df$Observed_Waist_in),2),
      Profile_Latest_Observed_Waist = r(last(na.omit(df$Observed_Waist_in)),2),
      Profile_Trend_Interpretation = trend_msg
    ),
    Nutrition = list(
      Profile_Earliest_Observed_TDEE = r(first(df$Observed_TDEE_Raw)),
      Profile_Latest_Observed_TDEE = r(last(df$Observed_TDEE_Raw)),
      Profile_TDEE_change = r(last(df$Observed_TDEE_Raw) - first(df$Observed_TDEE_Raw)),
      
      Profile_Avg_Calorie_Intake_g = r(mean(df$Observed_Avg_Daily_Calorie_Intake, na.rm = TRUE),1),
      Profile_Avg_Protein_Intake_g = r(mean(df$Observed_Avg_Daily_Protein_Intake, na.rm = TRUE),1)
    ),
    Training = list(
      Profile_Avg_Sessions = r(mean(df$Observed_Sessions, na.rm = TRUE),0),
      Profile_Avg_SessionDuration = r(mean(df$Observed_Session_Duration_min, na.rm = TRUE),1),
      
      Profile_Avg_Weekly_Training_Sets_Chest = r(mean(df$Weekly_Training_Sets_Chest, na.rm = TRUE),0),
      Profile_Avg_Weekly_Training_Sets_Shoulders = r(mean(df$Weekly_Training_Sets_Shoulders, na.rm = TRUE),0),
      Profile_Avg_Weekly_Training_Sets_Back = r(mean(df$Weekly_Training_Sets_Back, na.rm = TRUE),0),
      Profile_Avg_Weekly_Training_Sets_Triceps = r(mean(df$Weekly_Training_Sets_Triceps, na.rm = TRUE),0),
      Profile_Avg_Weekly_Training_Sets_Biceps = r(mean(df$Weekly_Training_Sets_Biceps, na.rm = TRUE),0),
      Profile_Avg_Weekly_Training_Sets_Legs = r(mean(df$Weekly_Training_Sets_Legs, na.rm = TRUE),0)
    ),
    Lifestyle = list(
      Profile_Avg_Steps = r(mean(df$Observed_Avg_Steps, na.rm = TRUE),0),
      Profile_Avg_Sleep_hr = r(mean(df$Observed_Avg_Sleep_hr, na.rm = TRUE),1)
    )
  )
  
  #insights and commentary
  waist_delta <- last(na.omit(df$Observed_Waist_in)) - first_non_na(df$Observed_Waist_in) #to be used in the profile summary below
  BF_delta <- last(na.omit(df$Observed_Avg_BF_perc)) - first_non_na(df$Observed_Avg_BF_perc) #to be used in the profile summary below
  FFMI_delta <- last(na.omit(df$Observed_FFMI)) - first_non_na(df$Observed_FFMI) #to be used in the profile summary below
  
  profile_insights <- list(
    Profile_Summary = paste0(
      "🚩 Since the start: ",
      
      # Weight change
      ifelse(!is.na(total_weight_change) && total_weight_change >= 0, "+", ""),
      format_stat(total_weight_change, "kg")," in weight", ", ",
      
      # BF% change
      ifelse(!is.na(BF_delta) && BF_delta >= 0, "+", ""),
      format_stat(BF_delta, "%"), " in body fat",  ", ",
      
      # FFMI change
      ifelse(!is.na(FFMI_delta) && FFMI_delta >= 0, "+", ""),
      format_stat(FFMI_delta, ""), " in FFMI", ", and ",
      
      # Waist change
      ifelse(!is.na(waist_delta) && waist_delta >= 0, "+", ""),
      format_stat(waist_delta, "in"), " on the waist. "
    ),
    
    Profile_Trend = paste0("⚡ Long-term trend: ", format_stat(slope_kg_per_week_profile,"kg/week"), " (", velocity_class, ")."),
    
    Profile_Trend_Pattern = trend_msg,
    
    Next_Milestones = milestone_insights
  )
  
  list(profile_stats = profile_stats, profile_insights = profile_insights , plots = list())
}

# -----------------------------
# 3) Main wrapper
# -----------------------------

create_multi_period_analysis <- function(weekly_data_active_profile,
                                         include_plots = TRUE) {
  
  # --- clean data ---
  df <- weekly_data_active_profile %>%
    ungroup() %>%
    safe_date() %>%
    arrange(Week_Start)
  
  if (nrow(df) == 0) {
    return(list(
      Period_Table     = tibble(),
      Profile_Stats    = list(),
      Profile_Insights = list(),
      Profile_Plots    = list()
    ))
  }
  
  # --- (A) compute per-period summary ---
  per_period_summary <- compute_period_summary(df)
  
  # --- (B) compute profile-level stats + insights ---
  profile_res <- compute_profile_summary(df)
  
  # --- (C) plots ---
  profile_plots <- list()
  
  # --- period boundaries for plots ---
  period_boundaries_df <- df %>%
    arrange(Week_Start) %>%
    distinct(Period_Name, .keep_all = TRUE) %>%
    select(Period_Name, Week_Start)
  
  if (isTRUE(include_plots)) {
    
    # Helper function for axis title margins
    axis_margin <- function() theme(
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10))
    )
    
    # Helper function for plot titles
    title_style <- function(size = 12) theme(
      plot.title = element_text(family = "Arial", face = "bold", color = "#2C3E50", size = size, hjust = 0.5)
    )
    
    # Helper function to handle NA values
    safe_max <- function(...) {
      x <- c(...)
      if (all(is.na(x))) return(NA_real_)
      max(x, na.rm = TRUE)
    }
    
    # ------------------------------------------------------
    # A. BODY COMPOSITION
    # ------------------------------------------------------
    max_lbm_y <- safe_max(df$Observed_LBM_Raw_kg, df$Observed_LBM_Smoothed_kg, df$Ideal_LBM_kg)
    profile_plots$LBM <- ggplot(df, aes(x = Week_Start)) +
      {if(any(!is.na(df$Observed_LBM_Raw_kg)))
        geom_line(aes(y = Observed_LBM_Raw_kg, color = "Observed (Raw)"), linewidth = 0.8)
      } +
      {if(any(!is.na(df$Observed_LBM_Raw_kg)))
        geom_point(aes(y = Observed_LBM_Raw_kg, color = "Observed (Raw)"), size = 1.2)
      } +
      {if(any(!is.na(df$Observed_LBM_Smoothed_kg)))
        geom_line(aes(y = Observed_LBM_Smoothed_kg, color = "Observed (3wk Smoothed)"), linetype = "dotted", linewidth = 0.8)
      } +
      {if(any(!is.na(df$Observed_LBM_Smoothed_kg)))
        geom_point(aes(y = Observed_LBM_Smoothed_kg, color = "Observed (3wk Smoothed)"), shape = 18, size = 1.5)
      } +
      geom_line(aes(y = Ideal_LBM_kg, color = "Ideal"), linetype = "dashed", linewidth = 0.9) +
      labs(title = "LBM Over Time (Observed vs Ideal)", y = "kg", x = "Week", color = "",  
           caption = "Notes:\n• The smoothed line is calculated based on the 3wk rolling average of the raw values.\n• The Ideal LBM is calculated based on the Profile's Ideal FFMI and represents the minimum ideal LBM - the higher the FFMI the better.") +
      scale_color_manual(values = c(
        "Observed (Raw)" = "steelblue",
        "Observed (3wk Smoothed)" = "orange",
        "Ideal"    = "darkgreen"
      )) +
      theme_minimal() +
      title_style() +
      axis_margin() +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        # --- caption styling ---
        plot.caption = element_text(
          hjust = 0,        # left-aligned
          color = "gray60", # minimalistic grey
          face = "italic",  # italic
          size = 9,
          lineheight = 1.2)         # small text
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
      geom_vline(aes(xintercept = as.numeric(Week_Start)), data = period_boundaries_df,
                 linetype = "dotted", color = "gray60", linewidth = 0.2) +
      geom_text(data = period_boundaries_df,
                aes(x = Week_Start, y = max_lbm_y * 1.02, label = Period_Name),
                angle = 90, vjust = -0.4, hjust = 0, size = 3, color = "gray60")
    
    max_ffmi_y <- safe_max(df$Observed_FFMI, df$Ideal_FFMI)
    profile_plots$FFMI <- ggplot(df, aes(x = Week_Start)) +
      {if(any(!is.na(df$Observed_FFMI))) geom_line(aes(y = Observed_FFMI, color = "Observed"), linewidth = 0.8)} +
      {if(any(!is.na(df$Observed_FFMI))) geom_point(aes(y = Observed_FFMI, color = "Observed"), size = 1.2)} +
      geom_line(aes(y = Ideal_FFMI, color = "Ideal"), linetype = "dashed", linewidth = 0.9) +
      labs(title = "3wk Smoothed FFMI Over Time (Observed vs Ideal)", y = "FFMI", x = "Week", color = "",
           caption = "Notes:\n• The Ideal FFMI is based on the Profile's Ideal FFMI and drives the minimum Ideal LBM/Weight - the higher the FFMI the better .\n• The Observed FFMI is calculated based on the raw LBM values and the 3wk rolling average of the raw LBM values") +
      scale_color_manual(values = c("Observed" = "steelblue", "Ideal" = "darkgreen")) +
      theme_minimal() +
      title_style() +
      axis_margin() +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        # --- caption styling ---
        plot.caption = element_text(
          hjust = 0,        # left-aligned
          color = "gray60", # minimalistic grey
          face = "italic",  # italic
          size = 9,
          lineheight = 1.2)         # small text
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
      geom_vline(aes(xintercept = as.numeric(Week_Start)), data = period_boundaries_df,
                 linetype = "dotted", color = "gray60", linewidth = 0.2) +
      geom_text(data = period_boundaries_df,
                aes(x = Week_Start, y = max_ffmi_y * 1.02, label = Period_Name),
                angle = 90, vjust = -0.4, hjust = 0, size = 3, color = "gray60")
    
    max_bf_y <- safe_max(df$Observed_Avg_BF_perc, df$Ideal_BF_perc)
    profile_plots$BodyFat <- ggplot(df, aes(x = Week_Start)) +
      {if(any(!is.na(df$Observed_Avg_BF_perc))) geom_line(aes(y = Observed_Avg_BF_perc, color = "Observed"), linewidth = 0.8)} +
      {if(any(!is.na(df$Observed_Avg_BF_perc))) geom_point(aes(y = Observed_Avg_BF_perc, color = "Observed"), size = 1.2)} +
      geom_line(aes(y = Ideal_BF_perc, color = "Ideal"), linetype = "dashed", linewidth = 0.9) +
      labs(title = "Body Fat % Over Time (Observed vs Ideal)", y = "%", x = "Week", color = "",
           caption = "Notes:\n• The Observed BF% may be based on In-Body Analysis readings or AI-assisted estimates."
      ) +
      scale_color_manual(values = c("Observed" = "steelblue", "Ideal" = "darkgreen")) +
      theme_minimal() +
      title_style() +
      axis_margin() +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        # --- caption styling ---
        plot.caption = element_text(
          hjust = 0,        # left-aligned
          color = "gray60", # minimalistic grey
          face = "italic",  # italic
          size = 9,
          lineheight = 1.2)         # small text
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
      geom_vline(aes(xintercept = as.numeric(Week_Start)), data = period_boundaries_df,
                 linetype = "dotted", color = "gray60", linewidth = 0.2) +
      geom_text(data = period_boundaries_df,
                aes(x = Week_Start, y = max_bf_y * 1.02, label = Period_Name),
                angle = 90, vjust = -0.4, hjust = 0, size = 3, color = "gray60")
    
    max_weight_y <- safe_max(df$Observed_Avg_Weight_kg, df$Ideal_Weight_FFMI)
    profile_plots$Weight <- ggplot(df, aes(x = Week_Start)) +
      {if(any(!is.na(df$Observed_Avg_Weight_kg))) geom_line(aes(y = Observed_Avg_Weight_kg, color = "Observed"), linewidth = 0.8)} +
      {if(any(!is.na(df$Observed_Avg_Weight_kg))) geom_point(aes(y = Observed_Avg_Weight_kg, color = "Observed"), size = 1.2)} +
      geom_line(aes(y = Ideal_Weight_FFMI, color = "Ideal"), linetype = "dashed", linewidth = 0.9) +
      labs(title = "Weight Over Time (Observed vs Ideal)", y = "kg", x = "Week", color = "",
           caption = "Notes:\n• The ideal weight is calculated based on the Ideal LBM and the Profile's Ideal BF%. \n• The ideal LBM itself is based on the Profile's Ideal FFMI; the Ideal FFMI drives the minimum Ideal LBM/Weight - the higher the FFMI the better.") +
      scale_color_manual(values = c("Observed" = "steelblue", "Ideal" = "darkgreen")) +
      theme_minimal() +
      title_style() +
      axis_margin() +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        # --- caption styling ---
        plot.caption = element_text(
          hjust = 0,        # left-aligned
          color = "gray60", # minimalistic grey
          face = "italic",  # italic
          size = 9,
          lineheight = 1.2)         # small text
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
      geom_vline(aes(xintercept = as.numeric(Week_Start)), data = period_boundaries_df,
                 linetype = "dotted", color = "gray60", linewidth = 0.2) +
      geom_text(data = period_boundaries_df,
                aes(x = Week_Start, y = max_weight_y * 1.02, label = Period_Name),
                angle = 90, vjust = -0.4, hjust = 0, size = 3, color = "gray60")
    
    max_waist_y <- safe_max(df$Observed_Waist_in, df$Ideal_Waist_in)
    profile_plots$Waist <- ggplot(df, aes(x = Week_Start)) +
      {if(any(!is.na(df$Observed_Waist_in))) geom_line(aes(y = Observed_Waist_in, color = "Observed"), linewidth = 0.8)} +
      {if(any(!is.na(df$Observed_Waist_in))) geom_point(aes(y = Observed_Waist_in, color = "Observed"), size = 1.2)} +
      geom_line(aes(y = Ideal_Waist_in, color = "Ideal"), linetype = "dashed", linewidth = 0.9) +
      labs(title = "Waist Over Time (Observed vs Ideal)", y = "in", x = "Week", color = "",
           caption = "Notes:\n• The Ideal waist is calculated as 45% of the Height.") +
      scale_color_manual(values = c("Observed" = "steelblue", "Ideal" = "darkgreen")) +
      theme_minimal() +
      title_style() +
      axis_margin() +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        # --- caption styling ---
        plot.caption = element_text(
          hjust = 0,        # left-aligned
          color = "gray60", # minimalistic grey
          face = "italic",  # italic
          size = 9,
          lineheight = 1.2)         # small text
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
      geom_vline(aes(xintercept = as.numeric(Week_Start)), data = period_boundaries_df,
                 linetype = "dotted", color = "gray60", linewidth = 0.2) +
      geom_text(data = period_boundaries_df,
                aes(x = Week_Start, y = max_waist_y * 1.02, label = Period_Name),
                angle = 90, vjust = -0.4, hjust = 0, size = 3, color = "gray60")
    
    # ------------------------------------------------------
    # B. NUTRITION
    # ------------------------------------------------------
    max_tdee_y <- safe_max(df$Observed_TDEE_Smoothed, df$Baseline_Period_TDEE)
    profile_plots$TDEE <- ggplot(df, aes(x = Week_Start)) +
      {if(any(!is.na(df$Observed_TDEE_Smoothed))) geom_line(aes(y = Observed_TDEE_Smoothed, color = "Observed"), linewidth = 0.8)} +
      geom_line(aes(y = Baseline_Period_TDEE, color = "Target"), linetype = "dashed", linewidth = 0.9) +
      labs(title = "3wk Smoothed TDEE Over Time (Observed vs Period Target)", y = "kcal", x = "Week", color = "",
           caption = "Notes:\n• The Period Target TDEE is calcualted based on BMR (i.e. period starting weight, age, height) and Activity Factor (i.e. baseline steps and target training intensity)\n• The Observed TDEE is back-calculated from the Observed Weight.") +
      scale_color_manual(values = c("Observed" = "steelblue", "Target" = "darkgreen")) +
      theme_minimal() +
      title_style() +
      axis_margin() +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        # --- caption styling ---
        plot.caption = element_text(
          hjust = 0,        # left-aligned
          color = "gray60", # minimalistic grey
          face = "italic",  # italic
          size = 9,
          lineheight = 1.2)         # small text
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
      geom_vline(aes(xintercept = as.numeric(Week_Start)), data = period_boundaries_df,
                 linetype = "dotted", color = "gray60", linewidth = 0.2) +
      geom_text(data = period_boundaries_df,
                aes(x = Week_Start, y = max_tdee_y * 1.02, label = Period_Name),
                angle = 90, vjust = -0.4, hjust = 0, size = 3, color = "gray60")
    
    max_cal_y <- safe_max(df$Observed_Avg_Daily_Calorie_Intake, df$Target_Calorie_Intake_for_Period)
    profile_plots$Calories <- ggplot(df, aes(x = Week_Start)) +
      {if(any(!is.na(df$Observed_Avg_Daily_Calorie_Intake))) geom_line(aes(y = Observed_Avg_Daily_Calorie_Intake, color = "Observed"), linewidth = 0.8)} +
      geom_line(aes(y = Target_Calorie_Intake_for_Period, color = "Target"), linetype = "dashed", linewidth = 0.9) +
      labs(title = "Calories Over Time (Observed vs Period Target)", y = "kcal", x = "Week", color = "",
           caption = "Notes:\n• The Period Target is calculated based on the Period Target TDEE + an adjustment factor as per the period's Preset Goal Template") +
      scale_color_manual(values = c("Observed" = "steelblue", "Target" = "darkgreen")) +
      theme_minimal() +
      title_style() +
      axis_margin() +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        # --- caption styling ---
        plot.caption = element_text(
          hjust = 0,        # left-aligned
          color = "gray60", # minimalistic grey
          face = "italic",  # italic
          size = 9,
          lineheight = 1.2)         # small text
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
      geom_vline(aes(xintercept = as.numeric(Week_Start)), data = period_boundaries_df,
                 linetype = "dotted", color = "gray60", linewidth = 0.2) +
      geom_text(data = period_boundaries_df,
                aes(x = Week_Start, y = max_cal_y * 1.02, label = Period_Name),
                angle = 90, vjust = -0.4, hjust = 0, size = 3, color = "gray60")
    
    max_prot_y <- safe_max(df$Observed_Avg_Daily_Protein_Intake, df$Target_Protein_Intake_for_Period)
    profile_plots$Protein <- ggplot(df, aes(x = Week_Start)) +
      {if(any(!is.na(df$Observed_Avg_Daily_Protein_Intake))) geom_line(aes(y = Observed_Avg_Daily_Protein_Intake, color = "Observed"), linewidth = 0.8)} +
      geom_line(aes(y = Target_Protein_Intake_for_Period, color = "Target"), linetype = "dashed", linewidth = 0.9) +
      labs(title = "Protein Intake Over Time (Observed vs Period Target)", y = "g", x = "Week", color = "",
           caption = "Notes:\n• The Period Target is calculated based on the period's Preset Goal Template") +
      scale_color_manual(values = c("Observed" = "steelblue", "Target" = "darkgreen")) +
      theme_minimal() +
      title_style() +
      axis_margin() +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        # --- caption styling ---
        plot.caption = element_text(
          hjust = 0,        # left-aligned
          color = "gray60", # minimalistic grey
          face = "italic",  # italic
          size = 9,
          lineheight = 1.2)         # small text
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
      geom_vline(aes(xintercept = as.numeric(Week_Start)), data = period_boundaries_df,
                 linetype = "dotted", color = "gray60", linewidth = 0.2) +
      geom_text(data = period_boundaries_df,
                aes(x = Week_Start, y = max_prot_y * 1.02, label = Period_Name),
                angle = 90, vjust = -0.4, hjust = 0, size = 3, color = "gray60")
    # ------------------------------------------------------
    # C. TRAINING
    # ------------------------------------------------------
    set_data <- df %>%
      select(Week_Start, starts_with("Weekly_Training_Sets_")) %>%
      pivot_longer(-Week_Start, names_to = "Muscle", values_to = "Sets")
    
    muscle_max <- set_data %>%
      group_by(Muscle) %>%
      summarise(max_sets = max(Sets, na.rm = TRUE), .groups = "drop")
    
    label_data <- period_boundaries_df %>%
      crossing(muscle_max)
    
    profile_plots$Training <- ggplot(set_data, aes(x = Week_Start, y = Sets)) +
      geom_line(linewidth = 0.7, color = "black") +
      geom_point(size = 1.2, color = "black") +
      facet_wrap(~ Muscle, scales = "free_y") +
      scale_y_continuous(
        breaks = function(x) {rng <- floor(min(x)):ceiling(max(x));if (length(rng) > 6) rng[seq(1, length(rng), by = 2)] else rng},
        expand = expansion(mult = c(0.05, 0.1))
      ) +
      labs(title = "Training Volume (Sets per Primary Muscle Group)", x = NULL, y = NULL) +
      theme_minimal(base_size = 9) +
      theme(
        plot.title = element_text(family = "Arial", face = "bold", color = "#2C3E50", size = 12, hjust = 0.5),
        panel.grid = element_blank(),
        strip.text = element_text(size = 7, face = "bold"),
        axis.text.x = element_text(size = 6, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))
      ) +
      geom_vline(
        aes(xintercept = as.numeric(Week_Start)),
        data = period_boundaries_df,
        linetype = "dotted",
        color = "gray50",
        linewidth = 0.2
      ) +
      geom_text(
        data = label_data,
        aes(x = Week_Start, y = max_sets * 1.02, label = Period_Name),
        angle = 90, vjust = 0.5, hjust = 0, size = 3, color = "gray40",
        inherit.aes = FALSE
      )
    
    # ------------------------------------------------------
    # D. LIFESTYLE
    # ------------------------------------------------------
    max_sleep_y <- safe_max(df$Observed_Avg_Sleep_hr, df$Baseline_Sleep_hr)
    profile_plots$Sleep <- ggplot(df, aes(x = Week_Start)) +
      {if(any(!is.na(df$Observed_Avg_Sleep_hr))) geom_line(aes(y = Observed_Avg_Sleep_hr, color = "Observed"), linewidth = 0.8)} +
      geom_line(aes(y = Baseline_Sleep_hr, color = "Baseline"), linetype = "dashed", linewidth = 0.9) +
      labs(title = "Sleep Over Time (Observed vs Baseline)", y = "Hours", x = "Week", color = "") +
      scale_color_manual(values = c("Observed" = "steelblue", "Baseline" = "darkgreen")) +
      theme_minimal() +
      title_style() +
      axis_margin() +
      theme(
        legend.position = "bottom",
        
        legend.text = element_text(size = 8)
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
      geom_vline(aes(xintercept = as.numeric(Week_Start)), data = period_boundaries_df,
                 linetype = "dotted", color = "gray60", linewidth = 0.2) +
      geom_text(data = period_boundaries_df,
                aes(x = Week_Start, y = max_sleep_y * 1.02, label = Period_Name),
                angle = 90, vjust = -0.4, hjust = 0, size = 3, color = "gray60")
    
    max_steps_y <- safe_max(df$Observed_Avg_Steps, df$Baseline_Daily_Steps)
    profile_plots$Steps <- ggplot(df, aes(x = Week_Start)) +
      {if(any(!is.na(df$Observed_Avg_Steps))) geom_line(aes(y = Observed_Avg_Steps, color = "Observed"), linewidth = 0.8)} +
      geom_line(aes(y = Baseline_Daily_Steps, color = "Baseline"), linetype = "dashed", linewidth = 0.9) +
      labs(title = "Steps Over Time (Observed vs Baseline)", y = "Steps", x = "Week", color = "") +
      scale_color_manual(values = c("Observed" = "steelblue", "Baseline" = "darkgreen")) +
      theme_minimal() +
      title_style() +
      axis_margin() +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 8)
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
      geom_vline(aes(xintercept = as.numeric(Week_Start)), data = period_boundaries_df,
                 linetype = "dotted", color = "gray60", linewidth = 0.2) +
      geom_text(data = period_boundaries_df,
                aes(x = Week_Start, y = max_steps_y * 1.02, label = Period_Name),
                angle = 90, vjust = -0.4, hjust = 0, size = 3, color = "gray60")
}
  
  # --- (D) final output ---
  list(
    Per_Period_Summary = per_period_summary,
    Profile_Stats    = profile_res$profile_stats,
    Profile_Insights = profile_res$profile_insights,
    Profile_Plots    = profile_plots
  )
}

# ---------------------------
# 4) FINAL PROFILE SUMMARY OBJECT
# ---------------------------
profile_final_summary <- create_multi_period_analysis(weekly_data_active_profile)

# ─────────────────────────────────────────────────────────────
# C) CONSOLIDATED & FINAL OUTPUT OBJECT 
# ─────────────────────────────────────────────────────────────

final_output_raw <- list(
  
  # ===============================
  # 1) STATS
  # ===============================
  Stats = list(
    Current_Week = current_week_final_summary$Current_Week_Stats,
    Profile      = profile_final_summary$Profile_Stats
  ),
  
  # ===============================
  # 2) INSIGHTS
  # ===============================
  Insights = list(
    Current_Week = current_week_final_summary$Current_Week_Insights,
    Profile      = profile_final_summary$Profile_Insights
  ),
  
  # ===============================
  # 3) PLOTS
  # ===============================
  Plots = list(
    Profile      = profile_final_summary$Profile_Plots
  ),
  
  # ===============================
  # 4) RAW DATA (FULL TABLES)
  # ===============================
  Raw_Data = list(
    # underlying data sets
    Weekly_Data_Current_Period = current_week_final_summary$Current_Period_WeeklyData, # contains CURRENT period -> all weeks
    
    Weekly_Data_Active_Profile = weekly_data_active_profile,  # contains ALL periods -> all weeks (for the active profile of selected identity)
    
    Weekly_Data_All_Profiles        = weekly_data, #contains ALL profiles -> all periods -> all weeks) (for the selected identity)
    
    Per_Period_Summary = profile_final_summary$Per_Period_Summary  #aggregated summary (one row per period) of the Weekly_Data_Active_Profile
  )
)

# ============================================================
# D) MASTER OUTPUT BUILDER FOR SHINY
# ============================================================
#Helper: convert stats data sets to tibbles
stats_to_tibbles <- function(stats_section) {
  
  if (!is.list(stats_section)) return(NULL)
  
  purrr::map(stats_section, function(x) {
    
    # Case 1: x is a tibble already → leave it alone
    if (is.data.frame(x)) {
      return(as_tibble(x))
    }
    
    # Case 2: x is a named list of scalars → convert to tibble (row)
    if (is.list(x) && all(purrr::map_lgl(x, ~ length(.x) == 1))) {
      return(as_tibble(x))
    }
    
    # Case 3: x is a list of unknown structure → try safer transpose
    tryCatch(
      purrr::map_df(x, ~ as.data.frame(t(.))),
      error = function(e) as_tibble(list())
    )
  })
}

#final output organizaiton
prepare_final_output <- function(final_output_raw) {
  
  final <- list(
    
    # --------------------------------
    # 1) STATS
    # --------------------------------
    Stats = list(
      Current_Week  = stats_to_tibbles(final_output_raw$Stats$Current_Week),
      Profile = stats_to_tibbles(final_output_raw$Stats$Profile)
    ),
    
    # --------------------------------
    # 2) INSIGHTS
    # --------------------------------
    Insights = list(
      Current_Week  = final_output_raw$Insights$Current_Week,
      Profile = final_output_raw$Insights$Profile
    ),
    
    # --------------------------------
    # 3) PLOTS
    # --------------------------------
    Plots = list(
      Profile = final_output_raw$Plots$Profile
    ),
    
    # --------------------------------
    # 4) RAW DATA
    # --------------------------------
    Raw_Data = final_output_raw$Raw_Data
  )
  
  return(final)
}


#final output for shiny
final_output_processed <- prepare_final_output(final_output_raw)

return(final_output_processed) #final output of the shiny app.R process_all wrapper

}
