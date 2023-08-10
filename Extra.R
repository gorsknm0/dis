# This is for other calculations that have been made but not used



##insert vax if wanted to 'Dataset Creation'
# focusing on vaccines to see how many vaccines children get
total_vax <- rowSums(select(individuals, starts_with('vax_card_')) == 'Yes', na.rm = TRUE) # Calculate the total vaccinations for each row (ignoring NA values)
all_na_rows <- rowSums(is.na(select(individuals, starts_with('vax_card_')))) == ncol(individuals) # Create a logical vector indicating rows where all vaccine columns are NA
total_vax[all_na_rows] <- NA # Assign NA to total_vax for rows with all NA vaccine columns
total_vax[total_vax == 0 & !all_na_rows] <- NA # Replace 0 values with NA if total_vax is 0 and there are no NA columns
individuals$total_vax <- total_vax # Add the total_vax column to the data frame



##insert total_bought to 'Dataset Creation'##
# focusing on how much people spent on water, milk, vegetables, meat, beans, rice, cooking oil
households <- households %>%
  mutate(sec3_3_a = sec3_3_a >= 1,
         sec3_3_b = sec3_3_b >= 1,
         sec3_3_c = sec3_3_c >= 1,
         sec3_3_d = sec3_3_d >= 1,
         sec3_3_e = sec3_3_e >= 1,
         sec3_3_f = sec3_3_f >= 1,
         sec3_3_g = sec3_3_g >= 1) # make the variables boolean
households <- households %>%
  mutate(sec3_3_a = ifelse(sec3_3_a == 0, FALSE, sec3_3_a), 
         sec3_3_b = ifelse(sec3_3_b == 0, FALSE, sec3_3_b), 
         sec3_3_c = ifelse(sec3_3_c == 0, FALSE, sec3_3_c), 
         sec3_3_d = ifelse(sec3_3_d == 0, FALSE, sec3_3_d), 
         sec3_3_e = ifelse(sec3_3_e == 0, FALSE, sec3_3_e), 
         sec3_3_f = ifelse(sec3_3_f == 0, FALSE, sec3_3_f), 
         sec3_3_g = ifelse(sec3_3_g == 0, FALSE, sec3_3_g)) #add false answers
households <- households %>%
  mutate(total_bought = ifelse(!is.na(sec3_3_a),
                               rowSums(select(., sec3_3_a:sec3_3_g) == TRUE, na.rm = TRUE), NA)) # create a total_bought column
households <- households  %>%
  group_by(household_id) %>%
  mutate(
    has_non_na_value = any(!is.na(total_bought)),
    representative_total_bought = ifelse(has_non_na_value, max(total_bought, na.rm = TRUE), NA)
  ) %>%
  ungroup() %>%
  mutate(
    total_bought = ifelse(!is.na(representative_total_bought), representative_total_bought, total_bought),
    total_bought = ifelse(has_non_na_value & is.na(total_bought), representative_total_bought, total_bought)
  ) %>%
  select(-has_non_na_value, -representative_total_bought) # assign the number of total_bought to all of those in the household




##insert total_assets to 'Dataset Creation'##
# determine the total number of assets for a household
households <- households %>%
  mutate(sec2_q1_3a = sec2_q1_3a >= 1,
         sec2_q1_7a = sec2_q1_7a >= 1,
         sec2_q1_8a = sec2_q1_8a >= 1,
         sec2_q1_9a = sec2_q1_9a >= 1) #make the numeric boolean for the assets section
households <- households %>%
  mutate(sec2_q1_1a = ifelse(sec2_q1 == 0, FALSE, sec2_q1_1a), 
         sec2_q1_2a = ifelse(sec2_q1 == 0, FALSE, sec2_q1_2a), 
         sec2_q1_3a = ifelse(sec2_q1 == 0, FALSE, sec2_q1_3a), 
         sec2_q1_4a = ifelse(sec2_q1 == 0, FALSE, sec2_q1_4a), 
         sec2_q1_5a = ifelse(sec2_q1 == 0, FALSE, sec2_q1_5a), 
         sec2_q1_6a = ifelse(sec2_q1 == 0, FALSE, sec2_q1_6a), 
         sec2_q1_7a = ifelse(sec2_q1 == 0, FALSE, sec2_q1_7a), 
         sec2_q1_8a = ifelse(sec2_q1 == 0, FALSE, sec2_q1_8a), 
         sec2_q1_9a = ifelse(sec2_q1 == 0, FALSE, sec2_q1_9a), 
         sec2_q1_10a = ifelse(sec2_q1 == 0, FALSE, sec2_q1_10a), 
         sec2_q1_11a = ifelse(sec2_q1 == 0, FALSE, sec2_q1_11a), 
         sec2_q1_12a = ifelse(sec2_q1 == 0, FALSE, sec2_q1_12a)) #add false answers
households <- households %>%
  mutate(total_assets = ifelse(!is.na(sec2_q1),
                               rowSums(select(., sec2_q1_1a:sec2_q1_12a) == TRUE, na.rm = TRUE), NA)) # create a total_assets column
households <- households  %>%
  group_by(household_id) %>%
  mutate(
    has_non_na_value = any(!is.na(total_assets)),
    representative_total_assets = ifelse(has_non_na_value, max(total_assets, na.rm = TRUE), NA)
  ) %>%
  ungroup() %>%
  mutate(
    total_assets = ifelse(!is.na(representative_total_assets), representative_total_assets, total_assets),
    total_assets = ifelse(has_non_na_value & is.na(total_assets), representative_total_assets, total_assets)
  ) %>%
  select(-has_non_na_value, -representative_total_assets) # assign the number of total_assets to all of those in the household



## insert breastfeeding to 'Dataset Creation'
# Group the data by household and check if any individual in the household has 
# "Yes" for breastfeeding
data <- data %>% 
  group_by(household_id) %>% 
  mutate(breastfeeding_status = ifelse(any(breastfeeding == "Yes", na.rm = TRUE), "Yes", ifelse(all(is.na(breastfeeding)), NA, "No"))) %>% ungroup()

# Replace NAs in the new column with "No"
my_data$Breastfeeding_Status[is.na(my_data$Breastfeeding_Status)] <- "No"

# side note: sec2_q1_10a, sec2_q1_11a, sec2_q1_12a are all NA