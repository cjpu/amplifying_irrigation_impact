# header ------------------------------------------------------------------

# Author: Christine Pu
# Date Created: Oct 9, 2023
# Purpose: Clean variables of interest
# Inputs: raw_data.rds
# Outputs: clean_data.rds

# load packages -----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(here)

# import data -------------------------------------------------------------

ethiopia_baseline_selected_vars_raw <- readRDS(here("data", "raw_data.rds"))

# filter for eligible households ------------------------------------------

ethiopia_baseline_selected_vars_raw_rural_cultivators <- 
  ethiopia_baseline_selected_vars_raw %>% 
  
  filter(
    
    # filter for eligibility criteria
    
    hh_member_yn == "Yes, respondent is household member" &
      eighteen_older == "Yes" &
      able_to_answer == "1. Yes" &
      consent_obtained == "Yes" &
      
    # filter for rural households that grew crops last year  
      
      rur_urb == "RURAL" &
      grew_crops == "Yes" 
      
  ) %>% 
  
  # remove eligibility variables
  
  select(
    - hh_member_yn,
    - eighteen_older, 
    - able_to_answer, 
    - consent_obtained,
    - rur_urb,
    - grew_crops
  )  

# reclassify "other" and "don't know" survey responses -------------------------------------

ethiopia_baseline_selected_vars_raw_rural_cultivators[ethiopia_baseline_selected_vars_raw_rural_cultivators$other_soldt_en_1 == "Households",]$sold_to_1 <- "1. To HOUSEHOLD(S) or NEIGHBOR(S)"
ethiopia_baseline_selected_vars_raw_rural_cultivators[ethiopia_baseline_selected_vars_raw_rural_cultivators$other_soldt_en_2 == "Households",]$sold_to_2 <- "1. To HOUSEHOLD(S) or NEIGHBOR(S)"
ethiopia_baseline_selected_vars_raw_rural_cultivators[ethiopia_baseline_selected_vars_raw_rural_cultivators$other_soldt_en_2 == "Merchants and households (we have sold it for everyone interested)",]$sold_to_2 <- "1. To HOUSEHOLD(S) or NEIGHBOR(S)"
ethiopia_baseline_selected_vars_raw_rural_cultivators[ethiopia_baseline_selected_vars_raw_rural_cultivators$other_soldt_en_3 == "Households",]$sold_to_3 <- "1. To HOUSEHOLD(S) or NEIGHBOR(S)"
ethiopia_baseline_selected_vars_raw_rural_cultivators[ethiopia_baseline_selected_vars_raw_rural_cultivators$other_csp_en_1 == "Cooperatives",]$crop_sold_place_1 <- NA
ethiopia_baseline_selected_vars_raw_rural_cultivators[(is.na(ethiopia_baseline_selected_vars_raw_rural_cultivators$money_spent_1) == FALSE) & (ethiopia_baseline_selected_vars_raw_rural_cultivators$money_spent_1 == -88),]$money_spent_1 <- NA
ethiopia_baseline_selected_vars_raw_rural_cultivators[(is.na(ethiopia_baseline_selected_vars_raw_rural_cultivators$money_spent_2) == FALSE) & (ethiopia_baseline_selected_vars_raw_rural_cultivators$money_spent_2 == -88),]$money_spent_2 <- NA
ethiopia_baseline_selected_vars_raw_rural_cultivators[(is.na(ethiopia_baseline_selected_vars_raw_rural_cultivators$amount_earned_3) == FALSE) & (ethiopia_baseline_selected_vars_raw_rural_cultivators$amount_earned_3 == -88),]$amount_earned_3 <- NA
ethiopia_baseline_selected_vars_raw_rural_cultivators[ethiopia_baseline_selected_vars_raw_rural_cultivators$farmers_planted == "-88. Do not know",]$farmers_planted <- NA

# create new and modify existing variables ----------------------------------

ethiopia_baseline_selected_vars_cleaned_rural_cultivators <- 
  ethiopia_baseline_selected_vars_raw_rural_cultivators %>% 
  mutate(
    
    # create binary variable indicating whether households used irrigation on each plot in the dry season
    
    irrigate_dry_1 = ifelse(
      parcel_wtr_src_dry_1 == "", "did not plant in dry", ifelse(
        parcel_wtr_src_dry_1 == "-77", 0, 1 
      )
    ),
    
    irrigate_dry_2 = ifelse(
      parcel_wtr_src_dry_2 == "", "did not plant in dry", ifelse( 
        parcel_wtr_src_dry_2 == 96, 0, ifelse( # this "other" response translates to "household does not water because the soil is wet"
          parcel_wtr_src_dry_2 == "-77", 0, 1 
        )
      )
    ),
    
    irrigate_dry_3 = ifelse(
      parcel_wtr_src_dry_3 == "", "did not plant in dry", ifelse(
        parcel_wtr_src_dry_3 == 96, 0, ifelse( # this "other" response translates to "household does not water because the soil is wet"
          parcel_wtr_src_dry_3 == "-77", 0, 1 
        )
      )
    ),
    
    # create binary variable indicating whether households used irrigation for any plot in the dry season
    
    irrigate_any_dry = factor(
      ifelse( 
        irrigate_dry_1 == 1 | irrigate_dry_2 == 1 | irrigate_dry_3 == 1, 1, 0 
        ) # 0 NAs
      ),

    # create continuous variable indicating area planted on each plot in the rainy season (acres)
    
    acres_planted_rainy_1 = ifelse(
      parcel_area_unit_1 == "2. Timad", parcel_area_val_1 * 0.62, ifelse( # 1 timad / gemed / qada = 1/4 hectares = 0.62 acres (world bank, FAO standard)
        parcel_area_unit_1 == "3. Gemed", parcel_area_val_1 * 0.62, ifelse( 
          parcel_area_unit_1 == "4. Qada", parcel_area_val_1 * 0.62, ifelse( 
            parcel_area_unit_1 == "7. Response is in dimensions (meters)", plw_length_1 * plw_width_1 * 0.00025, ifelse( # 1 square meter = 0.00025 acres
              parcel_area_unit_1 == "", 0, NA
            )
          )
        )
      ) 
    ),
    
    acres_planted_rainy_2 = ifelse(
      parcel_area_unit_2 == "2. Timad", parcel_area_val_2 * 0.62, ifelse( 
        parcel_area_unit_2 == "3. Gemed", parcel_area_val_2 * 0.62, ifelse( 
          parcel_area_unit_2 == "4. Qada", parcel_area_val_2 * 0.62, ifelse( 
            parcel_area_unit_2 == "7. Response is in dimensions (meters)", plw_length_2 * plw_width_2 * 0.00025, ifelse( 
              parcel_area_unit_2 == "", 0, NA
            )
          )
        ) 
      )
    ),
    
    acres_planted_rainy_3 = ifelse(
      parcel_area_unit_3 == "2. Timad", parcel_area_val_3 * 0.62, ifelse( 
        parcel_area_unit_3 == "3. Gemed", parcel_area_val_3 * 0.62, ifelse( 
          parcel_area_unit_3 == "4. Qada", parcel_area_val_3 * 0.62, ifelse(
            parcel_area_unit_3 == "5. Gezim", parcel_area_val_3 * 0.62, ifelse(
              parcel_area_unit_3 == "7. Response is in dimensions (meters)", plw_length_3 * plw_width_3 * 0.00025, ifelse(
                parcel_area_unit_3 == "", 0, NA 
              )
            )
          )
        )
      )
    ),
    
    # create continuous variable indicating area planted on each plot in the dry season (acres)
    
    acres_planted_dry_1 = ifelse(
      dry_ssn_area_unit_1 == "2. Timad", dry_ssn_area_val_1 * 0.62, ifelse( 
        dry_ssn_area_unit_1 == "3. Gemed", dry_ssn_area_val_1 * 0.62, ifelse( 
          dry_ssn_area_unit_1 == "4. Qada", dry_ssn_area_val_1 * 0.62, ifelse( 
              dry_ssn_area_unit_1 == "", 0, NA 
            )
          )
        )
      ),
    
    acres_planted_dry_2 = ifelse(
      dry_ssn_area_unit_2 == "2. Timad", dry_ssn_area_val_2 * 0.62, ifelse( 
        dry_ssn_area_unit_2 == "3. Gemed", dry_ssn_area_val_2 * 0.62, ifelse( 
          dry_ssn_area_unit_2 == "4. Qada", dry_ssn_area_val_2 * 0.62, ifelse( 
            dry_ssn_area_unit_2 == "7. Response is in dimensions (meters)", dslw_length_2 * dslw_width_2 * 0.00025, ifelse( 
              dry_ssn_area_unit_2 == "", 0, NA 
            )
          )
        )
      )
    ),
    
    acres_planted_dry_3 = ifelse(
      dry_ssn_area_unit_3 == "2. Timad", dry_ssn_area_val_3 * 0.62, ifelse( 
        dry_ssn_area_unit_3 == "3. Gemed", dry_ssn_area_val_3 * 0.62, ifelse( 
          dry_ssn_area_unit_3 == "4. Qada", dry_ssn_area_val_3 * 0.62, ifelse(
              dry_ssn_area_unit_3 == "7. Response is in dimensions (meters)", dslw_length_3 * dslw_width_3 * 0.00025, ifelse(
                dry_ssn_area_unit_3 == "", 0, NA 
              )
            )
          )
        )
    ), 
     
    # create continuous variable indicating total acres planted in the rainy season

    total_acres_planted_rainy = acres_planted_rainy_1 + acres_planted_rainy_2 + acres_planted_rainy_3,

    # create continuous variable indicating total area planted in the dry season

    total_acres_planted_dry = acres_planted_dry_1 + acres_planted_dry_2 + acres_planted_dry_3,

    # assign NAs to illogical responses re: total area planted in the rainy season

    total_acres_planted_rainy_updated = ifelse(
      total_acres_planted_rainy == 0 & 
        (planting_season_1 == "1. Planted crops in the RAINY season only" | 
        planting_season_2 == "1. Planted crops in the RAINY season only" | 
        planting_season_3 == "1. Planted crops in the RAINY season only" | 
        planting_season_1 == "3. Planted crops in RAINY AND DRY seasons" |
        planting_season_2 == "3. Planted crops in RAINY AND DRY seasons" |
        planting_season_3 == "3. Planted crops in RAINY AND DRY seasons"), NA, total_acres_planted_rainy 
    ),

    # assign NAs to illogical responses re: total area planted in the dry season
    
    total_acres_planted_dry_updated = ifelse(
      total_acres_planted_dry == 0 & 
        (planting_season_1 == "2. Planted crops in the DRY season only" | 
           planting_season_2 == "2. Planted crops in the DRY season only" | 
           planting_season_3 == "2. Planted crops in the DRY season only" | 
           planting_season_1 == "3. Planted crops in RAINY AND DRY seasons" |
           planting_season_2 == "3. Planted crops in RAINY AND DRY seasons" |
           planting_season_3 == "3. Planted crops in RAINY AND DRY seasons"), NA, total_acres_planted_dry
    ),

    # create continuous variable indicating total acres planted on an annual basis

    total_acres_planted_annual = total_acres_planted_rainy_updated + total_acres_planted_dry_updated,
    
    # create continuous variable indicating total number of crops planted in the dry season only
    
    total_num_crops_planted_dry_only = rowSums(
      ethiopia_baseline_selected_vars_raw_rural_cultivators %>% select(barley_ssn:rue_ssn) == "2. Dry", na.rm = TRUE
    ),
    
    # create continuous variable indicating total number of crops planted in both the rain and dry season
    
    total_num_crops_planted_both_ssn = rowSums(
      ethiopia_baseline_selected_vars_raw_rural_cultivators %>% select(barley_ssn:rue_ssn) == "3. Both", na.rm = TRUE
    ),
    
    # create continuous variable indicating total number of crops planted in the dry season (including those that were planted in both seasons)
    
    total_num_crops_planted_dry_both_ssn = total_num_crops_planted_dry_only + total_num_crops_planted_both_ssn,
    
    # create continuous variable indicating gross revenue from each of the 3 top earning crops
    
    crop_rev_top1 = as.numeric(
      ifelse(
        revenue_type_1 == "1. Gross revenue (total sales)", amount_earned_1, ifelse(
          revenue_type_1 == "2. Net revenue (total sales – expenses)", amount_earned_1 + money_spent_1, ifelse(
            revenue_type_1 == "", NA, "error" 
          )
        )
      )
    ),
    
    crop_rev_top2 = as.numeric(
      ifelse(
        revenue_type_2 == "1. Gross revenue (total sales)", amount_earned_2, ifelse( 
          revenue_type_2 == "2. Net revenue (total sales – expenses)", amount_earned_2 + money_spent_2, ifelse(
            revenue_type_2 == "", NA, "error"
          )
        )
      )
    ),
    
    crop_rev_top3 = as.numeric(
      ifelse(
        revenue_type_3 == "1. Gross revenue (total sales)", amount_earned_3, ifelse( 
          revenue_type_3 == "2. Net revenue (total sales – expenses)", amount_earned_3 + money_spent_3, ifelse(
            revenue_type_3 == "", NA, "error"
          )
        )
      )
    ),

    # create continuous variable indicating total gross revenue earned from top 3 crops
    
    crop_rev_total = as.numeric(
      ifelse(
        is.na(crop_rev_top1) == TRUE & is.na(crop_rev_top2) == TRUE & is.na(crop_rev_top3) == TRUE, NA, ifelse(
          is.na(crop_rev_top1) == FALSE & is.na(crop_rev_top2) == TRUE & is.na(crop_rev_top3) == TRUE, crop_rev_top1, ifelse(
            is.na(crop_rev_top1) == FALSE & is.na(crop_rev_top2) == FALSE & is.na(crop_rev_top3) == TRUE, crop_rev_top1 + crop_rev_top2, ifelse(
              is.na(crop_rev_top1) == FALSE & is.na(crop_rev_top2) == FALSE & is.na(crop_rev_top3) == FALSE, crop_rev_top1 + crop_rev_top2 + crop_rev_top3, "error" 
              ) 
            )
          )
        )
      ),
    
    # create categorical variable indicating where the household sold most of their top earning crops
    
    crop_sold_place_1 = case_when(
      crop_sold_place_1 == "1. At respondent’s garden / home" ~ "garden",
      crop_sold_place_1 == "2. At LOCAL market / trading center" ~ "local market",
      crop_sold_place_1 == "3. At MAJOR / CITY market" ~ "major market"
    ),
    
    crop_sold_place_2 = case_when(
      crop_sold_place_2 == "1. At respondent’s garden / home" ~ "garden",
      crop_sold_place_2 == "2. At LOCAL market / trading center" ~ "local market",
      crop_sold_place_2 == "3. At MAJOR / CITY market" ~ "major market"
    ),
    
    crop_sold_place_3 = case_when(
      crop_sold_place_3 == "1. At respondent’s garden / home" ~ "garden",
      crop_sold_place_3 == "2. At LOCAL market / trading center" ~ "local market",
      crop_sold_place_3 == "3. At MAJOR / CITY market" ~ "major market"
    ),
    
    # create categorical variable indicating to whom the household sold most of their top earning crops
    
    sold_to_1 = case_when(
      sold_to_1 == "1. To HOUSEHOLD(S) or NEIGHBOR(S)" ~ "households",
      sold_to_1 == "2. To TRADER(S)" ~ "trader",
      sold_to_1 == "3. To COOPERATIVE(S)" ~ "coops",
      sold_to_1 == "4. To MANUFACTURER(S) or INSTITUTION(S) (e.g., private company, restaurants, schools)" ~ "institutions",
    ),
    
    sold_to_2 = case_when(
      sold_to_2 == "1. To HOUSEHOLD(S) or NEIGHBOR(S)" ~ "households",
      sold_to_2 == "2. To TRADER(S)" ~ "trader",
      sold_to_2 == "3. To COOPERATIVE(S)" ~ "coops",
      sold_to_2 == "4. To MANUFACTURER(S) or INSTITUTION(S) (e.g., private company, restaurants, schools)" ~ "institutions",
    ),
    
    sold_to_3 = case_when(
      sold_to_3 == "1. To HOUSEHOLD(S) or NEIGHBOR(S)" ~ "households",
      sold_to_3 == "2. To TRADER(S)" ~ "trader",
      sold_to_3 == "3. To COOPERATIVE(S)" ~ "coops",
      sold_to_3 == "4. To MANUFACTURER(S) or INSTITUTION(S) (e.g., private company, restaurants, schools)" ~ "institutions",
    ),
    
    # create continuous variable indicating the walking time to the nearest paved road in hours
    
    time_paved_road_h = time_paved_road_mins / 60,
    
    # create continuous variable indicating the walking time to the most frequently visited market in hours
    
    time_primary_market_h = time_primary_market_mins / 60,
    
    # create binary variable indicating whether the household owns any mobile phones
    
    owns_mobile_phone = as.factor(
      ifelse(
        mobile_phone == "Yes", 1, ifelse(
          mobile_phone == "No", 0, "error"
          )
      )
    ),
    
    # rename variable
    
    irrig_prev_village = farmers_planted, 
    
    # create binary variable indicating whether the self-reported prevalence of irrigators in the household's village is 50% or more
  
    irrig_prev_50plus = as.factor(
      ifelse(
        is.na(irrig_prev_village) == TRUE, NA, ifelse(
          irrig_prev_village == "1. Very few? (<10%)" | irrig_prev_village == "2. Less than half ?(25-33%)", 0, ifelse(
            irrig_prev_village == "3. About half? (~50%)" | irrig_prev_village == "4. More than half? (66-75%)" | irrig_prev_village == "5. Almost all / All? (>90%)", 1, "error" 
          )
        )
      )
    ),
    
    # create binary variable indicating whether the household hired wage labor
    
    labour = as.factor(
      ifelse(
        labour == "0. No", 0, 1
      )
    ),
    
    # create binary variable indicating whether the household used purchased seeds from formal sellers
    
    seeds = as.factor(
      ifelse(
        seeds == "0. No", 0, 1
      )
    ),
    
    # create binary variable indicating whether the household used animal waste to fertilize
    
    animal_waste = as.factor(
      ifelse(
        animal_waste == "0. No", 0, 1
      )
    ),
    
    # create binary variable indicating whether the household used synthetic fertilizer
    
    chemical_synthetic = as.factor(
      ifelse(
        chemical_synthetic == "0. No", 0, 1
      )
    ),
    
    # create binary variable indicating whether the household used pesticides
    
    pesticides = as.factor(
      ifelse(
        pesticides == "0. No", 0, 1
      )
    ),
    
    # create binary variable indicating whether the household used an ox plough
    
    ox_plough = as.factor(
      ifelse(
        ox_plough == "0. No", 0, 1
      )
    ),
    
    # create binary variable indicating whether the household used a tractor
    
    tractor = as.factor(
      ifelse(
        tractor == "0. No", 0, 1
      )
    ),
    
    # create binary variable indicating whether the household used a thresher
    
    thresher = as.factor(
      ifelse(
        thresher == "0. No", 0, 1
      )
    ),
    
    # create binary variable indicating whether more than 2,000 Birr was borrowed
    
    borrowed_2000_more = as.factor(
      ifelse(
        money_borrowed == "0. No: Household did not obtain a loan of Birr 2,000 or more", 0, ifelse(
          money_borrowed == "1. Yes: Household obtained a loan of Birr 2,000 or more", 1, ifelse(
            money_borrowed == "-88. Do not know", NA, "error" #
          )
        )
      )
    ),
    
    # create binary variable indicating whether the household earned anything from non-farm income activities
    
    nonfarm_livelihood = as.factor(
      ifelse(
        nonfarm_salaried_yn == "Yes" | nonfarm_skilled_yn == "Yes" | nonfarm_unskilled_yn == "Yes" | nonfarm_wholesale_yn == "Yes" | nonfarm_retail_yn == "Yes" | nonfarm_military_yn == "Yes", 1, 0
      )
    ),
    
    # create binary variable indicating whether manual or non-manual irrigation technologies were used 
    
    nonmanual_irrig = as.factor(
      ifelse(
        fuel_usage_petroldiesel == "Yes", 1, ifelse(
          fuel_usage_nofuelmanual == "Yes" | fuel_usage_nofuelgravity == "Yes", 0, ifelse(
            fuel_usage == "", NA, "error" 
            )
        )
      )
    ),
    
    # create binary variable indicating whether the household used a walking vs. non-walking mode of transportation on their last trip to the market (with their harvest)
    
    most_used_trans_market_nonwalking = ifelse( 
      market_trans_mode == "", NA, ifelse( 
        market_trans_mode == "1. Walking by foot" | market_trans_mode == "2. Walking by foot with harvest on an animal/livestock" | (market_trans_mode == "96. Other (specify)" & (other_mtm_en == "A hired donkey")), 0, 1 
      ) 
    ),
    
    # create binary variable indicating whether the household owns any transportation vehicles
    
    owns_any_vehicle = ifelse(
      bicycle_yn == "Yes" | motorcycle_yn == "Yes" | animal_cart_yn == "Yes" | car_truck_yn == "Yes" | tricycle == "Yes", 1, 0
    ), 
    
    # create binary variable indicating whether the household's most earning work comes from nonfarm activities
    
    most_earning_is_nonfarm = ifelse(
      most_earning_work == "Growing crops?" | most_earning_work == "Raising livestock?" | most_earning_work == "Working on someone else's farm?" | (most_earning_work == "Other type of work (specify)" & (other_mew_en == "Selling Eucalyptus tree" | other_mew_en == "Selling wood" | other_mew_en == "Selling Buckthorn leaves" | other_mew_en == "From sharecropped land" | other_mew_en == "From Sharecropped land" | other_mew_en == "Merchant- selling vegetables and buckthorn leaves")), 0, 1 
    ),
    
    # create binary variable indicating whether the household received remittances
    
    remittances = ifelse(
      sent_money == "Yes", 1, ifelse(
        sent_money == "No", 0, NA 
        )
      )
    ) %>%

  # remove variables that aren't being used for downstream analyses

  select(
    - other_soldt_en_1,
    - other_soldt_en_2,
    - other_soldt_en_3,
    - other_csp_en_1,
    - parcel_wtr_src_dry_1,
    - parcel_wtr_src_dry_2,
    - parcel_wtr_src_dry_3,
    - irrigate_dry_1,
    - irrigate_dry_2,
    - irrigate_dry_3,
    - parcel_area_unit_1,
    - parcel_area_unit_2,
    - parcel_area_unit_3,
    - dry_ssn_area_unit_1,
    - dry_ssn_area_unit_2,
    - dry_ssn_area_unit_3,
    - parcel_area_val_1,
    - parcel_area_val_2,
    - parcel_area_val_3,
    - dry_ssn_area_val_1,
    - dry_ssn_area_val_2,
    - dry_ssn_area_val_3,
    - plw_width_1,
    - plw_width_2,
    - plw_width_3,
    - dslw_width_1,
    - dslw_width_2,
    - dslw_width_3,
    - plw_length_1,
    - plw_length_2,
    - plw_length_3,
    - dslw_length_1,
    - dslw_length_2,
    - dslw_length_3,
    - acres_planted_rainy_1,
    - acres_planted_rainy_2,
    - acres_planted_dry_3,
    - total_acres_planted_rainy,
    - total_acres_planted_dry,
    - total_acres_planted_rainy_updated,
    - total_acres_planted_dry_updated,
    - revenue_type_1,
    - revenue_type_2,
    - revenue_type_3,
    - amount_earned_1,
    - amount_earned_2,
    - amount_earned_3,
    - money_spent_1,
    - money_spent_2,
    - money_spent_3,
    - crop_rev_top1,
    - crop_rev_top2,
    - crop_rev_top3,
    - time_paved_road_mins,
    - time_primary_market_mins,
    - mobile_phone,
    - money_borrowed,
    - nonfarm_salaried_yn,
    - nonfarm_skilled_yn,
    - nonfarm_unskilled_yn,
    - nonfarm_wholesale_yn,
    - nonfarm_retail_yn,
    - nonfarm_military_yn,
    - fuel_usage_petroldiesel,
    - fuel_usage_nofuelmanual,
    - fuel_usage_nofuelgravity,
    - fuel_usage,
    - market_trans_mode,
    - other_mtm_en,
    - bicycle_yn,
    - motorcycle_yn,
    - animal_cart_yn,
    - car_truck_yn,
    - tricycle,
    - other_mew_en,
    - sent_money
  )
    
# only keep households in irrigating EAs to reduce confounding effects ----------------------------------

irrigating_eas <- ethiopia_baseline_selected_vars_cleaned_rural_cultivators %>% 
  filter(irrigate_any_dry == 1) %>% 
  group_by(ea) %>% 
  summarise(n()) %>% 
  pull(ea)

ethiopia_baseline_selected_vars_cleaned_rural_cultivators_irrigating_eas <- ethiopia_baseline_selected_vars_cleaned_rural_cultivators %>% 
  filter(ethiopia_baseline_selected_vars_cleaned_rural_cultivators$ea %in% irrigating_eas)

# merge socioeconomic data ---------------------------------

ethiopia_listing_poverty_cleaned <- readRDS(here("data", "clean_socioeconomic_data.rds")) %>% # more information on this data set can be found here: https://github.com/cjpu/comparing_poverty_measurement_approaches/tree/main
  rename(
    hhid = HHID,
    woreda = district,
  ) %>% 
  select(
    hhid,
    woreda,
    num_hh_member_5_to_65yrs,
    fem_hoh,
    mbr_edu_lvl_1,
    ppi_prob_1.90
  ) %>% 
  
  # create binary variable indicating whether the head of household attended any formal schooling
  
  mutate(
    hoh_any_formal_school = ifelse(
      is.na(mbr_edu_lvl_1) == TRUE, NA, ifelse(
        mbr_edu_lvl_1 == "None: No formal/regular schooling" | mbr_edu_lvl_1 == "Spiritual education to become Priest" | mbr_edu_lvl_1 == "Adult literacy program", 0, 1
      )
    ),
    
    fem_hoh = as.factor(fem_hoh)
    
  ) %>% 
  select(
    -mbr_edu_lvl_1
  )

ethiopia_baseline_selected_vars_cleaned_rural_cultivators_irrigating_eas_joined <- left_join(
  ethiopia_baseline_selected_vars_cleaned_rural_cultivators_irrigating_eas,
  ethiopia_listing_poverty_cleaned,
  by = c("hhid", "woreda")
) 

# irrigation: export data -------------------------------------------------------------

saveRDS(ethiopia_baseline_selected_vars_cleaned_rural_cultivators_irrigating_eas_joined, here("data", "clean_data.rds"))