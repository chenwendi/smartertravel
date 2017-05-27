source("0-init.R")

#=== load initial transaction data
#transactions <- read.csv(file.path(wd_data, "assignment_data.csv"))
#saveRDS(transactions, file.path(wd_data, "transactions.rds"))
#transactions <- readRDS(file.path(wd_data, "transactions.rds"))

#=== create dataset with balanced classes ===
set.seed(10)
balanced_data <- transactions %>% 
  group_by(search_id) %>%
  dplyr::summarize(number_results = length(unique(search_result_id)),
                   was_clicked = sum(ad_click) > 0) %>%
  group_by(was_clicked) %>%
  sample_n(size = 120000, replace = F) %>%    # there are just over 120k searches that have been clicked in the dataset
  ungroup()
#saveRDS(balanced_data, file.path("balanced_data.rds")) #balanced_data <- readRDS(file.path("balanced_data.rds"))

#=== format data ===
balanced_data <- balanced_data %>% left_join(transactions, by="search_id")
balanced_data$check_in_date <- as.Date(balanced_data$check_in_date)
balanced_data$check_out_date <- as.Date(balanced_data$check_out_date)
balanced_data$search_timestamp <- as.POSIXct(balanced_data$search_timestamp, tz = "UTC")
balanced_data$user_id <- NULL      #data incomplete
balanced_data$session_id <- NULL   #data incomplete
all_nav_strings <- unique(balanced_data$user_location_navigation_string)
parsed_nav_strings <- bind_rows(lapply(all_nav_strings, parse_location))
names(parsed_nav_strings) <- paste0("user_loc_", names(parsed_nav_strings))
parsed_nav_strings$user_location_navigation_string <- all_nav_strings
saveRDS(parsed_nav_strings, file.path(wd_data, "user_nav_strings.rds"))
balanced_data <- balanced_data %>% left_join(parsed_nav_strings, by="user_location_navigation_string")
balanced_data$user_location_navigation_string <- NULL

all_nav_strings <- unique(balanced_data$dest_location_navigation_string)
parsed_nav_strings <- bind_rows(lapply(all_nav_strings, parse_location))
names(parsed_nav_strings) <- paste0("dest_loc_", names(parsed_nav_strings))
parsed_nav_strings$dest_location_navigation_string <- all_nav_strings  
saveRDS(parsed_nav_strings, file.path(wd_data, "dest_nav_strings.rds"))
balanced_data <- balanced_data %>% left_join(parsed_nav_strings, by="dest_location_navigation_string")
balanced_data$dest_location_navigation_string <- NULL
#saveRDS(balanced_data, file.path(wd_data, "balanced_data_formatted.rds"))  

#=== generate features ===
# balanced_data <- readRDS(file.path(wd_data, "balanced_data_formatted.rds"))
dataset <- balanced_data
a <- Sys.time()
set.seed(12)
search_ids <- sample(unique(dataset$search_id))[1:240000]
statuses <- unlist(mclapply(1:length(search_ids), mc.cores = ncores,  FUN=function(s){
  if(verbose & s%%10 == 0) print(paste("calculating features for search", s, "of", length(search_ids)))   #logging
  tryCatch({
    output_filename <- file.path("../data", paste0(search_ids[s], ".rds"))
    if(file.exists(output_filename)) return(NULL)
    
    data_s <- dataset %>% filter(search_id == search_ids[s])
    
    if(data_s$check_in_date[1] > data_s$check_out_date[1]) stop(print(paste("iteration", s, "has check-in date after check-out date")))
    if(data_s$check_in_date[1] < as.Date(data_s$search_timestamp)[1]) stop(print(paste("iteration", s, "has check-in date in past")))
    
    feat_s <- data.frame(search_id = data_s$search_id[1], num_results(data_s), stay_duration(data_s), days_advance(data_s),
                         historical_searches(data_s), log_historical_searches(data_s),
                         historical_clicks(data_s), historical_sessions(data_s), device_pc(data_s), device_tablet(data_s),
                         device_windows(data_s), device_osx(data_s), device_iosx(data_s), device_macos(data_s),
                         device_chrome(data_s), device_ie(data_s), device_firefox(data_s), device_msafari(data_s),
                         device_safari(data_s), device_medge(data_s), same_country(data_s), same_state(data_s),
                         user_loc_us(data_s), user_loc_uk(data_s), user_loc_ca(data_s), user_loc_in(data_s),
                         user_loc_de(data_s), dest_loc_us(data_s), dest_loc_in(data_s), dest_loc_uk(data_s),
                         dest_loc_it(data_s), dest_loc_ca(data_s), dest_loc_cb(data_s), dest_loc_es(data_s),
                         dest_loc_fr(data_s), dest_loc_mx(data_s), dest_loc_de(data_s),
                         has_dest_city(data_s), has_municipality(data_s),
                         num_weeknights(data_s), num_weekdays(data_s), prop_weekdays(data_s),
                         mys_feature_1_mean(data_s), mys_feature_1_min(data_s), mys_feature_1_max(data_s), mys_feature_2(data_s))
    
    feat_s$ad_clicks <- sum(data_s$ad_click)
    saveRDS(feat_s, output_filename)
    return(TRUE)
  }, error=function(e){ 
    print(print(paste("iteration", s, e)))
    return(NULL)
  })
}))
print(Sys.time()-a)  

#=== combine results saved to individual files as a single csv===
all_files <- list.files(file.path(wd_data,"/data"), full.names=T)
system.time(
  all_features <- bind_rows(lapply(all_files, FUN= function(f){
    data_f <- readRDS(f)
  }))
)
summary(all_features)
all_features <- all_features %>% filter(!is.na(mys_feature_2))
write.csv(all_features, file.path(wd_data, "all_features.csv"), row.names = FALSE)
