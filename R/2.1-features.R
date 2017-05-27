#search-level features
num_results    <- function(data_s){ data.frame(num_results = length(unique(data_s$search_result_id)))}
stay_duration  <- function(data_s){ data.frame(stay_duration = as.numeric(data_s$check_out_date[1] - data_s$check_in_date[1], units="days"))}
same_country   <- function(data_s){ data.frame(same_country = !is.na(data_s$user_loc_Country[1]) & !is.na(data_s$dest_loc_Country[1]) & data_s$user_loc_Country[1] == data_s$dest_loc_Country[1])}
same_state     <- function(data_s){ data.frame(same_state = !is.na(data_s$user_loc_State[1]) & !is.na(data_s$dest_loc_State[1]) & data_s$user_loc_State[1] == data_s$dest_loc_State[1])}
user_loc_us    <- function(data_s){ data.frame(user_loc_us = !is.na(data_s$user_loc_Country[1]) & data_s$user_loc_Country[1] == "United States")}
user_loc_uk    <- function(data_s){ data.frame(user_loc_uk = !is.na(data_s$user_loc_Country[1]) & data_s$user_loc_Country[1] == "United Kingdom")}
user_loc_ca    <- function(data_s){ data.frame(user_loc_ca = !is.na(data_s$user_loc_Country[1]) & data_s$user_loc_Country[1] == "Canada")}
user_loc_in    <- function(data_s){ data.frame(user_loc_in = !is.na(data_s$user_loc_Country[1]) & data_s$user_loc_Country[1] == "India")}
user_loc_de    <- function(data_s){ data.frame(user_loc_de = !is.na(data_s$user_loc_Country[1]) & data_s$user_loc_Country[1] == "Germany")}
num_weeknights  <- function(data_s){ 
  stay <- seq(data_s$check_in_date[1], data_s$check_out_date[1], by=as.difftime(1, units="days"))
  weeknights <- lubridate::wday(stay[1:(length(stay)-1)]) %in% c(6,7) #check-in friday, sat
  data.frame(num_weeknights = sum(weeknights))}
num_weekdays  <- function(data_s){ 
  stay <- seq(data_s$check_in_date[1], data_s$check_out_date[1], by=as.difftime(1, units="days"))
  weekdays <- !lubridate::wday(stay[1:(length(stay)-1)]) %in% c(6,7) #check-in friday, sat
  data.frame(num_weekdays = sum(weekdays))}
prop_weekdays  <- function(data_s){ 
  stay <- seq(data_s$check_in_date[1], data_s$check_out_date[1], by=as.difftime(1, units="days"))
  weekdays <- !lubridate::wday(stay[1:(length(stay)-1)]) %in% c(6,7) #check-in friday, sat
  data.frame(prop_weekdays = sum(weekdays)/ length(stay))}

days_advance       <- function(data_s){ data.frame(days_advance = (as.numeric(data_s$check_in_date[1] - as.Date(data_s$search_timestamp)[1], units="days")))}
log_days_advance   <- function(data_s){ data.frame(log_days_advance = log(as.numeric(data_s$check_in_date[1] - as.Date(data_s$search_timestamp)[1], units="days")+1))}

dest_loc_us    <- function(data_s){ data.frame(dest_loc_us = !is.na(data_s$dest_loc_Country[1]) & data_s$dest_loc_Country[1] == "United States")}
dest_loc_in    <- function(data_s){ data.frame(dest_loc_in = !is.na(data_s$dest_loc_Country[1]) & data_s$dest_loc_Country[1] == "India")}
dest_loc_uk    <- function(data_s){ data.frame(dest_loc_uk = !is.na(data_s$dest_loc_Country[1]) & data_s$dest_loc_Country[1] == "United Kingdom")}
dest_loc_it    <- function(data_s){ data.frame(dest_loc_it = !is.na(data_s$dest_loc_Country[1]) & data_s$dest_loc_Country[1] == "Italy")}
dest_loc_ca    <- function(data_s){ data.frame(dest_loc_ca = !is.na(data_s$dest_loc_Country[1]) & data_s$dest_loc_Country[1] == "Canada")}
dest_loc_cb    <- function(data_s){ data.frame(dest_loc_cb = !is.na(data_s$dest_loc_Country[1]) & data_s$dest_loc_Country[1] == "Caribbean")}
dest_loc_es    <- function(data_s){ data.frame(dest_loc_es = !is.na(data_s$dest_loc_Country[1]) & data_s$dest_loc_Country[1] == "Spain")}
dest_loc_fr    <- function(data_s){ data.frame(dest_loc_fr = !is.na(data_s$dest_loc_Country[1]) & data_s$dest_loc_Country[1] == "France")}
dest_loc_mx    <- function(data_s){ data.frame(dest_loc_mx = !is.na(data_s$dest_loc_Country[1]) & data_s$dest_loc_Country[1] == "Mexico")}
dest_loc_de    <- function(data_s){ data.frame(dest_loc_de = !is.na(data_s$dest_loc_Country[1]) & data_s$dest_loc_Country[1] == "Germany")}

has_dest_city     <- function(data_s){ data.frame(has_dest_city = !is.na(data_s$dest_loc_City[1]))}
has_municipality  <- function(data_s){ data.frame(has_municipality = !is.na(data_s$dest_loc_Municipality[1]))}

#user-level features
historical_searches   <- function(data_s){ data.frame(historical_searches = data_s$historical_searches[1])}
log_historical_searches<- function(data_s){ data.frame(log_historical_searches = log(data_s$historical_searches[1] + 1))}
historical_clicks     <- function(data_s){ data.frame(historical_clicks = data_s$historical_clicks[1])}
historical_sessions   <- function(data_s){ data.frame(historical_sessions = data_s$historical_sessions[1])}

#session-level features
device_pc       <- function(data_s){ data.frame(device_pc = !is.na(data_s$ua_device_category[1]) & data_s$ua_device_category[1] == "Personal computer")}
device_tablet   <- function(data_s){ data.frame(device_tablet = !is.na(data_s$ua_device_category[1]) & data_s$ua_device_category[1] == "Tablet")}
device_windows  <- function(data_s){ data.frame(device_windows = !is.na(data_s$ua_device_os[1]) & data_s$ua_device_os[1] == "Windows")}
device_osx      <- function(data_s){ data.frame(device_osx = !is.na(data_s$ua_device_os[1]) & data_s$ua_device_os[1] == "OS X")}
device_iosx     <- function(data_s){ data.frame(device_iosx = !is.na(data_s$ua_device_os[1]) & data_s$ua_device_os[1] == "iOS")}
device_macos    <- function(data_s){ data.frame(device_macos = !is.na(data_s$ua_device_os[1]) & data_s$ua_device_os[1] == "macOS")}

device_chrome   <- function(data_s){ data.frame(device_chrome = !is.na(data_s$ua_browser_name[1]) & data_s$ua_browser_name[1] == "Chrome")}
device_ie       <- function(data_s){ data.frame(device_ie = !is.na(data_s$ua_browser_name[1]) & data_s$ua_browser_name[1] == "IE")}
device_firefox  <- function(data_s){ data.frame(device_firefox = !is.na(data_s$ua_browser_name[1]) & data_s$ua_browser_name[1] == "Firefox")}
device_msafari  <- function(data_s){ data.frame(device_msafari = !is.na(data_s$ua_browser_name[1]) & data_s$ua_browser_name[1] == "Mobile Safari")}
device_safari   <- function(data_s){ data.frame(device_safari = !is.na(data_s$ua_browser_name[1]) & data_s$ua_browser_name[1] == "Safari")}
device_medge    <- function(data_s){ data.frame(device_medge = !is.na(data_s$ua_browser_name[1]) & data_s$ua_browser_name[1] == "Microsoft Edge")}

mys_feature_1_mean    <- function(data_s){ data.frame(mys_feature_1_mean = mean(data_s$mysterious_feature_1, na.rm=T))}
mys_feature_1_min    <- function(data_s){ data.frame(mys_feature_1_min = min(data_s$mysterious_feature_1, na.rm=T))}
mys_feature_1_max    <- function(data_s){ data.frame(mys_feature_1_max = max(data_s$mysterious_feature_1, na.rm=T))}
mys_feature_2         <- function(data_s){ data.frame(mys_feature_2 = mean(data_s$mysterious_feature_2, na.rm=T))}

#testing
# balanced_data <- readRDS(file.path(wd_data, "balanced_data_formatted.rds"))
# data_s <- balanced_data %>% filter(search_id == "49a66474-318a-4e47-8362-81c59bca89a9")
# feat_s <- data.frame(search_id = data_s$search_id[1], num_results(data_s), stay_duration(data_s), days_advance(data_s),
#                      log_days_advance(data_s), historical_searches(data_s), log_historical_searches(data_s),
#                      historical_clicks(data_s), historical_sessions(data_s), device_pc(data_s), device_tablet(data_s),
#                      device_windows(data_s), device_osx(data_s), device_iosx(data_s), device_macos(data_s),
#                      device_chrome(data_s), device_ie(data_s), device_firefox(data_s), device_msafari(data_s),
#                      device_safari(data_s), device_medge(data_s), same_country(data_s), same_state(data_s),
#                      user_loc_us(data_s), user_loc_uk(data_s), user_loc_ca(data_s), user_loc_in(data_s),
#                      user_loc_de(data_s), dest_loc_us(data_s), dest_loc_in(data_s), dest_loc_uk(data_s),
#                      dest_loc_it(data_s), dest_loc_ca(data_s), dest_loc_cb(data_s), dest_loc_es(data_s),
#                      dest_loc_fr(data_s), dest_loc_mx(data_s), dest_loc_de(data_s),
#                      has_dest_city(data_s), has_municipality(data_s),
#                      num_weeknights(data_s), num_weekdays(data_s), prop_weekdays(data_s),
#                      mys_feature_1_mean(data_s), mys_feature_1_min(data_s), mys_feature_1_max(data_s), mys_feature_2(data_s))
