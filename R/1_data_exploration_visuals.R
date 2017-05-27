#=== population-wide stats === 
search_level <- transactions %>% 
  group_by(user_id, session_id, search_id, search_timestamp) %>%
  dplyr::summarize(number_results = length(unique(search_result_id)),
                   num_click = sum(ad_click)) %>%
  ungroup()
#saveRDS(search_level, file.path(wd_data, "search_level.rds"))

#How many search results end with clicks- ECDF
ggplot(search_level) + stat_ecdf(aes(x=num_click)) + 
  theme_minimal() + coord_cartesian(xlim=c(0,10)) + 
  labs(title="ECDF of number of clicks on search results", x="Number of clicks", y="Proportion of searches")
round(prop.table(table(search_level$num_click)), 3)

plot_data <-search_level %>% group_by(number_results) %>% 
  dplyr::summarize(count=n(), num_click=sum(num_click>0)) %>%
  mutate(prop=count/sum(count), pct_click = num_click/count)
ggplot(plot_data) + geom_line(aes(x=number_results, y=prop)) + theme_minimal() + 
  labs(x="Number of search results", y="Proportion of searches", title="Distribution of number of search results returned")
ggplot(plot_data) + geom_line(aes(x=number_results, y=pct_click)) + theme_minimal() + 
  geom_hline(yintercept = mean(plot_data$pct_click), color="grey") +
  labs(x="Number of search results", y="Proportion of searches clicked", title="Proportion of searches clicked vs number of search results returned")

#exploration of user data shows it is incomplete
user_level <- search_level %>%
  group_by(user_id) %>%
  dplyr::summarize(num_searches = n(),
                   num_clicked = sum(num_click > 0))

round(prop.table(table(user_level$num_searches)), 3)   #75.3% of users only do 1 search, 14.9% do 2 searches
round(prop.table(table(user_level$num_clicked)), 3)    #92.1% of users have never clicked, 6.9% have clicked on 1 search


#exploration of session data 
session_level <- search_level %>%
  group_by(session_id) %>%
  dplyr::summarize(num_searches = n(),
                   num_clicked = sum(num_click > 0))

round(prop.table(table(session_level$num_searches)), 3)   #79.1% of sessions include 1 search, 12.1% do 2 searches
round(prop.table(table(session_level$num_clicked)), 3)    #92.3% of sessions do not include any click, 6.8% have clicked on 1 search

#Click rates based on ad ranks
round(prop.table(table(transactions$ad_rank, transactions$ad_click), margin = 1), 3)

data.frame(sort(table(transactions$ua_device_os))) %>% mutate(prop = round(Freq/sum(Freq), 3))

transactions_ids <- unique(transactions$search_id)[3]
transactions %>% filter(search_id %in% transactions_ids)
head(unique(transactions$dest_location_navigation_string), 20)


#=== given balanced data ===
balanced_data <- readRDS("balanced_data.rds")
ggplot(balanced_data) + 
  geom_density(aes(x=number_results, group=was_clicked, fill=was_clicked), alpha=0.1, adjust=6) +
  theme_minimal() + labs(title="Density plot of number of search results returned")

#=== explore nav strings ===
get_val_prop <- function(field){
  as.data.frame(round(prop.table(table(field, useNA = "always")),3)) %>% 
    arrange(-Freq) %>% mutate(cumsum=cumsum(Freq))
}

parsed_nav_strings <- readRDS(file.path(wd_data, "parsed_nav_strings.rds"))
get_val_prop(parsed_nav_strings$World)
get_val_prop(parsed_nav_strings$Continent)  #77.9% of searches originate from NAm, 12.4% from Europe, 4.5% from Asia
head(get_val_prop(parsed_nav_strings$Country))    #74.0% of searches originate from US, 2.5% from UK, 2.3% from Canada

prop.table(table(is.na(parsed_nav_strings$City))) # 1.4% of searches have Region as NA
prop.table(table(is.na(parsed_nav_strings$Region))) # 85.1% of searches have Region as NA
prop.table(table(is.na(parsed_nav_strings$Province))) # 91.6% of searches have Province as NA
prop.table(table(is.na(parsed_nav_strings$Municipality))) # 96.3% of searches have Municipality as NA

parsed_nav_strings %>% filter(is.na(City)) %>% head

parsed_nav_strings <- readRDS(file.path(wd_data, "dest_nav_strings.rds"))
prop.table(table(is.na(parsed_nav_strings$dest_loc_Country))) # 0.25% of searches have dest_loc_Country as NA
head(get_val_prop(parsed_nav_strings$dest_loc_Country), 10)    #64.3% of dest in US, 3.2% of dest in India, 2.9% of dest in UK

#=== analyse features ===
all_features <- read.csv(file.path(wd_data, "all_features.csv"))
max_x <- quantile(all_features$mys_feature_2, 0.99, na.rm=T)
ggplot(all_features) + 
  geom_density(aes(x=mys_feature_2, group=ad_clicks>0, fill=ad_clicks>0), alpha=0.1) +
  theme_minimal() + xlim(0, (max_x+1) )
names(all_features)

prop.table(table(feature = all_features$device_windows, clicked = all_features$ad_clicks>0), margin = 1)
prop.table(table(feature = all_features$user_loc_us, clicked = all_features$ad_clicks>0), margin = 1)

max_y <-quantile(all_features$mys_feature_2, 0.99, na.rm=T)
ggplot(all_features) + 
  geom_point(aes(x=historical_searches, y= stay_duration, color=ad_clicks>0), size = 0.5, position="jitter") +
  theme_minimal() + ylim(0, max_y) 
ggplot(all_features) + 
  geom_point(aes(x=mys_feature_1_min, y= mys_feature_1_max, color=ad_clicks>0), size = 0.5, position="jitter") +
  theme_minimal() #+ ylim(0, max_y)
