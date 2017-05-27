#setup location reference
location_type_id <- c(625389, 283784, 728861, 746938, 505486, 863420, 708129, 147494,
                      745929, 467541, 651253, 865442, 469604, 781706, 354571,
                      40815, 854313, 982927)
location_type_name <- c("World", "Continent", "Country", "Nation", "State", "City", "County", "Region",
                        "Island", "Island Group", "Territory", "District", "Municipality", "Department", "Province",
                        "Parish", "Canton", "NationalPark")
location_ref <- data.frame(location_type_id, location_type_name) 	

#function to parse location
parse_location <- function(location){
    parts <- strsplit(location, "[|]")[[1]]
    if(length(parts) == 0) return(data.frame(World=NA))
    location_cols <- bind_cols(lapply(parts, FUN=function(p){
      subparts <- strsplit(p, "[:]")[[1]]
      df <- data.frame(name = subparts[3])
      names(df) <- location_ref$location_type_name[location_ref$location_type_id == subparts[2]]
      return(df)
    }))
    return(location_cols)
}

#testing
#parse_location("617026444:625389:World|345082770:283784:North America|816206773:728861:United States|270374786:505486:Montana|180048012:863420:Bozeman")
