
#clean and organize the data that I want
data_access <- function(){
  data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"
                   ,header = TRUE, stringsAsFactors = FALSE)
  data <- select(data,"year","state","county_name","total_pop","total_jail_pop","aapi_jail_pop",
           "black_jail_pop","latinx_jail_pop","native_jail_pop","white_jail_pop","other_race_jail_pop")
  convert <- function(x){
    new <- as.numeric(x)
    return(new)
  }
  data[,(4:9)] <- lapply(data[,(4:9)], convert)
  data$year <- as.factor(data$year)
  return(data)
  
}
#A paragraph of summary information, citing at least 5 values calculated from the data
summary <- function(data){
  summary_info <- list()
  summary_info$num_observations <- nrow(data)
  summary_info$num_variables <- ncol(data)
  summary_info$max_pop_jail_black <- data %>%
    filter(state == "WA") %>%
    select(black_jail_pop) %>%
    max(na.rm = TRUE)
  summary_info$max_pop_jail_white <- data %>%
    filter(state == "WA") %>%
    select(white_jail_pop) %>%
    max(na.rm = TRUE)
  summary_info$max_pop_jail_aapi<- data %>%
    filter(state == "WA") %>%
    select(aapi_jail_pop) %>%
    max(na.rm = TRUE)
  summary_info$max_pop_jail_latinx <- data %>%
    filter(state == "WA") %>%
    select(latinx_jail_pop) %>%
    max(na.rm = TRUE)
  text <- paste("According to the data, I can summarize that the number of 
  observations for the total data is ",summary_info$num_observations,". The number 
  of variavbles for the total data is ", summary_info$num_variables,". The maximum population
  of black people in Washington prisons is",summary_info$max_pop_jail_black,", and the maximum population
  of white people in Washington prisons is", summary_info$max_pop_jail_white,"; The maximum population
  of Asian Americans and Pacific Islanders in Washington prisons is",summary_info$max_pop_jail_aapi,", and the maximum population
  of latinx people in Washington prisons is", summary_info$max_pop_jail_latinx,".")
  return(cat(text))
}

# Chart1: Trends For Crime Rate of Different Races in US from 1970 to 2018
Chart1 <- function(data){
  US_data <- data %>%
    select("year","total_pop","total_jail_pop","aapi_jail_pop",
           "black_jail_pop","latinx_jail_pop","native_jail_pop","white_jail_pop","other_race_jail_pop")%>%
    group_by(year)%>%
    summarize("total_pop" = sum(total_pop,na.rm = TRUE),
              "total_jail_pop" = sum(total_jail_pop,na.rm = TRUE),
              "aapi_jail_pop" = sum(aapi_jail_pop,na.rm = TRUE),
              "black_jail_pop" = sum(black_jail_pop,na.rm = TRUE),
              "latinx_jail_pop" = sum(latinx_jail_pop,na.rm = TRUE),
              "native_jail_pop" = sum(native_jail_pop,na.rm = TRUE),
              "white_jail_pop" = sum(white_jail_pop,na.rm = TRUE),
              "other_race_jail_pop" = sum(other_race_jail_pop,na.rm = TRUE))
  US_rate <- data_frame(year = US_data$year, aapi_ratio = US_data$aapi_jail_pop/US_data$total_jail_pop*100,
                        black_ratio = US_data$black_jail_pop/US_data$total_jail_pop*100,
                        latinx_ratio = US_data$latinx_jail_pop/US_data$total_jail_pop*100,
                        native_ratio = US_data$native_jail_pop/US_data$total_jail_pop*100,
                        white_ratio = US_data$white_jail_pop/US_data$total_jail_pop*100,
                        other_ratio = US_data$other_race_jail_pop/US_data$total_jail_pop*100)
  chart_data_aapi <- data.frame(year = c(1970:2018),crime_rate = US_rate$aapi_ratio)
  chart_data_black <- data.frame(year = c(1970:2018),crime_rate = US_rate$black_ratio)
  chart_data_latinx <- data.frame(year = c(1970:2018),crime_rate = US_rate$latinx_ratio)
  chart_data_native <- data.frame(year = c(1970:2018),crime_rate = US_rate$native_ratio)
  chart_data_white <- data.frame(year = c(1970:2018),crime_rate = US_rate$white_ratio)
  chart_data_other <- data.frame(year = c(1970:2018),crime_rate = US_rate$other_ratio)
  chart_data <- bind_rows(chart_data_aapi,chart_data_black,chart_data_latinx,chart_data_native,
                          chart_data_white,chart_data_other)
  chart_data$race <- c(rep("AAPI",49),rep("BLACK",49),rep("LATINX",49),
                       rep("NATIVE",49),rep("WHITE",49),rep("OTHER RACE",49))
  chart<-ggplot(chart_data, aes(x=year,y = crime_rate, col = race)) + geom_line() +
    ylab("crime rate (%)") + ggtitle("Trends For Crime Rate of Different Races in US from 1970 to 2018")
  return(chart)
}

# Chart2: Relationship Between Jail Popultaion of White People and Jail Popultaion of Black People in US
Chart2 <- function(data){
  US_data <- data %>%
    select("year","white_jail_pop","black_jail_pop")%>%
    group_by(year)%>%
    summarize("white_jail_pop" = sum(white_jail_pop,na.rm = TRUE),
              "black_jail_pop" = sum(black_jail_pop,na.rm = TRUE))%>%
    arrange(white_jail_pop)
  chart <- ggplot(US_data, aes(x=white_jail_pop, y=black_jail_pop)) + 
    geom_point()+geom_smooth(formula =y~x, aes(x=white_jail_pop, y=black_jail_pop),method=lm, se = FALSE) + 
    ylab("jail popultaion of black people") + xlab("jail population of white people")+ 
    ggtitle("Relationship Between Jail Popultaion of White People and Jail Popultaion of Black People in US")
  efficient <- cor(US_data$white_jail_pop,US_data$black_jail_pop)
  return(chart)
}

# Chart3: Total Jail Population compare Across Different States in 2018 in US
Chart3 <- function(data){
  data_2018 <- data %>%
    filter(year == 2018)%>%
    select("state","total_jail_pop")%>%
    group_by(state)%>%
    summarize("total_jail_pop" = sum(total_jail_pop,na.rm = TRUE))
  state <- map_data("state")
  state$region <- toupper(state$region)
  state$region <- state.abb[match(state$region,toupper(state.name))]
  names(data_2018)[names(data_2018) == "state"] <- "region"
  chart_data <- inner_join(state,data_2018, by ="region")
  map <- ggplot(chart_data, aes(x = long, y = lat, group = group)) + 
    geom_polygon(aes(fill =total_jail_pop),color = "black") +
    scale_fill_continuous(name = "total jail population") + 
    ggtitle("Total Jail Population compare Across Different States in 2018 in US")
  return(map)
}

  
