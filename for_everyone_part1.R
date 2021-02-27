data_folder <- ("/home/Jia_Lu/R_programming/project_netflix/netflix-report/")
csv_folder <- paste0(data_folder, "Content_Interaction")
file_path <- paste0(csv_folder,"/ViewingActivity.csv")
netflixViews <- read.csv(file_path)
#head(netflixViews)


library(magrittr)  # to use the pipe-operator
library(tidyverse) # for all kind of data analyses/data manipulation
library(lubridate) # to manipulate the dates in the data-set

# to check the functionality of the mutate function
netflixViews_Cleaned <- netflixViews %>% 
    mutate(Profile.Name = factor(Profile.Name), 
        Attributes = factor(Attributes),
        Device.Type = factor(Device.Type),
        Country = factor(Country))
#View(netflixViews_Cleaned)

# Show summary of the data_frame
summary(netflixViews_Cleaned)


# let's use the lubridate library
netflixViews_Cleaned <- netflixViews_Cleaned %>% 
    mutate(Start.Time = ymd_hms(Start.Time)) 
#View(netflixViews_Cleaned)


# let's manipulate the "Duration" col and convert the time-stamp into the string duration 
netflixViews_Cleaned <- netflixViews_Cleaned %>% 
    mutate(Duration = hms(Duration),
        Duration = as.duration(Duration))
#View(netflixViews_Cleaned)


# let's manipulate the Title column
# following function manipulates the strings basically/originally
netflixViews_Cleaned <- netflixViews_Cleaned %>%
    separate(Title, into = c("Title", "Season"), sep = "Season|Series|Part|Volume|Chapter") %>%
    separate(Season, into = c("Season", "Episode"), "Episode") 
#head(netflixViews_Cleaned,10)


# let's clean columns: Season, and Episode 
netflixViews_Cleaned <- netflixViews_Cleaned %>%
    mutate(
        Title = gsub('(: )$', '', Title),
        Season = str_extract(Season, "[[:digit:]]+"),
        Episode = gsub('[[:punct:] ]+', ' ', Episode)) %>%
    mutate(
        Season = as.numeric(Season),
        Episode = as.numeric(Episode),
        Title = factor(Title))
#head(netflixViews_Cleaned)
summary(netflixViews_Cleaned)

# let's pull up a list of all the TV shows and films in the dataset
levels(netflixViews_Cleaned$Title)




# let's make a single function to check all above analyses'
analyseShow <- function(title){
    showData <- netflixViews_Cleaned %>%
        filter(Title == title)
    
    times <- showData %>%
        mutate(DayofWeek = wday(Start.Time, label = TRUE, abbr = FALSE),
            TimeofDay = ifelse(am(Start.Time), "AM", "PM")) %>%
        filter(Duration > 3*60)
    
    duration <- sum(times$Duration) %>% seconds_to_period()
    
    day <- times %>%
        group_by(DayofWeek) %>%
        count() %>%
        arrange(desc(n))  %>%
        ungroup %>%
        slice_max(order_by = n, n = 1)
    
    time <- times %>%
        group_by(TimeofDay) %>%
        count() %>%
        arrange(desc(n))  %>%
        ungroup %>%
        slice_max(order_by = n, n = 1)
    
    
    result <- paste0("I have spent ", duration, " of my life watching ", title, ". I was most likely to watch this show on ", day$DayofWeek, " in the ",  time$TimeofDay,".")
    
    return(result)
}
analyseShow("The Crown")



