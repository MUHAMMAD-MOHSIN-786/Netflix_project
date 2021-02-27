data_folder <- ("/home/Jia_Lu/R_programming/project_netflix/netflix-report/")
csv_folder <- paste0(data_folder, "Content_Interaction")
file_path <- paste0(csv_folder,"/ViewingActivity.csv")
netflixViews <- read.csv(file_path)
View(netflixViews)
str(netflixViews)

# all libraries which are to be needed in the data analyses
# to install them, use "install.packages("library_name")
library(magrittr)  # to use the pipe-operator
library(tidyverse) # for all kind of data analyses/data manipulation
library(lubridate) 


# let's take those rows which are full shows, also
# let's remove Bookmark, latest.bookmark, and supple... cols
netflixViews_Filtered <- netflixViews %>%
    filter(Supplemental.Video.Type == "") %>%
    select(-Bookmark, -Latest.Bookmark, -Supplemental.Video.Type)
View(netflixViews_Filtered)
str(netflixViews_Filtered)


# let's Create factors for categorical cols
netflixViews_Cleaned <- netflixViews_Filtered %>% 
    mutate(Profile.Name = factor(Profile.Name), 
        Attributes = factor(Attributes),
        Device.Type = factor(Device.Type),
        Country = factor(Country)) %>%
    mutate(Start.Time = as.character(Start.Time),
        Title = as.character(Title),
        Duration = as.character(Duration)) #%>%
#head(netflixViews_Cleaned)
#summary(netflixViews_Cleaned)
str(netflixViews_Cleaned)
head(netflixViews_Cleaned)


# Transform datetime
#netflixViews_Cleaned <- netflixViews_Cleaned %>% 
#    mutate(Start.Time = as.Date(Start.Time)) 
#head(netflixViews_Cleaned)


netflixViews_Cleaned <- netflixViews_Cleaned %>% 
    mutate(Duration = hms(Duration),
        Duration = as.duration(Duration))
#head(netflixViews_Cleaned)


# let's separate the cols: Title, Season, and Episodes
netflixViews_Cleaned <- netflixViews_Cleaned %>%
    separate(Title, into = c("Title", "Season"), sep = "Season|Series|Part|Volume|Chapter") %>%
    separate(Season, into = c("Season", "Episode"), "Episode")
#head(netflixViews_Cleaned)


# let's clean the col: Title
netflixViews_Cleaned <- netflixViews_Cleaned %>%
    mutate(
        Title = gsub('(: )$', '', Title),
        Season = str_extract(Season, "[[:digit:]]+"),
        Episode = gsub('[[:punct:] ]+', ' ', Episode)) %>%
    mutate(
        Season = as.numeric(Season),
        Episode = as.numeric(Episode),
        Title = factor(Title))
View(netflixViews_Cleaned)
#summary(netflixViews_Cleaned)
str(netflixViews_Cleaned)


# let's check the levels against Title
levels(netflixViews_Cleaned$Title)



# let's make a function which will do all above together
analyseShow <- function(title){
    netflixViews_Cleaned <- netflixViews_Cleaned %>% 
        mutate(Start.Time = as.Date(Start.Time))
    
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
analyseShow('The Crown')
