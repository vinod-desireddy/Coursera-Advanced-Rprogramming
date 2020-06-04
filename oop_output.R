#Part 2: Longitudinal Data Class and Methods

# The purpose of this part is to create a new class for representing 
#       longitudinal data, which is data that is collected over time 
#       on a given subject/person. This data may be collected at multiple 
#       visits, in multiple locations. You will need to write a series of 
#       generics and methods for interacting with this kind of data.

# The data for this part come from a small study on indoor air pollution 
#       on 10 subjects. Each subject was visited 3 times for data collection. 
#       Indoor air pollution was measured using a high-resolution monitor 
#       which records pollutant levels every 5 minutes and the monitor was 
#       placed in the home for about 1 week. In addition to measuring pollutant 
#       levels in the bedroom, a separate monitor was usually placed in another 
#       room in the house at roughly the same time.


url = 'https://d3c33hcgiwev3.cloudfront.net/_257dbf6be13177cd110e3ef91b34ff67_data.zip?Expires=1591056000&Signature=PzCPvza5Qt4b0drnemVZmBoBmItaa9ky4ByuKn0XYkFlQ23pmvuErCtUEMm4mDSv9BOVUyc2fJLA1qPlAaEznUtxdkD-NUix-xZdOrBE2UXZKbn6D9ZWMKyf5UFrVd4T6zGx2albtRNVmUPYGkQL0gUgZX~AxCNOnTlFlRYOpsw_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A'

if(!file.exists('data.zip')) {
      download.file(url, 'data.zip', 'curl')
      }
      

library(readr)
library(dplyr)
dataa = read_csv(unzip('data.zip', files = 'data/MIE.csv', overwrite = T))
summary(dataa)
## The variables in the dataset are-
# id: the subject identification number
# visit: the visit number which can be 0, 1, or 2
# room: the room in which the monitor was placed
# value: the level of pollution in micrograms per cubic meter
# timepoint: the time point of the monitor value for a given visit/room

# You will need to design a class called "LongitudinalData" that 
#       characterizes the structure of this longitudinal dataset. 
#       You will also need to design classes to represent the concept of 
#       a "subject", a "visit", and a "room".

# In addition you will need to implement the following functions
#     make_LD: a function that converts a data frame into a "LongitudinalData" object
#     subject: a generic function for extracting subject-specific information
#     visit: a generic function for extracting visit-specific information
#     room: a generic function for extracting room-specific information

##For each generic/class combination you will need to implement a method, 
#     although not all combinations are necessary. You will 
#     also need to write print and summary methods for some classes.

make_Ld <- function(daata){
      structure(daata, class = 'LongitudinalData')
}

dataa <- make_Ld(dataa)

subject <- function(daata, sid) UseMethod('subject')
subject.LongitudinalData <- function(daata, sid){
      class(daata) = 'data.frame'
      sub = daata %>% filter(id == sid)
      sub <- structure(sub,class = 'Subject')
      sub
}

subject.LongitudinalData(dataa, 14)

visit <- function(daata, visit) UseMethod('visit')
visit.LongitudinalData <- function(daata){
      class(daata) = 'data.frame'
      sub = daata %>% filter(visit == visit)
      sub <- structure(sub,class = 'Visit')
      sub
}
visit.LongitudinalData(dataa)

room <- function(daata, room_name) UseMethod('room')
room.LongitudinalData <- function(daata, room_name){
      class(daata) = 'data.frame'
      sub = daata %>% filter(room == room_name)
      sub <- structure(sub,class = 'Room')
      sub
}
room.LongitudinalData(dataa, 'bedroom')

#summary methods for the above classes

summary.Subject <- function(daata){
      class(daata) = 'data.frame'
      sub = daata %>% group_by(id) %>% 
            summarise(avg_value = mean(value, na.rm = T))
      structure(list(id = sub$id,
                     avg_value = sub$avg_value),
                class = 'Summary')
}

summary.Subject(dataa)

summary.Visit <- function(daata){
      class(daata) = 'data.frame'
      sub = daata %>% group_by(visit) %>% 
            summarise(avg_value = mean(value, na.rm = T))
      structure(list(visit = sub$visit,
                     avg_value = sub$avg_value),
                class = 'Summary')
}
summary.Visit(dataa)

summary.Room <- function(daata){
      class(daata) = 'data.frame'
      sub = daata %>% group_by(room) %>%
            summarise(avg_value = mean(value, na.rm = T))
      structure(list(room_name = sub$room,
                     avg_value = sub$avg_value),
                class = 'Summary')
}
summary.Room(dataa)

#print methods
print.Summary <- function(daata){
      print(paste("max_avg_value", max(daata$avg_value), sep = '='))
      print(paste("min_avg_value", min(daata$avg_value), sep = '='))
      print(paste("mean_avg_value", mean(daata$avg_value), sep = '='))
}
