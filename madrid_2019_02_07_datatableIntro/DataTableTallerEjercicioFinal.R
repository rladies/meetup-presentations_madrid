# Ejemplos de distintas webs

library(data.table)

flights <- fread("D:/TallerRladies/flights14.csv")
# Lo primero entiende que info tienes en el dataset
str(flights)
dim(flights)

# - Get all the flights with "JFK" as the origin airport 
# in the month of June.

ans <- flights[origin == "JFK" & month == 6L]


# - How many trips have had total delay < 0?

dim(flights[arr_delay+dep_delay<0])

ans <- flights[, sum( (arr_delay + dep_delay) < 0 )]

flights[, (arr_delay + dep_delay) ] # vector nuemrico
flights[, (arr_delay + dep_delay) < 0 ] # vector booleano
flights[, sum( (arr_delay + dep_delay) < 0 )] #numero

# - Calculate the average arrival and 
# departure delay for all flights with "JFK" as the 
# origin airport in the month of June.
flights[origin=="JFK" & month==6,
        .(mediaArrDelay = mean(arr_delay),
           mediaDepDelay = mean(dep_delay))]

flights[origin=="JFK" & month==6,
        lapply(.SD,mean),
        .SDcols=c("arr_delay","dep_delay")]

# Si quisieras sacarlo para todo los numericos..
flights[origin=="JFK" & month==6,
        lapply(.SD,mean),
        .SDcols=sapply(flights,is.numeric)]


# - How many trips have been made in 
#2014 from "JFK" airport in the month of June?
dim(flights[origin=="JFK" & month==6 & year==2014])

flights[origin=="JFK" & month==6 & year==2014,.N]

flights[origin == "JFK" & month == 6L, length(dest)]


# - How can we get the number of trips 
# corresponding to each origin airport?
flights[,.N,by=.(origin)]


#- How can we calculate the number of trips 
#for each origin airport for carrier code "AA"?

ans <- flights[carrier == "AA", 
               .N, by = .(origin)]


#- How can we get the total number of trips for each origin, dest pair for carrier code "AA"?
ans <- flights[carrier == "AA", .N, by = .(origin, dest)]

# - How can we get the average arrival and departure delay for each orig,dest pair 
# for each month for carrier code "AA"? (order the result!)

ans <- flights[carrier == "AA",
               .(mean(arr_delay), mean(dep_delay)),
               keyby = .(origin, dest, month)]

#- How can we return the first two rows for each month?
ans <- flights[, head(.SD, 2), by = .(month)]
ans2<-flights[, .SD[c(1,2)], by = .(month)]

#- Extract average of arrival and departure delays for carrier == 'DL' by 'origin' and 'dest' variables
flights[carrier == "DL",
       lapply(.SD, mean, na.rm = TRUE),
       by = .(origin, dest),
       .SDcols = c("arr_delay", "dep_delay")]

#- Find top 3 months with high mean arrival delay

flights[, .(mean_arr_delay = mean(arr_delay, na.rm = TRUE)), 
        by = month][order(-mean_arr_delay)][1:3]
flights[, .(mean_arr_delay = mean(arr_delay, na.rm = TRUE)), 
        by = month][order(-mean_arr_delay)][,.SD[1:3]]


#  Find origin of flights having average total delay is greater than 20 minutes

flights[, lapply(.SD, mean, na.rm = TRUE), 
        by =.(origin),
        .SDcols = c("arr_delay", "dep_delay")][(arr_delay + dep_delay) > 20]

flights[, setNames(lapply(.SD, mean, na.rm = TRUE),c("meanarr_delay","meandep_delay")), 
          by =.(origin),
       .SDcols = c("arr_delay", "dep_delay")][(meanarr_delay + meandep_delay) > 20]
