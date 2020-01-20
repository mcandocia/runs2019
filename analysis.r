library(tidyverse)
library(lubridate)
library(cetcolor)

PNG_RES=144

# https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R
source('calendarHeatmap.r')

df = read_csv('run_data.csv')
weekday_string = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
month_string = c('January','February','March','April','May','June','July','August','September','October','November','December')

minutes_format <- function(x){
  minutes = as.integer(floor(x))
  seconds = (x-minutes)*60
  return(sprintf('%d:%02d',minutes, as.integer(seconds)))
}

first_days = data.frame(
  month_int=1:12
) %>% mutate(
  month = factor(month_string[month_int], levels=month_string),
  first_day_c = sprintf('2019-%02d-01', month_int),
  first_day = as_date(first_day_c),
  ts_week = week(first_day_c),
  ts_dow_f = factor(strftime(first_day_c, '%A'), levels=weekday_string),
  ts_month_f = month
)

local_time <- function(x, timezone){
  sapply(
    1:length(x),
    function(i) as.character(as.POSIXct(x[i], origin='1970-01-01', tz=timezone[i]))
  )
}

df = df %>%
  rowwise() %>%
  mutate(
    timestamp = as.POSIXct(timestamp_start, origin='1970-01-01', tz=timezone),
    timestamp_c = local_time(timestamp_start, timezone),
    timestamp_c_end = local_time(timestamp_end_run, timezone),
    ts_week = week(timestamp_c),
    ts_month = month(timestamp_c),
    ts_month_f = factor(month_string[ts_month], levels = month_string),
    ts_dow = strftime(timestamp_c, '%A'),
    ts_dow_f = factor(ts_dow, levels = weekday_string),
    ts_hour = hour(timestamp_c),
    ts_hour_end = hour(timestamp_c_end),
    ts_dow_end = strftime(timestamp_c_end, '%A'),
    ts_dow_end_f = factor(ts_dow_end, levels=weekday_string),
    time_of_day = strftime(timestamp, '%H:%M:%S'),
    week_start = cut(as_date(timestamp_c), 'week')
  ) %>%
  ungroup()

df = df %>% filter(year(timestamp_c)==2019) %>%
  mutate(run_no = 1:n())

monthday_format <- function(x) strftime(x, '%m/%d')

# come up with covar matrix and means for variables
summary_variables = c('heart_rate_mean','speed_average','total_distance','cadence_mean','temperature_median_v2')

averages = colMeans(df[,summary_variables], na.rm=T)
summary_covariance = cov(df[,summary_variables], use='complete.obs')
summary_cor = cor(df[,summary_variables], use='complete.obs')

df$mahalanobis_distance = mahalanobis(df[,summary_variables], averages, summary_covariance)

format_pace <- function(x){
  mins = floor(x)
  seconds = round(60*(x - mins))
  return(sprintf('%d:%02d', mins, seconds))
}

format_speed_as_pace <- function(x){
  format_pace(60/x)
}

superlative_levels = c("Shortest Distance", "Longest Distance", "Slowest Run", 'Fastest Run',
                       'Lowest Average Heart Rate','Highest Average Heart Rate',
                       'Lowest Temperature','Highest Temperature',
                       'Least Typical Run',
                       'Most Typical Run',
                       'Earliest Run',
                       'Latest Run'
)

df = df %>%
  mutate(local_date = as_date(timestamp_c)) %>% 
  group_by(local_date) %>%
  mutate(
    number_of_daily_runs=n()
  ) %>% 
  ungroup()

superlatives = df %>%
  filter(total_distance > 1 | number_of_daily_runs==1) %>%
  mutate(
    superlative = case_when(
      !is.na(temperature_median_v2) & temperature_median_v2==max(temperature_median_v2, na.rm=T) ~ 'Highest Temperature',
      !is.na(temperature_median_v2) & temperature_median_v2==min(temperature_median_v2, na.rm=T) ~ 'Lowest Temperature',
      total_distance == max(total_distance) ~ 'Longest Distance',
      total_distance == min(total_distance) ~ 'Shortest Distance',
      speed_average == max(speed_average) ~ 'Fastest Run',
      speed_average == min(speed_average) ~ 'Slowest Run',
      !is.na(heart_rate_mean) & heart_rate_mean==max(heart_rate_mean, na.rm=T) ~ 'Highest Average Heart Rate',
      !is.na(heart_rate_mean) & heart_rate_mean==min(heart_rate_mean, na.rm=T) ~ 'Lowest Average Heart Rate',
      time_of_day == min(time_of_day) ~ 'Earliest Run',
      time_of_day == max(time_of_day) ~ 'Latest Run',
      !is.na(mahalanobis_distance) & mahalanobis_distance==max(mahalanobis_distance, na.rm=T) ~ 'Least Typical Run', 
      !is.na(mahalanobis_distance) & mahalanobis_distance==min(mahalanobis_distance, na.rm=T) ~ 'Most Typical Run',
      TRUE ~ 'None'
    )
  ) %>%
  filter(superlative != 'None') %>%
  select(
    `Run #` = run_no,
    superlative,
    timestamp=timestamp_c,
    total_distance,
    heart_rate_mean,
    temperature_median_v2,
    speed_average,
    cadence_mean,
    mahalanobis_distance,
    time_of_day
  ) %>%
  mutate(
    total_distance = total_distance/1.609,
    speed_average = speed_average/1.609,
    average_pace = 60/speed_average,
    average_pace = format_pace(average_pace),
    temperature_median_v2 = 32 + temperature_median_v2 * 1.8,
    superlative = factor(superlative, levels=superlative_levels)
  )  %>%
  arrange(superlative)

# <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous">
superlatives %>%
  select(-cadence_mean) %>%
  mutate_if(is.numeric, function(x) round(x,1)) %>%
  rename(
    Superlative=superlative,
    Distance=total_distance,
    `Heart Rate`=heart_rate_mean,
    `Temperature`=temperature_median_v2,
    `Speed` = speed_average,
    `"Extremeness"`=mahalanobis_distance,
    `Time of Day`=time_of_day,
    Pace=average_pace
  ) %>%
  knitr::kable(format='html') %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

## when I ran
# need to tabulate all days + hours based on start and stop time

# DOW
dow_count = numeric(7)
names(dow_count)=weekday_string

# HOUR OF DAY
hours_count = numeric(24)
names(hours_count) = as.character(0:23)

# DOW + HOUR OF DAY
dow_hours_count = numeric(7*24)
names(dow_hours_count) = apply(expand.grid(weekday_string, as.character(0:23)), 1, paste, collapse=" ")

for (i in 1:nrow(df)){
  row = df[i,]
  hour1 = row$ts_hour 
  hour2 = row$ts_hour_end
  day1 = row$ts_dow
  day2 = row$ts_dow_end
  if (day1 == day2){
    for (hour_val in as.character(hour1:hour2)){
      if (hour_val=='3'){
        print(row)
        print(as.character(hour1:hour2))  
      }
      hours_count[hour_val] =  hours_count[hour_val] + 1
      dow_hours_count[paste(day1, hour_val)] = dow_hours_count[paste(day1, hour_val)] + 1
    }
    dow_count[day1]  = dow_count[day1] + 1
  } else {
    # assume only 2 days (I don't run *that* long)
    for (hour_val in as.character(hour1:23)){
      if (hour_val=='3'){
        print(row)
        print(as.character(hour1:23))
      }
      hours_count[hour_val] =  hours_count[hour_val] + 1
      dow_hours_count[paste(day1, hour_val)] = dow_hours_count[paste(day1, hour_val)] + 1
    }
    for (hour_val in as.character(0:hour2)){
      if (hour_val=='3'){
        print(row)
        print(as.character(0:hour2))  
      }
      hours_count[hour_val] =  hours_count[hour_val] + 1
      dow_hours_count[paste(day2, hour_val)] = dow_hours_count[paste(day2, hour_val)] + 1
    }
    dow_count[day1]  = dow_count[day1] + 1
    dow_count[day2]  = dow_count[day2] + 1
  }
}
# convert to data frame
dow_frame = as.data.frame(dow_count)
dow_frame$dow = as_factor(names(dow_count))
names(dow_frame)[1] = 'count'

hour_frame = as.data.frame(hours_count) %>%
  mutate(
    hour=0:23
  )
names(hour_frame)[1] = 'count'

dow_hour_frame = as.data.frame(dow_hours_count)
dow_hour_frame$combined_name = names(dow_hours_count)
dow_hour_frame = dow_hour_frame %>%
  mutate(
    hour = as.numeric(gsub('.* ','', combined_name)),
    dow = gsub(' .*', '', combined_name),
    dow=factor(dow, levels=weekday_string)
  )
names(dow_hour_frame)[1] = 'count'

# now plot

# hour
png('hour_2019.png', height=720, width=720, res=PNG_RES)
print(
ggplot(hour_frame) + 
  geom_bar(aes(x=hour, y=count), stat='identity') + 
  geom_text(aes(x=hour, y=count, label=count), vjust=0, size=5.5) + 
  scale_x_continuous(breaks=0:23) + 
  xlab('Hour of Day') + 
  ylab('Number of Runs') + 
  ggtitle('Number of Runs Occuring During Hours of Day in 2019')
)
dev.off()

# hour
png('dow_2019.png', height=840, width=840, res=PNG_RES)
print(
ggplot(dow_frame) + 
  geom_bar(aes(x=dow, y=count), stat='identity') + 
  geom_text(aes(x=dow, y=count, label=count), vjust=0, size=5.5) + 
  #scale_x_discrete(breaks=0:23) + 
  xlab('Day of Week') + 
  ylab('Number of Runs') + 
  ggtitle('Number of Runs Occuring During Days of Week in 2019')
)
dev.off()

# hour + dow
png('dow_hour_2019.png', height=840, width=840, res=PNG_RES)
print(
ggplot(dow_hour_frame) + 
  geom_tile(
    aes(x=dow, y=hour, fill=count)
  ) +
  scale_fill_gradientn(colors=cet_pal(7, 'inferno')) + 
  geom_text(aes(x=dow,y=hour,label=count)) + 
  scale_y_reverse(breaks=0:23) + 
  xlab('Day of Week') + 
  ylab('Hour') + 
  ggtitle('Number of Runs Occuring During Specific Hours of the Week\nin 2019') 
)
dev.off()

## weekly mileage, temperature, and pace
df_week_summary = df %>%
  group_by(week_start) %>%
  summarize(
    temperature = mean(temperature_median_v2, na.rm=T)*1.8 + 32,
    total_weekly_distance = sum(total_distance)/1.609,
    average_heart_rate = mean(heart_rate_mean, na.rm=T),
    average_speed = mean(speed_average)/1.609,
    weighted_average_speed = weighted.mean(speed_average, total_distance)/1.609,
    average_pace = format_pace(60/average_speed),
    weighted_average_pace = format_pace(60/weighted_average_speed),
    n_runs=n()
  )
  
png('weekly_distance_2019.png', height=960, width=840, res=PNG_RES)
print(
ggplot(df %>% inner_join(df_week_summary, by='week_start', suffix=c('','_weekly'))) + 
  geom_bar(data=df_week_summary,aes(x=week_start, y=total_weekly_distance, fill=60/weighted_average_speed), stat='identity') +
  geom_point(aes(x=week_start, y=total_distance/1.609), alpha=0.7) +
  scale_fill_gradientn(
    'Average Pace (min/mi)', label=format_pace, colors=rev(cet_pal(7, 'inferno')),
    breaks=seq(6,9, 0.25)
  ) + 
  coord_flip() +
  scale_x_discrete('', label=monthday_format) + 
  ylab('Weekly Distance (mi)') + 
  geom_text(
    data=df_week_summary,
    aes(x=week_start, y=total_weekly_distance+0.2, label=round(total_weekly_distance)), 
    alpha=0.7, size=4, hjust=0
  ) + 
  guides(size=FALSE, alpha=FALSE) + 
  ggtitle('2019 Weekly Total Distance, Colored by Average Pace', subtitle='distances of individual runs indicated by dots')
)
dev.off()

png('weekly_pace_2019.png', height=960, width=840, res=PNG_RES)
print(
ggplot(df %>% inner_join(df_week_summary, by='week_start', suffix=c('','_weekly'))) + 
  geom_bar(data=df_week_summary,aes(x=week_start, y=60/weighted_average_speed, fill=total_weekly_distance), stat='identity') +
  geom_point(aes(x=week_start, y=60/speed_average * 1.609), alpha=0.7) +
  scale_fill_gradientn(
    'Distance (mi)', colors=cet_pal(7, 'inferno')
  ) + 
  coord_flip() +
  scale_x_discrete('',label=monthday_format) + 
  scale_y_continuous('Average Pace (min/mi)', label=format_pace) +
  geom_label(data=df_week_summary, aes(x=week_start, y=0*60/weighted_average_speed, label=format_pace(60/weighted_average_speed)),
             hjust=0, size=2, alpha=0.5, label.padding=unit(0.1, 'lines')) + 
  ggtitle('2019 Weekly Average Pace, Colored by Total Distance', subtitle='paces of individual runs indicated by dots')
)
dev.off()

png('pace_vs_hr_2019.png', height=720, width=720, res=PNG_RES)
print(
ggplot(df) + 
  geom_smooth(aes(x=60/speed_average*1.609, y=heart_rate_mean), method='lm') +
  geom_point(aes(x=60/speed_average*1.609, y=heart_rate_mean, color=total_distance/1.609)) + 
  scale_color_gradientn('Distance (mi)', colors=cet_pal(7, 'inferno')) + 
  theme_bw() + 
  scale_x_continuous('Average Pace (min/mile)', label=format_pace) + 
  ylab('Average Heart Rate (bpm)') + 
  ggtitle('Pace vs. Heart Rate for Runs in 2019', subtitle='approx. 9 bpm per min/mi increase in speed')
)
dev.off()


summary(lm(heart_rate_mean ~ I(60/speed_average*1.609) , data=df))


#









