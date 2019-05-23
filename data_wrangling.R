library(tidyverse)

## Load data
data<- tribble(
  ~device, ~lighting, ~time,
  1, 1, 1656,
  1, 2, 1406,
  2, 1, 1022,
  2, 2, 1667
)

#Sorting
data %>% 
  arrange(time)

data %>%
  arrange(desc(time))

#Selecting
data %>%
  select(device,time)

data %>%
  select(-device)

data %>%
  select(lighting,everything())

data %>%
  select(DeviceType = device,LightingConditions = lighting, ReadingTime = time)

data %>%
  select(matches('time'))

data %>%
  select(starts_with('l'))

data %>%
  select(ends_with('e'))

# Filtering
data %>%
  filter(device == 2)

data %>%
  filter(device == 1 & time>1200)

# Combining columns
united = data %>%
  unite(conditions, device, lighting)

#Separating columns
united %>%
  separate(conditions,c('device','lighting'))


# Binding columns
data2<- tribble(
  ~participant,
  'P01',
  'P02',
  'P03',
  'P04'
)

bind_cols(data,data2)


# Binding rows
data2<- tribble(
  ~device, ~lighting, ~time,
  1, 1, 1500,
  1, 2, 1634,
  2, 1, 1422,
  2, 2, 1547
)

bind_rows(data,data2)

# Joins
data1<- tribble(
  ~device, ~lighting, ~participant,
  1, 1, 'P01',
  1, 2, 'P01',
  2, 1, 'P02', 
  2, 2, 'P02',
  1, 1, 'P02'
)

data2<- tribble(
  ~device, ~lighting, ~participant, ~time,
  1, 1, 'P01',1500,
  1, 2, 'P01', 1634,
  2, 1, 'P02',1422,
  2, 2, 'P02',1547
)

inner_join(data1,data2,by = c('device','lighting','participant'))
full_join(data1,data2,by = c('device','lighting','participant'))
left_join(data1,data2,by = c('device','lighting','participant'))
right_join(data1,data2,by = c('device','lighting','participant'))

# Set operations
data1<- tribble(
  ~device, ~lighting, ~participant, ~time,
  1, 1, 'P01',1500,
  1, 2, 'P01', 1634,
)

data2<- tribble(
  ~device, ~lighting, ~participant, ~time,
  1, 1, 'P01',1500,
  1, 2, 'P01', 1634,
  2, 1, 'P02',1422,
  2, 2, 'P02',1547
)

union(data1,data2)
intersect(data1,data2)
setdiff(data2,data1)


# Changing the shape of the data
data <- tribble(
  ~device, ~lighting, ~participant, ~time,
  1, 1, 'P01',1500,
  1, 2, 'P02', 1634,
  2, 1, 'P01',1422,
  2, 2, 'P02',1547
)

wide <- data %>%
  spread(device,time)

wide %>%
  gather(device,time,`1`,`2`)

# Summarizing data
data <- tribble(
  ~device, ~lighting, ~participant, ~time,
  1, 1, 'P01',1500,
  1, 2, 'P02', 1634,
  2, 1, 'P01',1422,
  2, 2, 'P02',1547
)

data %>%
  group_by(participant) %>%
  summarize(mean_time = mean(time))

data %>%
  group_by(participant) %>%
  summarize(sum_time = sum(time))

data %>%
  group_by(device) %>%
  summarize(count=n())

# Mutating
data %>%
  mutate(time = time/60)

data %>%
  mutate(trial = row_number()) %>%
  group_by(participant)%>%
  mutate(order = row_number())%>%
  ungroup()

# Working with strings
data %>% 
  mutate(user = str_sub(participant,start = 2))

# Durations
import::from(lubridate, seconds_to_period, minute, second)
data %>%
  mutate(
    period = seconds_to_period(time),
    minutes = minute(period)
  )
