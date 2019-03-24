library(tidyverse)
import::from(ggbeeswarm,  geom_beeswarm)

data <- read_csv("quanthci-blog/data/mobile_3d_tasktime.csv", col_types = "cccd") %>%
  mutate(Treatment = as.factor(Treatment),
         Display = as.factor(Display),
         Task = as.factor(Task))

## Boxplot
data %>% 
  ggplot(aes(x = Task, y = Time, fill = Display)) +
  geom_boxplot()

# Violin
data %>% 
  ggplot(aes(x = Task, y = Time, fill = Display)) +
  geom_violin()+
  facet_grid(. ~ Display)

# Beeswarm
data %>% 
  ggplot(aes(x = Display, y = Time, fill = Task)) +
  geom_beeswarm(cex=3)+
  facet_grid(. ~ Task)

# Histogram
data %>% 
  ggplot(aes(x = Time)) +
  geom_histogram() +
  facet_grid(Task ~ Display)

# Density plot
data %>% 
  ggplot(aes(x = Time,fill=Display))+ 
  geom_density() +
  facet_grid(Task ~ .)
