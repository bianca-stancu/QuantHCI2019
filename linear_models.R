library(tidyverse)
import::from(multcomp, glht, mcp, contrMat)
import::from(broom, tidy)
import::from(psycho, analyze)

## Load data and transform into factors
data <- read_csv("quanthci-blog/data/ereader.csv", col_types = "ccd") %>%
mutate(Device = as.factor(Device),
       Lighting = as.factor(Lighting))

## Visualizing the data
pd <- position_dodge(0)

#Plotting device and lighting
plot_device_lighting <- 
  data %>% 
  ggplot(aes(x = Device, y = ReadingTime, color = Lighting, group = Lighting)) +
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 3, position = pd) + 
  stat_summary(fun.y = mean, geom = "line", position = pd) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0, position = pd) + 
  ylab("Mean(time) and 95% CI (from individual group)") +
  expand_limits(y = 0)
plot_device_lighting

#Plotting device
plot_device <- 
  data %>% 
  ggplot(aes(x = Device, y = ReadingTime)) +
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 3, position = pd) + 
  stat_summary(fun.y = mean, geom = "line", position = pd) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0, position = pd) + 
  ylab("Mean(time) and 95% CI (from individual group)") +
  expand_limits(y = 0)
plot_device



#Plotting lighting
plot_lighting <- 
  data %>% 
  ggplot(aes(x = Lighting, y = ReadingTime)) +
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 3, position = pd) + 
  stat_summary(fun.y = mean, geom = "line", position = pd) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0, position = pd) + 
  ylab("Mean(time) and 95% CI (from individual group)") +
  expand_limits(y = 0)
plot_lighting

## One IV model ("Device")
m_device <- lm(ReadingTime ~ Device, data = data)
anova_m_device <- anova(m_device)
results_anova_device <- analyze(anova_m_device)
print(results_anova_device)
summary(results_anova_device) 

## Two IV
m_device_lighting <- lm(ReadingTime ~ Device * Lighting, data = data)
anova_m_device_lighting <- anova(m_device_lighting)
results_anova_device_lighting <- analyze(anova_m_device_lighting)
print(results_anova_device_lighting)
summary(results_anova_device_lighting)

##  Generalized linear hypothesis on the main effect
m_main <-  update(m_device_lighting, .~. - Lighting:Device)  
pairwise_main <- 
  glht(m_main,
       linfct = mcp(
         Device = "Tukey",
         Lighting = "Tukey"))
pairwise_main$linfct
##Plot the effects
p_pairwise_main <- 
  tidy(confint(pairwise_main))
p_pairwise_main %>% 
  mutate(Hypothesis = paste(lhs, "==", rhs)) %>% 
  ggplot(aes(x = Hypothesis, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_pointrange() +
  expand_limits(y = 0) +
  coord_flip() +
  ylab("Estimate of the difference with 95% CI\n(adjusted with the single-step method)")







