assulibrary(tidyverse)
import::from(modelr, add_residuals)
import::from(car, qqPlot, ncvTest)
import::from(nortest, lillie.test)
import::from(lawstat, levene.test)

data <- read_csv("quanthci-blog/data/ereader.csv", col_types = "ccd") %>%
  mutate(Device = as.factor(Device),
         Lighting = as.factor(Lighting))

m_full <- lm(ReadingTime ~ Device * Lighting, data = data)

##Adding the residuals function
data_aug <- 
  data %>% 
  add_residuals(m_full)

## Visualizing the residuals
data_aug %>% 
  ggplot(aes(x = Device, y = resid)) +
  geom_point()

data_aug %>% 
  ggplot(aes(x = Lighting, y = resid)) +
  geom_point()

## Assumption: normality of residuals
# NULL HYPOTHESIS: Residuals are normally distributed
qqPlot(data_aug$resid)
lillie.test(data_aug$resid) 

## Assumption: Homogeniety of variance
# Brown-Forsythe test 
# NULL HYPOTHESIS: variance of the groups are roughly the same
levene.test(data$ReadingTime, data$Device, location = "median") 
levene.test(data$ReadingTime, data$Lighting, location = "median") 
# Non-constant Variance Score Test 
ncvTest(m_full)   # NULL HYPOTHESIS: variance of residuals are constant
