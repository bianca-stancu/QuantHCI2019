assulibrary(tidyverse)
import::from(modelr, add_residuals)
import::from(car, qqPlot, ncvTest)
import::from(nortest, lillie.test)
import::from(lawstat, levene.test)
import::from(ARTool, art)
import::from(car, sigmaHat)
import::from(car, Anova)
import::from(modelr, add_predictions)

# Helper function for calculating geometric mean
mean_cl_geom <- function(x) {
  log_x <- log(x)
  t_result_log <- t.test(log_x)
  tibble(
    y = exp(t_result_log$estimate),
    ymin = exp(t_result_log$conf.int[1]),
    ymax = exp(t_result_log$conf.int[2]))
}


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

## Transformation: Log transformation
m_log <- lm(log(ReadingTime) ~ Device, data = data)
data_log_aug <- data %>% add_residuals(m_log)
qqPlot(data_log_aug$resid)  
ncvTest(m_log) #

## Transformation: Aligned ranks
art_result <- art(ReadingTime ~ Device, data = data)

data <-
  data %>%
  mutate(
    art_Device = art_result$aligned.ranks$Device
  )

# Analyzing aligned-rank-transformed data
m_art_Device <- lm(art_Device ~ Device, data = data)
anova(m_art_Device)

pairwise_art <- glht(m_art_Device, linfct = mcp(Device = "Tukey"))

# Cohen's ds and their CI
ci_pairwise_art <- tidy(confint(pairwise_art))
scale_factor <- sigmaHat(m_art_Device)
ci_pairwise_art_d <-
  ci_pairwise_art %>%
  mutate(
    estimate  = estimate / scale_factor,
    conf.low  = conf.low / scale_factor,
    conf.high = conf.high / scale_factor
  )

# Plot
p_pairwise_art_d <-
  ci_pairwise_art_d %>%
  ggplot(aes(x = lhs, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, color = "red") +
  xlab("hypothesis") +
  ylab("Cohen's d effect size") +
  coord_flip()
p_pairwise_art_d



## Examining residuals
m_original <- lm(ReadingTime ~ Device, data = data)
m_log <- lm(log(ReadingTime) ~ Device, data = data)

data_aug <-
  data %>%
  add_residuals(m_original, var = "resid_original") %>%
  add_residuals(m_log, var = "resid_log") %>%
  add_residuals(m_art_Device, var = "resid_art")
par(mfrow = c(1,3))
qqPlot(data_aug$resid_original)
qqPlot(data_aug$resid_log)
qqPlot(data_aug$resid_art)
par(mfrow = c(1,1))

#===============================================================================
## Generalized linear model
# Load the data
gpa_admit <- read_csv("quanthci-blog/data/binary.csv", col_types = "cd")
gpa_admit <-
  gpa_admit %>%
  mutate(
    admit_fct = factor(admit, levels = c("0", "1")),  
    Prob_Admit = as.numeric(admit_fct) - 1
  )

# Model fitting
m_logit_cont <- glm(admit_fct ~ gre, data = gpa_admit, family = binomial(link = "logit"))
anova_cont <- Anova(m_logit_cont, type = "II")
anova_cont

coeff_cont <- tidy(m_logit_cont, conf.int = TRUE, exponentiate = TRUE)
coeff_cont

# Plot model

p_prediction <-
  gpa_admit %>%
  add_predictions(m_logit_cont) %>%
  mutate(Prob = plogis(pred)) %>%
  ggplot(aes(x = gre, color = admit)) +
  geom_point(aes(y = Prob_Admit)) +
  geom_line(aes(y = Prob), color = "blue") +
  ylab("Probability of being admitted") +
  theme(legend.position = "none")
p_prediction





