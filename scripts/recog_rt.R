# 1 Data preparation
# 1.1 Read data
df = read.table("C:/Users/lnxin/Desktop/results/stat analysis/recog_rt.csv", 
                header=TRUE, sep=',')

# 1.2 Convert data types
df$participant = as.factor(df$participant)
df$word = as.factor(df$word)
df$iconicity = as.factor(df$iconicity)
df$complexity = as.factor(df$complexity)

# 1.3 View and adjust baseline levels
df$complexity <- relevel(df$complexity, ref = "low")
df$iconicity <- relevel(df$iconicity, ref = "low")
levels(df$iconicity)

# 1.4 Remove outliers
rt_mean = mean(df$rt, na.rm = TRUE)
rt_sd = sd(df$rt, na.rm=TRUE)
lower_bound = rt_mean - 3 * rt_sd
upper_bound = rt_mean + 3 * rt_sd
df2 = df[df$rt < upper_bound,] 


# 2 Descriptive statistics(optional)
# 2.1 Grouped description
mean = aggregate(rt ~ iconicity + complexity, 
                 data = df2, FUN = mean)
mean
sd = aggregate(rt ~ iconicity + complexity, 
               data = df2, FUN = sd)
sd
min = aggregate(rt ~ iconicity + complexity, 
                data = df2, FUN = min)
min
max = aggregate(rt ~ iconicity + complexity, 
                data = df2, FUN = max)
max


# 3 Normality check and data transformation
# 3.1 Normality tests
shapiro.test(df2$rt)
ks.test(df2$rt, "pnorm",mean = mean(df2$rt), sd = sd(df2$rt))
qqnorm(df2$rt)
qqline(df2$rt)
hist(df2$rt, breaks = 20, main = 'histogram of R', xlab = 'rt')

# 3.3 Log transformation
df2$logRT = log(df2$rt)
qqnorm(df2$logRT)
qqline(df2$logRT)
hist(df2$logRT, breaks = 30, main = 'histogram of R', xlab = 'rt')


# 4 Modeling
# 4.1 Model comparison and selection
library(lme4)
library(Matrix)

model1 = lmer(logRT ~ iconicity * complexity + (1 | word) + (1 | participant),
              data = df2,
              REML = FALSE)
summary(model1)
isSingular(model1) 

model2 = lmer(logRT ~ iconicity + complexity + (1 | word) + (1 | participant),
              data = df2,
              REML = FALSE) 
summary(model2)
isSingular(model2)

anova(model1, model2)  # summary: model2 better than model1

model3 = lmer(logRT ~ iconicity + complexity 
              + (1 | word) + (1 + iconicity | participant),
              data = df2,
              REML = FALSE
              ) 
summary(model3)
isSingular(model3)

anova(model2, model3) # summary: model2 better than model3

# 4.2 Confirm model
# 4.2.1 modeling
model = lmer(logRT ~ iconicity + complexity + (1 | word) + (1 | participant),
             data = df2,
             REML = TRUE)
summary(model)
isSingular(model)
# 4.2.2 Residual distribution check
plot(model, main = 'Residuals versus Fitted Values')
qqnorm(residuals(model, type = 'pearson'), main='Q-Q Plot of Standardized Residuals')
qqline(residuals(model, type = 'pearson'))
# or:
# library(performance)
# check_model(model)

# 5 Main effect analysis
library(lmerTest)
model = lmer(logRT ~ iconicity + complexity + (1 | word) + (1 | participant),
             data = df2,
             REML = TRUE)
anova1 = anova(model, type=3)
anova1


# Visualisation
library(emmeans)
library(ggplot2)

emm_X <- emmeans(model, specs = "iconicity")
emm_X_df <- as.data.frame(emm_X)

emm_Y <- emmeans(model, specs = "complexity") 
emm_Y_df <- as.data.frame(emm_Y)

p = ggplot(emm_X_df, aes(x = iconicity, y = emmean, group = 1)) + 
  geom_line(size = 1) +  
  geom_point(size = 3) +  
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.1) +  
  labs(x = "Iconicity Level", y = "Log-transformed recognition RT") +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = 'bold'),
    axis.title = element_text(size = 14, face = 'bold')
        )
p

ggsave(
  filename = "rt_iconicity_1.png",
  plot = p,
  path = "C:/Users/lnxin/Desktop/results/stat analysis/image/",
  device = 'png',
  width = 10,
  height = 6,
  dpi = 300
)

