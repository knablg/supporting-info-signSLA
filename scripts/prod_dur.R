# 1 Data preparation
# 1.1 Read data
df_raw = read.table("C:/Users/lnxin/Desktop/results/stat analysis/prod_dur.csv", 
                header=TRUE, sep=',')

# 1.2 Convert data types
df_raw$participant = as.factor(df_raw$participant)
df_raw$word = as.factor(df_raw$word)
df_raw$iconicity = as.factor(df_raw$iconicity)
df_raw$complexity = as.factor(df_raw$complexity)

# 1.3 View and adjust baseline levels
df_raw$complexity <- relevel(df_raw$complexity, ref = "low")
df_raw$iconicity <- relevel(df_raw$iconicity, ref = "low")

# 1.4 Remove outliers
dur_mean = mean(df_raw$duration, na.rm = TRUE)
dur_sd = sd(df_raw$duration, na.rm=TRUE)
lower_bound = dur_mean - 3 * dur_sd
upper_bound = dur_mean + 3 * dur_sd
df = df_raw[df_raw$duration < upper_bound,] 


# 2 Normality check and data transformation
# 2.1 Normality tests
qqnorm(df$duration)
qqline(df$duration)
hist(df$duration, breaks = 30, main = 'histogram of Duration', xlab = 'duration')
# 2.2 Log transformation
df$logdur = log(df$duration)
qqnorm(df$logdur)
qqline(df$logdur)
hist(df$logdur, breaks = 30, main = 'histogram of log_duration', xlab = 'rt')


# 3 Modeling
# 3.1 Model comparison and selection
library(lme4)
model1 = lmer(logdur ~ iconicity * complexity + (1 | word) + (1 | participant),
              data = df,
              REML = FALSE)
isSingular(model1)

model2 = lmer(logdur ~ iconicity * complexity + 
                (1 | word) + (1 + iconicity | participant),
              data = df,
              REML = FALSE)
isSingular(model2)
summary(model2)
summary(model1)
anova(model1, model2) # summary: model1 better than model2

# 3.2 Confirm model
# 3.2.1 modeling
model = lmer(logdur ~ iconicity * complexity + (1 | word) + (1 | participant),
                 data = df,
                 REML = TRUE)
isSingular(model)
summary(model)
# 3.2.2 Residual distribution check
plot(model, main = 'Residuals versus Fitted Values')
qqnorm(residuals(model, type = 'pearson'), main='Q-Q Plot of Standardized Residuals')
qqline(residuals(model, type = 'pearson'))


# 4 Main effect analysis
library(lmerTest)
model_log = lmer(logdur ~ iconicity * complexity + (1 | word) + (1 | participant),
             data = df,
             REML = TRUE)
anova2 = anova(model_log, type=3)
anova2

# Simple effects analysis
library(emmeans)
emmeans(model_log, pairwise ~ complexity | iconicity, p.adjust.methods = 'bonf')
emmeans(model_log, pairwise ~ iconicity | complexity, p.adjust.methods = 'bonf')

# Visualisation
library(ggplot2)
emm_interaction = emmeans(model_log, spec = c("complexity", "iconicity"))
emm_df = as.data.frame(emm_interaction)

p2 = ggplot(
  data = emm_df,
  aes(
    x = complexity,
    y = emmean,
    color = iconicity,
    group = iconicity,
    shape = iconicity
  )
) + 
  geom_line(size = 1) + 
  geom_point(size = 4) + 
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.1,
    position = position_dodge(0.05)
  ) + 
  labs(
    x = 'Phonological Complexity Level',
    y = 'Log-transformed Production Duration',
    color = 'Iconicity',
    shape = 'Iconicity',
    title = 'Interaction Effect of Iconicity and Phonological Complexity on Production Duration'
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = 'bottom'
  )
p2

ggsave(
  filename = "prod_dur_interaction.png",
  plot = p2,
  path = "C:/Users/lnxin/Desktop/results/stat analysis/image/",
  device = 'png',
  width = 8,
  height = 6,
  dpi = 300
)



p22 = ggplot(
  data = emm_df,
  aes(
    x = factor(complexity, labels = c('Low', 'High')),
    y = emmean,
    color = factor(iconicity, labels = c('Low','High')),
    group = iconicity,
  )) + 
  geom_line(size = 1.2) + 
  geom_point(size = 4) + 
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.1
    #position = position_dodge(0.1)
  ) + 
  labs(
    x = 'Phonological Complexity Level',
    y = 'Log-transformed Production Duration',
    color = 'Iconicity Level'
  ) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = 'bold'),
    axis.title.y = element_text(size = 14, face = 'bold'),
  )
p22

ggsave(
  filename = "prod_dur_interaction_4.png",
  plot = p22,
  path = "C:/Users/lnxin/Desktop/results/stat analysis/image/",
  device = 'png',
  width = 10,
  height = 6,
  dpi = 300
)
