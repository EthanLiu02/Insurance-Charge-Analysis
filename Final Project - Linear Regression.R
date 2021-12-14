library(readxl)
library(tidyverse)


insurance = read.csv("/home/ethan/Desktop/RStudio Workspace/Final Project/insurance.csv")


str(insurance)

# As an insurance company, a goal to achieve with data science is to model the cost a person will need in medical costs, and add a premium on 
# top of that to stay profitable. By taking actual insurance quote data and applying it to models, it is possible for us reverse engineer
# what the insurance companies has been using to calculate their customers' costs.

# In this data set, 7 variables are present, with the value we want to predict being insurance charges, and the other six variables being age,
# sex, BMI, number of children, whether the insured person is a smoker and the region they are in.



plot_1 = insurance %>% ggplot(aes(x = age, y = charges))+
  geom_point(alpha = 0.1)

ggsave(filename = "Plot_1.png", plot_1,
       width = 8, height = 5, dpi = 150, units = "in", device='png', limitsize = FALSE)

# There are three distinct tiers, and based on the shade, the bottom tier is the most dominant of the three. The insurnace charges also goes
# up in any tier with age.



plot_2 = insurance %>% ggplot(aes(x = age, y = charges, color = as.factor(children)))+
  geom_point(alpha = 0.8)

ggsave(filename = "Plot_2.png", plot_2,
       width = 8, height = 5, dpi = 150, units = "in", device='png', limitsize = FALSE)

# The tiers are not caused by the number of children one may have.



plot_3 = insurance %>% ggplot(aes(x = age, y = charges, color = as.factor(sex)))+
  geom_point(alpha = 0.8)

ggsave(filename = "Plot_3.png", plot_3,
       width = 8, height = 5, dpi = 150, units = "in", device='png', limitsize = FALSE)

# Neither are the tiers caused by gender.



plot_4 = insurance %>% ggplot(aes(x = age, y = charges, color = as.factor(smoker)))+
  geom_point(alpha = 0.8)

ggsave(filename = "Plot_4.png", plot_4,
       width = 8, height = 5, dpi = 150, units = "in", device='png', limitsize = FALSE)

# There is now a very distinct divide between smokers and nonsmokers, with smokers having much higher charges.







Linear_model_1 = lm(charges~., insurance)

summary(Linear_model_1) # R squared of 0.7509

# This is the linear model that uses every single parameter available to estimate the cost of insurance. As we can see, the region is large irrelevant
# (as expected), and neither does gender produce a noticeable difference.

# The four factors that are important are: age, BMI, number of children, and whether the insured is a smoker.
# The smoker aspect had already been explored and visualized by plot 4.

# The model can be optimized by simply removing the relatively insignificant factors:

Linear_model_2 = lm(charges~age+bmi+children+smoker, insurance)

summary(Linear_model_2) # R squared of 0.7497

# This linear model has now been simplified down to the four most important factors, but the accuracy of the model has not improved much.



plot_5 = insurance %>% ggplot(aes(x = age, y = charges, color = as.factor(smoker)))+
  geom_point(alpha = 0.8)+
  geom_abline(slope=280,intercept=200,col='red')+
  geom_abline(slope=256.9,intercept=20000,col='red')

ggsave(filename = "Plot_5.png", plot_5,
       width = 8, height = 5, dpi = 150, units = "in", device='png', limitsize = FALSE)

# However, there still lacks an explanation for why the charges are distinctly divided into three groups instead of two. 
# It should be noted, however, that there are virtually no nonsmokers in tier 3, and likewise so for smokers in tier 1.
# Therefore, in the following model, the lower red line only applies to nonsmokers, while the higher red line applies only to smokers.
# That is, smokers and nonsmokers will each be divided into two tiers.



Nonsmoker = insurance %>%
  filter(smoker == "no")

Smoker = insurance %>%
  filter(smoker != "no")


plot_6_n = Nonsmoker %>% ggplot(aes(x = age, y = charges))+
  geom_point(alpha = 0.8)+
  geom_abline(slope=280,intercept=200,col='red')

ggsave(filename = "Plot_6_n.png", plot_6_n,
       width = 8, height = 5, dpi = 150, units = "in", device='png', limitsize = FALSE)

plot_6_s = Smoker %>% ggplot(aes(x = age, y = charges))+
  geom_point(alpha = 0.8)+
  geom_abline(slope=256.9,intercept=20000,col='red')

ggsave(filename = "Plot_6_s.png", plot_6_s,
       width = 8, height = 5, dpi = 150, units = "in", device='png', limitsize = FALSE)

# These two plots show the tiers chosen for smokers and nonsmokers.



Tier_1_nonsmoker = Nonsmoker %>%
  filter(charges < (200 + age*280))

Tier_2_nonsmoker = Nonsmoker %>%
  filter(charges > (200 + age*280))

table(Tier_1_nonsmoker$children)
table(Tier_2_nonsmoker$children)

summarize(Tier_1_nonsmoker, average = mean(bmi), sd = sd(bmi))
summarize(Tier_2_nonsmoker, average = mean(bmi), sd = sd(bmi))

summarize(Tier_1_nonsmoker, average = mean(age), sd = sd(age))
summarize(Tier_2_nonsmoker, average = mean(age), sd = sd(age))

# As seen here, while the parameters we are given are nearly statistically identical, the charges are greatly divided.

Tier_1_smoker = Smoker %>%
  filter(charges < (20000 + age*256.9))

Tier_2_smoker = Smoker %>%
  filter(charges > (20000 + age*256.9))

table(Tier_1_smoker$children)
table(Tier_2_smoker$children)

summarize(Tier_1_smoker, average = mean(bmi), sd = sd(bmi))
summarize(Tier_2_smoker, average = mean(bmi), sd = sd(bmi))

summarize(Tier_1_smoker, average = mean(age), sd = sd(age))
summarize(Tier_2_smoker, average = mean(age), sd = sd(age))

# The comparison procedure can be repeated for smokers with similar results for both groups, except that BMI plays a large factor.



plot_7_n = Nonsmoker %>% ggplot(aes(x = age, y = charges, color = bmi))+
  geom_point(alpha = 0.8)+
  binned_scale(aesthetics = "color",
               scale_name = "stepsn", 
               palette = function(x) c("#001833", "#0058B8", "#028FEE", "#05DAFF", "#00FFD9", "green", "#C0FF02", "yellow", "#FDD003", "#FE9703", "red"),
               breaks = c(25, 28.5, 32, 35.5, 39, 42.5, 45, 47.5, 50, 52.5),
               limits = c(15,55),
               show.limits = TRUE, 
               guide = "colorsteps",
               labels = comma
  )

ggsave(filename = "Plot_7_n.png", plot_7_n,
       width = 8, height = 5, dpi = 150, units = "in", device='png', limitsize = FALSE)



plot_7_s = Smoker %>% ggplot(aes(x = age, y = charges, color = bmi))+
  geom_point(alpha = 0.8)+
  binned_scale(aesthetics = "color",
               scale_name = "stepsn", 
               palette = function(x) c("#001833", "#0058B8", "#028FEE", "#05DAFF", "#00FFD9", "green", "#C0FF02", "yellow", "#FDD003", "#FE9703", "red"),
               breaks = c(25, 28.5, 32, 35.5, 39, 42.5, 45, 47.5, 50, 52.5),
               limits = c(15,55),
               show.limits = TRUE, 
               guide = "colorsteps",
               labels = comma
  )

ggsave(filename = "Plot_7_s.png", plot_7_s,
       width = 8, height = 5, dpi = 150, units = "in", device='png', limitsize = FALSE)

# These two plots confirm the claims made so far regarding BMI.



nrow(Tier_2_nonsmoker)/(nrow(Tier_1_nonsmoker)+nrow(Tier_1_nonsmoker)) # 5.13%
nrow(Tier_2_smoker)/(nrow(Tier_1_smoker)+nrow(Tier_2_smoker)) # 55.11%

# However, here we notice that much more of the smoker would fall into the higher price tier than nonsmokers. A plausible explanation is that the higher
# price tier applies when the insured has other chronic illness, which is more frequent when the person smokes. And in this case, the factor for smokers
# appears to be BMI, and there is no such factor for nonsmokers in this data set.



# There are two ways to approach this. Since consideration for BMI is evidently different for nonsmokers and smokers, we can first exclude tier 2 for
# both smokers and nonsmokers.

Tier_1_all = full_join(Tier_1_nonsmoker, Tier_1_smoker)

plot_8 = Tier_1_all %>% ggplot(aes(x = age, y = charges, color = as.factor(smoker)))+
  geom_point(alpha = 0.8)

ggsave(filename = "Plot_8.png", plot_8,
       width = 8, height = 5, dpi = 150, units = "in", device='png', limitsize = FALSE)

# There is a noticeable improvement in linear correlation.

Linear_model_3 = lm(charges~age+bmi+children+smoker, Tier_1_all)

summary(Linear_model_3) # R squared of 0.9797

# And as expected, once the tier 2 values are removed, the remaining values present a very clear linear correlation. The model also indicates that
# BMI is not as strongly significant as the other three parameters. If it is to be removed:

Linear_model_4 = lm(charges~age+children+smoker, Tier_1_all)

summary(Linear_model_4) # R squared of 0.9797

# Very minor change overall, however, the model becomes very simplistic.

# Then, following which, tier 2 smokers get their own linear model:

Linear_model_5 = lm(charges~age+bmi+children+smoker, Tier_2_smoker)

summary(Linear_model_5) # R squared of 0.8545

# This model provides very strong approximations for most of the data.




















# A second way of approach is to keep the tier 2 smokers in the data, and construct a linear model from there.

No_tier_2_nonsmoker = full_join(full_join(Tier_1_nonsmoker, Tier_1_smoker), Tier_2_smoker)

plot_9 = No_tier_2_nonsmoker %>% ggplot(aes(x = age, y = charges, color = as.factor(smoker)))+
  geom_point(alpha = 0.8)

ggsave(filename = "Plot_9.png", plot_9,
       width = 8, height = 5, dpi = 150, units = "in", device='png', limitsize = FALSE)

# There is a noticeable improvement in linear correlation.

Linear_model_6 = lm(charges~age+bmi+children+smoker, No_tier_2_nonsmoker)

summary(Linear_model_6) # R squared of 0.8540

# This model is weaker than either of the two generated in the first approach. This approach is not recommended, as it is not specific to the different
# situations between smokers and nonsmokers.