# Load data
setwd("D:/OneDrive - Imperial College London/Bee project/Data")
data.food<-read.csv("Data.food.csv")
data.main<-read.csv("Data.csv")
data.main.r<-read.csv("Data.r.csv")
data.mortality<-read.csv("Mortality.csv")
data.walk<-read.csv("Data.walk.csv")
data.walk.1<-read.csv("Data.walk.1.csv")
# Packages area
install.packages('ggplot2')
install.packages('tidyr')
install.packages('readr')
install.packages('dplyr')
install.packages('ggtext')
install.packages('lme4')
install.packages("mgcv")
install.packages("randomForest")
install.packages("xgboost")
install.packages("sjPlot")
install.packages("gbm")
install.packages("dismo")
install.packages("sjstats")
install.packages("car")
install.packages("lmerTest")
install.packages("patchwork")
install.packages("cowplot")
install.packages("egg")
install.packages("glmmTMB")
install.packages("broom.mixed")
library(egg)
library(broom.mixed)
library(glmmTMB)
library(cowplot)
library(patchwork)
library(car)
library(sjstats)
library(dismo)
library(gbm)
library(mgcv)
library(randomForest)
library(xgboost)
library(ggplot2)
library(ggtext)
library(lme4)
library(lmerTest)
library(tidyr)
library(dplyr)
library(nlme)
library(sjPlot)

# Load walking data and turn column to value
data.walk.1<-pivot_longer(data.walk.1, names_to = "Round", values_to = "Movement", cols=Walk.1:Walk.5)

data.walk.1$Round[which(data.walk.1$Round == 'Walk.1')] <- '1'
data.walk.1$Round[which(data.walk.1$Round == 'Walk.2')] <- '2'
data.walk.1$Round[which(data.walk.1$Round == 'Walk.3')] <- '3'
data.walk.1$Round[which(data.walk.1$Round == 'Walk.4')] <- '4'
data.walk.1$Round[which(data.walk.1$Round == 'Walk.5')] <- '5'

# Turn data to factor
data.main$Pesticide.Control<-as.factor(data.main$Pesticide.Control)
data.main$Colony<-as.factor(data.main$Colony)
data.main$Name<-as.factor(data.main$Name)
data.main.r$Pesticide.Control<-as.factor(data.main.r$Pesticide.Control)
data.main.r$Colony<-as.factor(data.main.r$Colony)
data.main.r$Name<-as.factor(data.main.r$Name)

data.food$Pesticide.Control<-as.factor(data.food$Pesticide.Control)
data.food$Colony<-as.factor(data.food$Colony)
data.food$Name<-as.factor(data.food$Name)


data.walk$Pesticide.Control<-as.factor(data.walk$Pesticide.Control)
data.walk$Colony<-as.factor(data.walk$Colony)
data.walk$Name<-as.factor(data.walk$Name)
data.walk$Walk.1<-as.factor(data.walk$Walk.1)
data.walk$Walk.2<-as.factor(data.walk$Walk.2)
data.walk$Walk.3<-as.factor(data.walk$Walk.3)
data.walk$Walk.4<-as.factor(data.walk$Walk.4)
data.walk$Walk.5<-as.factor(data.walk$Walk.5)



# Walk.1 is data that turn round as a factor
data.walk.1$Pesticide.Control<-as.factor(data.walk.1$Pesticide.Control)
data.walk.1$Colony<-as.factor(data.walk.1$Colony)
data.walk.1$Name<-as.factor(data.walk.1$Name)
data.walk.1$Round<-as.factor(data.walk.1$Round)
data.walk.1$Movement<-as.factor(data.walk.1$Movement)
# Count Active/Inactive proportion in data.walk
selected_cols<-c("Walk.1","Walk.2","Walk.3","Walk.4","Walk.5")
selected_data<-data.walk[,selected_cols]=="Yes"
selected_data_neg<-data.walk[,selected_cols]=="No"
data.walk$Active<-rowMeans(selected_data)
data.walk$Inactive<-rowMeans(selected_data_neg)

# Turn  data to numeric
data.main$Time<-as.numeric(data.main$Time)
data.main<-transform(data.main, Resp=as.numeric(Resp))
data.main<-transform(data.main, B.Temp=as.numeric(B.Temp))
data.main<-transform(data.main,Temperature=as.numeric(Temperature))
data.main<-transform(data.main,B.Mass=as.numeric(B.Mass))
data.main<-transform(data.main,Walk.v=as.numeric(Walk.v))
data.main.r$Time<-as.numeric(data.main.r$Time)
data.main.r<-transform(data.main.r, Resp=as.numeric(Resp))
data.main.r<-transform(data.main.r, B.Temp=as.numeric(B.Temp))
data.main.r<-transform(data.main.r,Temperature=as.numeric(Temperature))
data.main.r<-transform(data.main.r,B.Mass=as.numeric(B.Mass))
data.main.r<-transform(data.main.r,Walk.v=as.numeric(Walk.v))
data.main.r<-transform(data.main.r,Abs.resp=as.numeric(Abs.resp))

data.food$Time<-as.numeric(data.food$Time)
data.food<-transform(data.food,Food.sum=as.numeric(Food.sum))
data.food<-transform(data.food,Food.sum.env=as.numeric(Food.sum.env))
data.food<-transform(data.food,Temperature=as.numeric(Temperature))
data.food<-transform(data.food,Food.ave=as.numeric(Food.ave))

data.walk$Temperature<-as.numeric(data.walk$Temperature)
data.walk$Time<-as.numeric(data.walk$Time)
data.walk$B.Mass<-as.numeric(data.walk$B.Mass)
data.walk$B.Temp<-as.numeric(data.walk$B.Temp)
data.walk$Resp<-as.numeric(data.walk$Resp)
data.walk$Walk.v<-as.numeric(data.walk$Walk.v)

data.walk.1$Temperature<-as.numeric(data.walk.1$Temperature)
data.walk.1$Time<-as.numeric(data.walk.1$Time)
data.walk.1$B.Mass<-as.numeric(data.walk.1$B.Mass)
data.walk.1$B.Temp<-as.numeric(data.walk.1$B.Temp)
data.walk.1$Resp<-as.numeric(data.walk.1$Resp)
data.walk.1$Walk.v<-as.numeric(data.walk.1$Walk.v)

# Set up colour and lab for Treatment Contol
color_labels <- c( "0"="Control", "1"="20 ppb Thiamethoxam")

# Model Sorting area
## Respiratory

# REmove NA
data.resp.nona<-filter(data.main,!is.na(Resp))
data.resp.abs<-filter(data.main.r,!is.na(Abs.resp))
which(is.na(data.resp.nona$Resp))
hist(data.main$Resp)



# Check if there is linear between resp and bmass
plot(data.resp.nona$B.Mass,data.resp.nona$Resp)
Mass<-lm(Resp~B.Mass,data=data.resp.nona)
summary(Mass)
abline(lm(Resp~B.Mass,data=data.resp.nona))
legend("topleft",legend=paste("R2 is", format(summary(Mass)$r.squared,digits=3)))

# Analyse from the pattern of resp~day, considering it might have three-factors interaction in formula
# We hypothesis as below
res.lm<-lmer(Resp~Time*Pesticide.Control*Temperature+(1|Colony)+(1|Name),data=data.resp.nona)
res.lm2<-lmer(Resp~Time*Pesticide.Control*Temperature+B.Mass+(1|Colony)+(1|Name),data=data.resp.nona)
summary(res.lm)

result_summary <- summary(res.lm.abs)$coefficients %>%
  as.data.frame() %>%
  mutate(
    Estimate = Estimate,
    CI_lower = Estimate - 1.96 * std.error,
    CI_upper = Estimate + 1.96 * std.error,
    p_value = ifelse(abs(t.value) < 1.96, "NS", format.pval(pt(abs(t.value), df.residual))),
    CI = paste0("(", round(CI_lower, 2), " ~ ", round(CI_upper, 2), ")")
  ) %>%
  select(rownames(.), Estimate, CI, p_value)

# Plot it out
print(result_summary)

res.lm.abs<-lmer(Abs.resp~Time*Pesticide.Control*Temperature+(1|Colony)+(1|Name),data=data.resp.abs)
summary(res.lm.abs)
qqnorm(residuals(res.lm))
plot(res.lm,which=2)
summary(res.lm)
plot_model(res.lm, type="int")
performance::r2(res.lm)
performance::r2(res.lm.abs)
anova(res.lm)
library(broom.mixed)


# 使用tidy()函数提取结果汇总
result_summary <- tidy(res.lm.abs, effects = "fixed")

# 计算置信区间
result_summary <- result_summary %>%
  mutate(
    CI = sprintf("(%0.2f ~ %0.2f)", estimate - 1.96 * std.error, estimate + 1.96 * std.error)
  ) %>%
  select(term, estimate, CI, p.value)

# 打印结果
print(result_summary)



residuals <- resid(res.lm.abs)

# 绘制残差QQ图
qqnorm(residuals)
qqline(residuals)

# 提取残差QQ图中的异常值
# 使用Z-score方法来提取异常值
z_scores <- (residuals - mean(residuals)) / sd(residuals)

# 设置异常值的阈值，例如设置为3（根据实际情况调整）
threshold <- 3

# 提取异常值
outliers_indices <- which(z_scores > threshold | z_scores < -threshold)

original_data.abs <- data.resp.abs # 假设data是原始数据框
outliers_data.abs <- original_data.abs[outliers_indices, ]

# 打印异常值及对应的原始数据
print("Outliers:")
print(outliers_data.abs)

ggplot()+
  geom_jitter(data=data.resp.nona,aes(x=Temperature,y=Resp,group=Pesticide.Control))+
  geom_point(data=outliers_data,aes(x=Temperature,y=Resp),color="blue")



lmerTest::ranova(res.lm)  # Random effect test
lmerTest::anova.lmerModLmerTest(res.lm)
parameters::p_value(res.lm)
plot(res.lm)
plot_model(res.lm, type = "pred", 
           terms = c("Time", "Pesticide.Control", "Temperature"),
           axis.title = c("Time(h)",'Respiratory(mm3.min-1)'),
           legend.title='Treatment',
           show.data =TRUE)

# Summarize mean and sd data
res.sum <- data.resp.nona %>%
  group_by(Time,Temperature,Pesticide.Control) %>%
  summarize(
    Resp.mean = mean(Resp),
    Resp.sd = sd(Resp))

res.sum.s<-data.resp.abs %>%
  group_by(Time,Temperature,Pesticide.Control) %>%
  summarize(
    Resp.mean = mean(Abs.resp),
    Resp.sd = sd(Abs.resp))



data.resp.nona$Temperature<-as.factor(data.resp.nona$Temperature)
data.resp.nona$Time<-as.factor(data.resp.nona$Time)
res.d<-ggplot(data.resp.nona,
              aes(x=Temperature,y=Resp,fill=Pesticide.Control))+
  geom_boxplot(outlier.shape = NA)+
  labs(x = expression('Ambient Temperature('*~degree*C*')'), y = bquote('Respiration rate' ~ (mm^3 * min^-1))) +
  scale_fill_manual(name = "Treatment",
                     values = c("#56B4E9", "#D55E00"),
                     labels = c("Control", "20 ppb Thiamethoxam"))+
  theme(legend.position = "top", 
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1))
print(res.d)



# Make distribution plot
res.sum$Time<-as.factor(res.sum$Time)
res.1<-ggplot(res.sum,
              aes(Time, Resp.mean, group=Pesticide.Control,col=Pesticide.Control)) + 
  facet_wrap(~Temperature,nrow = 1) +
  geom_errorbar(aes(ymin=Resp.mean-Resp.sd,ymax=Resp.mean+Resp.sd), position = position_dodge(1))+
  geom_point(position = position_dodge(1),size = 2)+
  labs(x = "Time (h)", y = bquote('Respiratory' ~ (mm^3 * min^-1))) +
  scale_color_manual(name = "Treatment",
                     values = c("#56B4E9", "#D55E00"),
                     labels = c("Control", "20 ppb Thiamethoxam"))
  

print(res.1)

temp.labs <- c("12°C","16°C","20°C","24°C","28°C","32°C")
names(temp.labs) <- c("12","16","20","24","28","32")

res.1.abs<-ggplot(res.sum.s,
              aes(Time, Resp.mean, group=Pesticide.Control,col=Pesticide.Control)) + 
  facet_wrap(~Temperature,nrow = 1,labeller = labeller(Temperature=temp.labs)) +
  geom_errorbar(aes(ymin=Resp.mean-Resp.sd,ymax=Resp.mean+Resp.sd), position = position_dodge(20))+
  geom_point(position = position_dodge(20),size = 2)+
  labs(x = "Time (h)", y = bquote('Respiratory' ~ (mm^3 * min^-1))) +
  scale_color_manual(name = "Treatment",
                     values = c("#56B4E9", "#D55E00"),
                     labels = c("Control", "20 ppb Thiamethoxam"))+
  scale_x_continuous(breaks = c(24,48,72,96,120))+
  theme(legend.position = "top")

print(res.1.abs)

# Making predict line
data.resp.nona$Time<-as.numeric(data.resp.nona$Time)
data.resp.nona$fit <- predict(res.lm.abs)   #Add model fits to dataframe
res.2 <- ggplot(data.resp.nona,
                aes(Time, Resp, group = Pesticide.Control, col = Pesticide.Control)) + 
  facet_wrap(~Temperature,nrow=1,labeller = labeller(Temperature=temp.labs)) +
  geom_smooth(aes(y = fit), method = 'lm', size = 0.8) +
  labs(x = "Time (h)", y = bquote('Respiration rate' ~ (mm^3 * min^-1))) +
  scale_color_manual(name = "Treatment",
                     values = c("#56B4E9", "#D55E00"),
                     labels = c("Control", "20 ppb Thiamethoxam"))+
  scale_x_continuous(breaks = c(24,48,72,96,120))+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))
print(res.2)


# patch two plots tgt using ggarrange
res.double<-ggarrange(res.1.abs +
            theme(axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.title.x = element_blank(),
                  plot.margin = margin(r = 15, l = 15)  ), 
          res.2 + 
            theme(axis.ticks.x = element_blank(),
                  plot.margin = margin(r = 15) ), 
          nrow=2,ncol=1,labels=c("a","b")
)
res.double <- ggarrange(
  res.1.abs + theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.title.x = element_blank()), 
  res.2 + theme(axis.ticks.x = element_blank(),
                axis.title.y = element_blank()),
  nrow = 2, ncol = 1, labels = c("a", "b")
)

print(res.double)
plot_grid(
  res.d, res.2, labels = "auto", 
  align = "h")

# Plot in same plot
res.plot<-ggplot(data.resp.nona,
                 aes(Time, Resp, group=Pesticide.Control, col=Pesticide.Control)) + 
  facet_wrap(~Temperature) +
  geom_smooth(aes(y=fit),method='lm', size=0.8) +
  geom_pointrange(stat = "summary", fun.data = "mean_se",size=0.3)+
  theme_bw()
print(res.plot)

# Visualize resp data
# Due to analysis, resp use Time as x-axis



### BodyTemperature ###
data.temp.nona<-filter(data.main,!is.na(B.Temp))
which(is.na(data.temp.nona$B.Temp))
btemp<-gam(B.Temp~Pesticide.Control+s(Time,k=3)+s(Temperature,k=3),data=data.temp.nona)
btempk5<-gam(B.Temp~Pesticide.Control+s(Time,k=5)+s(Temperature,k=5),data=data.temp.nona)
summary(btemp)
summary(btempk5)



# Plot B.Temp(gam k=3)
newdat.lme=data.frame(Temperature=data.temp.nona$Temperature,
                      Pesticide.Control=data.temp.nona$Pesticide.Control,
                      Time=data.temp.nona$Time,
                      Colony=data.temp.nona$Colony)
head(newdat.lme)
which(is.na(newdat.lme))
newdat.lme$predlme<-predict(btemp,newdata=data.frame(Temperature=data.temp.nona$Temperature,
                                                     Pesticide.Control=data.temp.nona$Pesticide.Control,
                                                     Time=data.temp.nona$Time))
btemp.plot<-ggplot(data.temp.nona,aes(Temperature, B.Temp, group=Pesticide.Control, col=Pesticide.Control )) + 
  facet_wrap(~Time,ncol=2) +
  geom_point(alpha = 0.8) +
  geom_line(data=newdat.lme,aes(y=predlme))+
  theme_bw()
print(btemp.plot)

# Plot B.Temp(gam k=5)
newdat.lme$predlmek5<-predict(btempk5,newdata=data.frame(Temperature=data.temp.nona$Temperature,
                                                     Pesticide.Control=data.temp.nona$Pesticide.Control,
                                                     Time=data.temp.nona$Time))
btemp.plot<-ggplot(data.temp.nona,aes(Temperature, B.Temp, group=Pesticide.Control, col=Pesticide.Control )) + 
  facet_wrap(~Time,ncol=2) +
  geom_point(alpha = 0.8) +
  geom_line(data=newdat.lme,aes(y=predlmek5))+
  theme_bw()
print(btemp.plot)

# Put Quadratic equation in Linear model and try
btemp.lm<-lm(B.Temp ~ Pesticide.Control*Temperature+I(Temperature^2),data=data.temp.nona)
summary(btemp.lm)
qplot(data.temp.nona$Temperature,data.temp.nona$B.Temp)
xx<-data.temp.nona$Temperature
yy<-predict(btemp.lm,newdata=data.frame(Temperature=xx,Pesticide.Control='0'))
yy2<-predict(btemp.lm,newdata=data.frame(Temperature=xx,Pesticide.Control='1'))
lines(xx,yy)
lines(xx,yy2,col='red')

anova(btemp.lm)





# Quartic in btemp model
btemp.c<-lm(B.Temp ~ Pesticide.Control*I(Temperature^4)*Time+Pesticide.Control*I(Temperature^3)*Time+Pesticide.Control*I(Temperature^2)*Time+Pesticide.Control*Time*Temperature,data=data.temp.nona)
summary(btemp.c)
plot(data.temp.nona$Temperature,data.temp.nona$B.Temp)
xx<-data.temp.nona$Temperature
Time<-data.temp.nona$Time
yy<-predict(btemp.c,newdata=data.frame(Temperature=xx,Time=24,Pesticide.Control='0'))
yy2<-predict(btemp.c,newdata=data.frame(Temperature=xx,Time=24,Pesticide.Control='1'))
lines(xx,yy,col='')
lines(xx,yy2,col='red')

# Fitting in lmer
btemp.lmer<-lmer(B.Temp ~ Pesticide.Control*I(Temperature^3)*Time+Pesticide.Control*I(Temperature^2)*Time+Pesticide.Control*Time*Temperature+(1|Colony)+(1|Name),data=data.temp.nona)
summary(btemp.lmer)
performance::r2(btemp.lmer)

result_summary <- tidy(btemp.lmer, effects = "fixed")

# 计算置信区间
result_summary <- result_summary %>%
  mutate(
    CI = sprintf("(%0.2f ~ %0.2f)", estimate - 1.96 * std.error, estimate + 1.96 * std.error)
  ) %>%
  select(term, estimate, CI, p.value)

# 打印结果
print(result_summary)
# lmm_model 是您拟合的LMM模型
btemp_residuals <- resid(btemp.lmer)

# 绘制残差QQ图
qqnorm(btemp_residuals)
qqline(btemp_residuals)

# 提取残差QQ图中的异常值
# 使用Z-score方法来提取异常值
z_scores <- (btemp_residuals - mean(btemp_residuals)) / sd(btemp_residuals)

# 设置异常值的阈值，例如设置为3（根据实际情况调整）
threshold <- 3

# 提取异常值
outliers_indices <- which(z_scores > threshold | z_scores < -threshold)

original_data.btemp <- data.temp.nona 
outliers_data.btemp <- original_data.btemp[outliers_indices, ]

cleaned_data <- original_data[-outliers_indices, ]

ggplot(cleaned_data,aes(x=Temperature,y=B.Temp,group=Pesticide.Control))


# 打印删除异常值后的数据
btemp.clean <- ggplot(cleaned_data,
                  aes(Temperature, B.Temp, group = Pesticide.Control, col = Pesticide.Control)) + 
  geom_point()+
  geom_smooth()+
  facet_wrap(~Time, nrow = 1)  +
  labs(x = expression('Ambient Temperature('*~degree*C*')'), y = expression('Body Temperature('*~degree*C*')')) +
  scale_color_manual(name = "Treatment",
                     values = c("#56B4E9", "#D55E00"),
                     labels = c("Control", "20 ppb Thiamethoxam"))+
  scale_x_continuous(breaks = c(12,16,20,24,28,32))
print(btemp.clean)

# Plot btemp lmer
plot_model(btemp.lmer, type = "pred", 
           terms = c("Temperature[12,16,20,24,28,32]", "Pesticide.Control", "Time[all]"),
           axis.title = c("Temperature(degree)",'Body Temperature(degree)'),
           legend.title='Treatment')

# Plot btemp
# Summarize mean and sd data
btemp.sum <- data.temp.nona %>%
  group_by(Time,Temperature,Pesticide.Control) %>%
  summarize(
    Btemp.mean = mean(B.Temp),
    Btemp.sd = sd(B.Temp))

time.labs <- c("24 h","48 h","72 h","96 h","120 h")
names(time.labs) <- c("24","48","72","96","120")


# Plot Distribution 
btemp.1<-ggplot(btemp.sum,
              aes(Temperature, Btemp.mean, group=Pesticide.Control,col=Pesticide.Control)) + 
  facet_wrap(~Time,nrow = 1,labeller = labeller(Time=time.labs)) +
  geom_errorbar(aes(ymin=Btemp.mean-Btemp.sd,ymax=Btemp.mean+Btemp.sd), position = position_dodge(4))+
  geom_point(position = position_dodge(4),size = 2)+
  labs(x = expression('Ambient Temperature('*~degree*C*')'), y = expression('Body Temperature('*~degree*C*')')) +
  scale_color_manual(name = "Treatment",
                     values = c("#56B4E9", "#D55E00"),
                     labels = c("Control", "20 ppb Thiamethoxam"))+
  scale_x_continuous(breaks = c(12,16,20,24,28,32))+
  theme(legend.position = "top")
  


print(btemp.1)

# Making predict line
data.temp.nona$fit <- predict(btemp.lmer)   #Add model fits to dataframe
btemp.2 <- ggplot(data.temp.nona,
                  aes(Temperature, B.Temp, group = Pesticide.Control, col = Pesticide.Control)) + 
  facet_wrap(~Time, nrow = 1,labeller = labeller(Time=time.labs)) +
  geom_smooth(aes(y = fit), size = 0.8) +
  labs(x = expression('Ambient Temperature('*~degree*C*')'), y = expression('Body Temperature('*~degree*C*')')) +
  scale_color_manual(name = "Treatment",
                     values = c("#56B4E9", "#D55E00"),
                     labels = c("Control", "20 ppb Thiamethoxam"))+
    scale_x_continuous(breaks = c(12,16,20,24,28,32))+
    theme(legend.position = "bottom")
print(btemp.2)


# patch two plots tgt using ggarrange
btemp.double<-ggarrange(btemp.1 +
                        theme(axis.text.x = element_blank(),
                              axis.ticks.x = element_blank(),
                              axis.title.x = element_blank()
                              ), 
                      btemp.2 + 
                        theme(axis.ticks.x = element_blank(),
                              axis.title.y = element_blank()), 
                      nrow=2,ncol=1,labels = c("a","b")
)



### Food comsumption


# Calculate sum weight 

data.food.mass<-data.main%>%
  filter(B.Temp!= "")
sum_b_mass <- data.food.mass %>%
  group_by(Temperature, Pesticide.Control, Colony, No., Time) %>%
  summarize(sum.mass = sum(B.Mass, na.rm = TRUE))%>%
  arrange(Temperature,Time)
data.food.nona<-data.food%>%
  filter(Time!="0")
data.food.nona$mass<-sum_b_mass$sum.mass
data.food.nona$Food.ave.w<-data.food.nona$Food.sum.env/data.food.nona$mass


# Fitting it with gam
food.gam<-gam(Food.ave.w~Pesticide.Control+s(Time,k=3)+s(Temperature,k=3),data=data.food.nona)
summary(food.gam)

# Food lm model
food.lm<-lm(Food.ave.w ~ Pesticide.Control*I(Temperature^4)*Time+Time*I(Temperature^3)+Time*I(Temperature^2)+Temperature*Time,data=data.food.nona)
summary(food.lm)
plot(data.food.nona$Temperature,data.food.nona$Food.ave.w)
xx<-data.food.nona$Temperature
yy<-predict(food.lm,newdata=data.frame(Temperature=xx,Day=1,Pesticide.Control=0))
yy2<-predict(food.lm,newdata=data.frame(Temperature=xx,Day=1,Pesticide.Control=1))
lines(xx,yy)
lines(xx,yy2,col='red')



food.lmer2<-lmer(Food.ave.w ~ Pesticide.Control*Time*I(Temperature^3)+Pesticide.Control*Time*I(Temperature^2)+Pesticide.Control*Temperature*Time+(1|Name),data=data.food.nona)
summary(food.lmer2)

food.lmer3<-lmer(Food.ave.w ~ Pesticide.Control*Time*I(Temperature^2)+Pesticide.Control*Temperature*Time+(1|Name),data=data.food.nona)
summary(food.lmer3)
performance::r2(food.lmer3)
anova(food.lmer2,food.lmer3)

food_residuals <- resid(food.lmer2)
result_summary <- tidy(food.lmer3, effects = "fixed")

# 计算置信区间
result_summary <- result_summary %>%
  mutate(
    CI = sprintf("(%0.2f ~ %0.2f)", estimate - 1.96 * std.error, estimate + 1.96 * std.error)
  ) %>%
  select(term, estimate, CI, p.value)
# 打印结果
print(result_summary)
# 绘制残差QQ图
qqnorm(food_residuals)
qqline(food_residuals)

# 提取残差QQ图中的异常值
# 使用Z-score方法来提取异常值
z_scores <- (food_residuals - mean(food_residuals)) / sd(food_residuals)

# 设置异常值的阈值
threshold <- 3

# 提取异常值
outliers_indices <- which(z_scores > threshold | z_scores < -threshold)

original_data.food <- data.food.nona 
outliers_data.food <- original_data.food[outliers_indices, ]

cleaned_data <- original_data[-outliers_indices, ]

ggplot(cleaned_data,aes(x=Temperature,y=B.Temp,group=Pesticide.Control))
 



qqnorm(residuals(food.lmer))
qqline(residuals(food.lmer))
plot_model(food.lmer3, type = "pred", 
           terms = c("Temperature[12,16,20,24,28,32]", "Pesticide.Control", "Time[all]"),
           axis.title = c("Temperature(degree)",'Food Consumption(g)'),
           legend.title='Treatment')
performance::r2(food.lmer2)
anova(food.lmer,food.lmer2,food.lmer3)
# Plot food
# Summarize mean and sd data
food.sum <- data.food.nona %>%
  group_by(Time,Temperature,Pesticide.Control) %>%
  summarize(
    food.mean = mean(Food.ave.w),
    food.sd = sd(Food.ave.w))


# Plot Distribution 
food.1<-ggplot(food.sum,
                aes(Temperature, food.mean, group=Pesticide.Control,col=Pesticide.Control)) + 
  facet_wrap(~Time,nrow = 1,labeller = labeller(Time=time.labs)) +
  geom_errorbar(aes(ymin=food.mean-food.sd,ymax=food.mean+food.sd), position = position_dodge(4))+
  geom_point(position = position_dodge(4),size = 2)+
  labs(x = expression('Ambient Temperature('*~degree*C*')'), y = expression('Food consumption(g/g)')) +
  scale_color_manual(name = "Treatment",
                     values = c("#56B4E9", "#D55E00"),
                     labels = c("Control", "20 ppb Thiamethoxam"))+
  scale_x_continuous(breaks = c(12,16,20,24,28,32))+
  theme(legend.position = "top")


print(food.1)

# Making predict line
data.food.nona$fit <- predict(food.lmer3)   #Add model fits to dataframe
food.2 <- ggplot(data.food.nona,
                  aes(Temperature, Food.ave.w, group = Pesticide.Control, col = Pesticide.Control)) + 
  facet_wrap(~Time, nrow = 1,labeller = labeller(Time=time.labs)) +
  geom_smooth(aes(y = fit), size = 0.8) +
  labs(x = expression('Ambient Temperature('*~degree*C*')'), y = expression('Food consumption(g/g)')) +
  scale_color_manual(name = "Treatment",
                     values = c("#56B4E9", "#D55E00"),
                     labels = c("Control", "20 ppb Thiamethoxam"))+
  scale_x_continuous(breaks = c(12,16,20,24,28,32))+
  theme(legend.position = "bottom")
print(food.2)
 

# patch two plots tgt using ggarrange
food.double<-ggarrange(food.1 +
                          theme(axis.text.x = element_blank(),
                                axis.ticks.x = element_blank(),
                                axis.title.x = element_blank()  ), 
                        food.2 + 
                          theme(axis.ticks.x = element_blank(),
                                axis.title.y = element_blank() ), 
                        nrow=2,ncol=1,labels = c("a","b")
)


btemp.food<-ggarrange(btemp.1 +
                          theme(axis.text.x = element_blank(),
                                axis.ticks.x = element_blank(),
                                axis.title.x = element_blank(),
                                plot.margin = margin(r = 15, l = 15)  ), 
                        btemp.2 + 
                          theme(axis.ticks.x = element_blank(),
                                plot.margin = margin(r = 20) ), 
                      food.1 +
                        theme(axis.text.x = element_blank(),
                              axis.ticks.x = element_blank(),
                              axis.title.x = element_blank(),
                              plot.margin = margin(r = 15, l = 15)  ), 
                      food.2 + 
                        theme(axis.ticks.x = element_blank(),
                              plot.margin = margin(r = 20) ), 
                        nrow=4,ncol=1,labels=c("a","b","c","d")
)

######################################################
walk.lm<-lmer(Walk.v ~ Pesticide.Control*Temperature*Time+(1|Name),data=data.walk)
summary(walk.lm)
performance::r2(walk.lm)

data.walk$fit <- predict(walk.lm)   #Add model fits to dataframe
ggplot(data.walk,
                 aes(Temperature, Walk.v, group = Pesticide.Control, col = Pesticide.Control)) + 
  #facet_wrap(~Time, nrow = 1) +
  geom_smooth(aes(y = fit), size = 0.8) +
  labs(x = expression('Ambient Temperature('*~degree*C*')'), y = expression('Food consumption(g)')) 
  scale_color_manual(name = "Treatment",
                     values = c("#56B4E9", "#D55E00"),
                     labels = c("Control", "20 ppb Thiamethoxam"))
  scale_x_continuous(breaks = c(12,16,20,24,28,32))
print(food.2)
##########################################################################################
data.walk.1<-filter(data.walk.1,!is.na(Movement))
data.walk<-filter(data.walk,!is.na(Active))
data.walk<-filter(data.walk,!is.na(Inactive))
data.walk <- data.walk %>%
  filter(!is.na(Walk.1) & trimws(Walk.1) != "") 
data.walk.1 <- data.walk.1 %>%
  filter(!is.na(Movement) & trimws(Movement) != "") 

walk.sum<-data.walk.1 %>%
  group_by(Temperature,Pesticide.Control,Time,Movement) %>%
  summarise(count = n())%>%
  group_by(Temperature, Pesticide.Control, Time) %>%
  mutate(total_count = sum(count),
         percentage = count / total_count * 100) %>%
  filter(!is.na(Movement))





walk.p<-ggplot(walk.sum, aes(x = Temperature, y = percentage, fill = Movement)) +
  geom_col(position = "fill",alpha=0.8) +
  facet_wrap(~ Pesticide.Control+Time,nrow=2,ncol=5,labeller = labeller(Pesticide.Control = c("0" = "Control", "1" = "20 ppb Thiamethoxam"))) +
  labs(x = expression('Ambient Temperature('*~degree*C*')'), y = "Precentage", fill = "likelihood of movement") +
  scale_y_continuous(labels = scales::percent_format(scale = 100))+
  scale_x_continuous(breaks = c(12,16,20,24,28,32))+
  scale_fill_manual(values = c("#999999", "#E69F00"),
                    labels = c("Inactive", "Active"))+
  theme(legend.position = "top")

print(walk.p)

walk.p<-ggplot(walk.sum, aes(x = Time, y = percentage, fill = Movement)) +
  geom_col(position = "fill",alpha=0.8) +
  facet_wrap(~ Pesticide.Control, nrow = 1,labeller = labeller(Pesticide.Control = c("0" = "Control", "1" = "20 ppb Thiamethoxam"))) +
  labs(x = expression('Time(h)'), y = "Precentage", fill = "likelihood of movement") +
  scale_y_continuous(labels = scales::percent_format(scale = 100))+
  scale_x_continuous(breaks = c(24,48,72,96,120))+
  scale_fill_manual(values = c("#56B4E9", "#D55E00"))
print(walk.p)
##################################################
# Model selected
glmm_model.1 <- glmer(Movement~ Temperature*Pesticide.Control*Time+ (1|Round)+(1|Name)+(1|Colony),
                    family = binomial(link="logit"),  
                    data = data.walk.1)
summary(glmm_model.1)
performance::r2(glmm_model.1)
glmm_model.2 <- glmer(Movement~ Temperature+Pesticide.Control+Time+ (1|Name),
                      family = binomial,  
                      data = data.walk.1)
summary(glmm_model.2)
performance::r2(glmm_model.2)

glmm_model.3 <- glmer(Movement~ Temperature*Time+Pesticide.Control*Time+Pesticide.Control*Temperature+ (1|Name)+(1|Colony),
                      family = binomial(link="logit"),  
                      data = data.walk.1)
summary(glmm_model.3)


performance::r2(glmm_model.3)

glmm_model.4 <- glmer(Movement~ I(Temperature^2)*Pesticide.Control*Time+Temperature*Pesticide.Control*Time+(1|Name)+(1|Colony),
                      family = binomial(link="logit"), 
                      data = data.walk.1)
summary(glmm_model.4)
performance::r2(glmm_model.4)

result_summary <- tidy(glmm_model.4, effects = "fixed")

# 计算置信区间
result_summary <- result_summary %>%
  mutate(
    CI = sprintf("(%0.2f ~ %0.2f)", estimate - 1.96 * std.error, estimate + 1.96 * std.error)
  ) %>%
  select(term, estimate, CI, p.value)
# 打印结果
print(result_summary)

# Check overdispersiom

residuals <- residuals(glmm_model.4, type = "pearson")

# Estimate dispersion parameter
dispersion <- sum(residuals^2) / (length(residuals) - length(fixef(glmm_model.4)))

# Output the dispersion parameter
print(dispersion)
anova(glmm_model.1,glmm_model.4)

which(is.infinite(data.walk.1$Movement))

# 预测movement的概率
data.walk.1$predicted_prob <- predict(glmm_model.4, newdata = data.walk.1, type = "response")

# 使用ggplot绘制图形
data.walk.1$Movement<-as.numeric(data.walk.1$Movement)
data.walk.1$Movement<- as.numeric(data.walk.1$Movement) - 1
ggplot()+
  geom_jitter(aes(y=Movement,x=Temperature,color = Pesticide.Control,),data=data.walk.1,width = 3, alpha=0.3,shape=1,height = 0.05) +
  geom_smooth(aes(y = predicted_prob,x=Temperature,color = Pesticide.Control,linetype= Pesticide.Control),data=data.walk.1,size = 0.8) +
  facet_wrap(~Time, nrow = 1,labeller = labeller(Time=time.labs)) +
  labs(x = expression('Ambient Temperature('*~degree*C*')'), y = expression('Likelihood of movement')) +
  scale_color_manual(name = "Treatment",
                     values = c("#56B4E9", "#D55E00"),
                     labels = c("Control", "20 ppb Thiamethoxam"))+
  scale_linetype_manual(name = "Treatment",
                        values = c("solid", "twodash"),
                        labels = c("Control", "20 ppb Thiamethoxam"))+
  scale_x_continuous(breaks = c(12,16,20,24,28,32))+
  theme(legend.position = "top")

ggplot()+
  geom_jitter(aes(y=Active,x=Time,color = Pesticide.Control,),data=data.walk,width = 5, alpha=0.3,shape=1) +
  geom_smooth(aes(y = predicted_prob,x=Time,color = Pesticide.Control,linetype= Pesticide.Control),data=data.walk.1,size = 0.8) +
  facet_wrap(~Temperature, nrow = 1) +
  labs(x = expression('Time(h)'), y = expression('Likelihood to move')) +
  scale_color_manual(name = "Treatment",
                     values = c("#56B4E9", "#D55E00"),
                     labels = c("Control", "20 ppb Thiamethoxam"))+
  scale_linetype_manual(name = "Treatment",
                        values = c("solid", "twodash"),
                        labels = c("Control", "20 ppb Thiamethoxam"))+
  scale_x_continuous(breaks = c(24,48,72,96,120))+
  theme(legend.position = "top")


walk.glm<-glm(Movement ~ Pesticide.Control*I(Temperature^4)*Time+ Pesticide.Control*Time*I(Temperature^3)+ Pesticide.Control*Time*I(Temperature^2)+ Pesticide.Control*Temperature*Time,family = binomial,data=data.walk.1)
summary(walk.glm)
performance::r2(walk.glm)
plot(data.walk.1$Temperature,data.walk.1$Movement)
xx<-data.walk.1$Temperature
yy<-predict(walk.glm,newdata=data.frame(Temperature=12,Day=1,Pesticide.Control=0))
yy2<-predict(walk.glm,newdata=data.frame(Temperature=12,Day=1,Pesticide.Control=1))
lines(xx,yy)
lines(xx,yy2,col='red')


#########################################################################
#############################Relationship################################
food_result <- data.walk %>%
  group_by(Temperature,Pesticide.Control,Colony,No.,Time) %>%
  summarize(Resp_sum = sum(Resp)/n(),btemp_sum = sum(B.Temp)/n(),walkv_sum = sum(Walk.v)/n(),walk_p=sum(Active)/n())%>%
  arrange(Temperature,Time)
food_result$Food.ave.w<-data.food.nona$Food.ave.w

# 计算每个组的均值
mean_data <- data.walk %>%
  filter(!is.na(B.Temp) & trimws(B.Temp) != "")%>%
  filter(!is.na(Resp) & trimws(Resp) != "")%>%
  group_by(Pesticide.Control, Temperature) %>%
  summarise(mean_y = mean(Resp),mean_x=mean(B.Temp))

mean_data_food <- food_result %>%
  filter(!is.na(Food.ave.w) & trimws(Food.ave.w) != "")%>%
  filter(!is.na(walk_p) & trimws(walk_p) != "")%>%
  group_by(Pesticide.Control, Time) %>%
  summarise(mean_y = mean(Food.ave.w),mean_x=mean(walk_p))

# 绘制图形，手动添加均值点
library(ggpubr)
plot1<-ggplot(data.walk, aes(x =Resp, y =Active, color = Pesticide.Control, group = Pesticide.Control)) +
  geom_jitter(height = 0.1,alpha=0.8) +
  geom_smooth(method = "lm") +
  labs(x = bquote('Respiratory'~(mm^3*min^-1)), y = expression('Likelihood of movement')) +
  scale_color_manual(name = "Treatment",
                     values = c("#56B4E9", "#D55E00"),
                     labels = c("Control", "20 ppb Thiamethoxam"))+
  scale_linetype_manual(name = "Treatment",
                        values = c("solid", "twodash"),
                        labels = c("Control", "20 ppb Thiamethoxam"))
  theme(legend.position = "bottom")
print(plot1)

plot2<-ggplot(data.walk, aes(x =B.Temp, y =Active, color = Pesticide.Control, group = Pesticide.Control)) +
  geom_jitter(height = 0.1,alpha=0.8) +
  geom_smooth(method = "lm") +
  labs(x = expression('Body Temperature('*~degree*C*')'), y = expression('Likelihood of movement')) +
  scale_color_manual(name = "Treatment",
                     values = c("#56B4E9", "#D55E00"),
                     labels = c("Control", "20 ppb Thiamethoxam"))+
  scale_linetype_manual(name = "Treatment",
                        values = c("solid", "twodash"),
                        labels = c("Control", "20 ppb Thiamethoxam"))
  theme(legend.position = "bottom")
print(plot2)

  geom_point(data = mean_data, aes(x = mean_x, y = mean_y), 
             size = 5, shape = 18, show.legend = FALSE,color ="blue") +
  facet_wrap(~Temperature)

plot3<-ggplot(food_result, aes(x =Food.ave.w, y = walk_p, color = Pesticide.Control, group = Pesticide.Control)) +
  geom_jitter(height = 0.05,alpha=0.8) +
  geom_smooth(method = "lm")+
  labs(x = expression('Food Consumption(g/g)'), y = expression('Likelihood of movement')) +
  scale_color_manual(name = "Treatment",
                     values = c("#56B4E9", "#D55E00"),
                     labels = c("Control", "20 ppb Thiamethoxam"))+
  scale_linetype_manual(name = "Treatment",
                        values = c("solid", "twodash"),
                        labels = c("Control", "20 ppb Thiamethoxam"))
  theme(legend.position = "bottom")
print(plot3)

plot4<-ggplot(food_result, aes(x =Resp_sum, y = Food.ave.w, color = Pesticide.Control, group = Pesticide.Control)) +
  geom_jitter(height = 0.05,alpha=0.8) +
  geom_smooth(method = "lm")+
  labs(x = bquote('Respiratory'~(mm^3*min^-1)), y = expression('Food Consumption(g/g)')) +
  scale_color_manual(name = "Treatment",
                     values = c("#56B4E9", "#D55E00"),
                     labels = c("Control", "20 ppb Thiamethoxam"))+
  scale_linetype_manual(name = "Treatment",
                        values = c("solid", "twodash"),
                        labels = c("Control", "20 ppb Thiamethoxam"))
print(plot4)

plot5<-ggplot(food_result, aes(x =btemp_sum, y = Food.ave.w, color = Pesticide.Control, group = Pesticide.Control)) +
  geom_jitter(height = 0.05,alpha=0.8) +
  geom_smooth(method = "lm")+
  labs(x = expression('Body Temperature('*~degree*C*')'), y = expression('Food Consumption(g/g)')) +
  scale_color_manual(name = "Treatment",
                     values = c("#56B4E9", "#D55E00"),
                     labels = c("Control", "20 ppb Thiamethoxam"))+
  scale_linetype_manual(name = "Treatment",
                        values = c("solid", "twodash"),
                        labels = c("Control", "20 ppb Thiamethoxam"))
print(plot5)

plot6<-ggplot(data.walk, aes(x =Resp, y =B.Temp, color = Pesticide.Control, group = Pesticide.Control)) +
  geom_jitter(height = 0.1,alpha=0.8) +
  geom_smooth(method = "lm") +
  labs(x = bquote('Respiratory'~(mm^3*min^-1)), y = expression('Body Temperature('*~degree*C*')')) +
  scale_color_manual(name = "Treatment",
                     values = c("#56B4E9", "#D55E00"),
                     labels = c("Control", "20 ppb Thiamethoxam"))+
  scale_linetype_manual(name = "Treatment",
                        values = c("solid", "twodash"),
                        labels = c("Control", "20 ppb Thiamethoxam"))
print(plot6)


all<-ggarrange(plot1 +
                 theme(legend.position = "none"),
               plot2+
                 theme(legend.position = "none"),
               plot3+
                 theme(legend.position = "none"),
               plot4+
                 theme(legend.position = "none"),
               plot5+
                 theme(legend.position = "none"),
               plot6,
               nrow=2,ncol=3,labels=c("a","b","c","d","e","f")
)

print(all)

combined_plot <- (plot1 + plot2 + plot3 + plot4 + plot5 + plot6) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") +
  theme(legend.position = "center")
# 显示合并后的图
print(combined_plot)

plot_grid(
  plot1, plot2, labels = "auto", 
  rel_widths = c(0.3, 0.6), 
  align = "h")
########################################################################
# Visualize data by day and temp

Resp<-ggplot(data.main,aes(x=Time, y=Resp,colour=factor(Temperature)),na.rm=TRUE)+
  geom_point()+
  geom_smooth()+
  labs(x = "Time (h)",
       y = bquote('Respiratory'~(mm^3*min^-1)))+
  facet_wrap(~Pesticide.Control,ncol=2)
print(Resp)

Resp_c<-ggplot(data.main,aes(x=Colony, y=Resp,colour=factor(Pesticide.Control)),na.rm=TRUE)+
  geom_point()+
  geom_smooth()+
  labs(x = "Time (h)",
       y = bquote('Respiratory'~(mm^3*min^-1)))+
  facet_wrap(~Temperature,ncol=2)
print(Resp_c)

B_temp<-ggplot(data.main,aes(x=Time, y=B.Temp,colour=factor(Temperature)),na.rm=TRUE)+
  geom_point()+
  geom_smooth()+
  labs(x = "Time (h)",
       y = expression('Body Temperature('*~degree*C*')'))+
  facet_wrap(~Pesticide.Control,ncol=2)
print(B_temp)


Food<-ggplot(data.food, aes(x=Time, y=Food.ave,colour=factor(Temperature)),na.rm=TRUE)+
  geom_point()+
  geom_smooth()+
  labs(x = "Time (h)",
       y = expression('Food consumption(g)'))+
  facet_wrap(~Pesticide.Control,ncol=2)
print(Food)

Food.w<-ggplot(data.food, aes(x=Day, y=Food.ave.w,colour=factor(Temperature)),na.rm=TRUE)+
  geom_point()+
  geom_smooth()+
  labs(x = "Time (h)",
       y = expression('Food consumption(g)'))+
  facet_wrap(~Pesticide.Control,ncol=2)
print(Food.w)

#Visualize by splited temp
Resp_temp<-ggplot(data.main,aes(x=Time, y=Resp,colour=factor(Pesticide.Control)),na.rm=TRUE)+
  geom_point()+
  geom_smooth()+
  labs(x = "Time (h)",
       y = bquote('Respiratory'~(mm^3*min^-1)))+
  facet_wrap(~Temperature,ncol=2)
print(Resp_temp)

B_temp_1<-ggplot(data.main,aes(x=Time, y=B.Temp,colour=factor(Pesticide.Control)),na.rm=TRUE)+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Time (h)",
       y = expression('Body Temperature('*~degree*C*')'))+
  facet_wrap(~Temperature,ncol=2)
print(B_temp_1)

Walk_temp<-ggplot(data.walk,aes(x=Time, y=Walk.v,colour=factor(Pesticide.Control)),na.rm=TRUE)+
  geom_jitter(alpha = 0.8, width = 1, height = 0)+
  geom_smooth(method = "lm")+
  labs(x = "Time (h)",
       y = expression('Likelihood of movement'))+
  facet_wrap(~Temperature,ncol=2)
print(Walk_temp)

Walk_temp1<-ggplot(data.walk,aes(x=Temperature, y=Walk.v,colour=factor(Pesticide.Control)),na.rm=TRUE)+
  geom_jitter(alpha=0.5,width=5,height = 0)+
  geom_smooth(method = "lm")+
  labs(x = expression('Temperature'),
       y = expression('Likelihood of movement'))
  facet_wrap(~Time,ncol=2)
print(Walk_temp1)

data.walk
Walk_temp2<-ggplot(data.walk,aes(x=Temperature, y=Active,colour=factor(Pesticide.Control)),na.rm=TRUE)+
  geom_boxplot()+
  labs(x = expression('Temperature'),
       y = expression('Likelihood of movement'))+
  facet_wrap(~Time,ncol=2)
print(Walk_temp2)


Food_temp<-ggplot(data.food, aes(x=Day, y=Food.ave,colour=factor(Pesticide.Control)),na.rm=TRUE)+
  geom_point()+
  geom_smooth()+
  labs(x = "Time (h)",
       y = expression('Food consumption(g)'))+
  facet_wrap(~Temperature,ncol=2)
print(Food_temp)

Food_temp.w<-ggplot(data.food, aes(x=Day, y=Food.ave.w,colour=factor(Pesticide.Control)),na.rm=TRUE)+
  geom_point()+
  geom_smooth()+
  labs(x = "Time (h)",
       y = expression('Food consumption(g)'))+
  facet_wrap(~Temperature,ncol=2)
print(Food_temp.w)

# Visualize by day
Resp_day<-ggplot(data.main,aes(x=Temperature, y=Resp,colour=factor(Pesticide.Control)),na.rm=TRUE)+
  geom_point()+
  geom_smooth()+
  labs(x = expression('Temperature('*~degree*C*')'),
       y =  bquote('Respiratory'~(mm^3*min^-1)))+
  facet_wrap(~Time,ncol=2)
print(Resp_day)

btemp_day<-ggplot(data.main,aes(x=Temperature, y=B.Temp,colour=factor(Pesticide.Control)),na.rm=TRUE)+
  geom_point()+
  geom_smooth()+
  labs(x = expression('Temperature('*~degree*C*')'),
       y = expression('Body Temperature('*~degree*C*')'))+
  facet_wrap(~Time,ncol=2)
print(btemp_day)

food_day<-ggplot(data.food,aes(x=Temperature, y=Food.ave,colour=factor(Pesticide.Control)),na.rm=TRUE)+
  geom_point()+
  geom_smooth()+
  labs(x = expression('Temperature('*~degree*C*')'),
       y = expression('Food consumption(g)'))+
  facet_wrap(~Day,ncol=2)
print(food_day)

walk_day<-ggplot(data.main,aes(x=Temperature, y=Walk.v,colour=factor(Pesticide.Control)),na.rm=TRUE)+
  geom_point()+
  geom_smooth()+
  labs(x = expression('Temperature('*~degree*C*')'),
       y = expression('Walking velocity(cm/s)'))+
  facet_wrap(~Time,ncol=2)
print(walk_day)

hist(data.main$Walk.v)

# Look into data structure
hist(data.main$B.Mass)
hist(data.mortality$Dead.day,na.rm=TRUE)


# Split dataframe by temperature
library(dplyr)

split_data_by_column <- function(data.main, Temperature) {
  split_data <- data %>% group_split({{Temperature}})
  names(split_data) <- unique(data.main[[Temperature]])
  return(split_data)
}



