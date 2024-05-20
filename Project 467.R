tax_data <- read.csv('/Users/giggabite/Downloads/To export - data project 467.xlsx - Sheet1-2.csv')
library(car)
library(dplyr)
library(bestglm)
par(mfrow = c(2,2))
df1 <- data.frame(tax = tax_data$gross_tax_cap, gdp = tax_data$gdp_cap, 
                 pop = tax_data$population, forms = tax_data$forms_cap, 
                 unem = tax_data$unemploy_cap, ins = tax_data$insured_cap, 
                 govWrkrs = tax_data$gov_workers_cap, state = tax_data$state,
                 L_reg = tax_data$large_region, s_reg =tax_data$small_region)
list2env(df1, envir = .GlobalEnv) #small project, shouldn't cause problems. If up-scaled, would need to remove



#Quantile only (preliminary) diagnostics
summary(tax_data)
df_q<- df1 %>% select(-(state:s_reg))
linmod_q <- lm(tax ~ gdp + pop + forms + unem + ins + govWrkrs, data = df_q)
var(df_q)
cor(df_q[-1])
vif(linmod_q)
summary(linmod_q)
plot(linmod_q)
par(mfrow = c(2,3))
plot(tax ~ forms , data = tax_data, main ="Gross State & Local Tax \nvs Amount of Tax Forms Filled ", xlab ='Tax Forms (per capita)', ylab = 'Tax Collected (per capita)' )
plot(tax ~ unem , data = tax_data, main ="Gross State & Local Tax \nvs Unemployed People", xlab ='People (per capita)', ylab = 'Tax Collected (per capita)')
plot(tax ~ ins , data = tax_data,  main ="Gross State & Local Tax  \nvs Insured People", xlab ='People (per capita)', ylab = 'Tax Collected (per capita)')
plot(tax ~  govWrkrs , data = tax_data, main ="Gross State & Local \nTax vs Government Workers ", xlab ='Workers (per capita)', ylab = 'Tax Collected (per capita)')
plot(tax ~ gdp , data = tax_data, main ="Gross State & Local Tax \nvs GDP ", xlab ='GDP (per capita)', ylab = 'Tax Collected (per capita)')
boxplot(tax ~ L_reg , tax_data = tax_data, main ="Gross State & Local Tax \nvs Regions in US ", xlab ='People (per capita)', ylab = 'Tax Collected (per capita)')
par(mfrow = c(2,2))
require(ggplot2)
ggplot(tax_data,aes(x=forms,y=tax))+stat_summary(fun.y="mean",geom="line",aes(group=L_reg,linetype=L_reg))+theme(legend.position = "top", legend.direction = "horizontal")
ggplot(tax_data,aes(x=unem,y=tax))+stat_summary(fun.y="mean",geom="line",aes(group=L_reg,linetype=L_reg))+theme(legend.position = "top", legend.direction = "horizontal")
ggplot(tax_data,aes(x=ins,y=tax))+stat_summary(fun.y="mean",geom="line",aes(group=L_reg,linetype=L_reg))+theme(legend.position = "top", legend.direction = "horizontal")
ggplot(tax_data,aes(x=govWrkrs,y=tax))+stat_summary(fun.y="mean",geom="line",aes(group=L_reg,linetype=L_reg))+theme(legend.position = "top", legend.direction = "horizontal")
ggplot(tax_data,aes(x=gdp,y=tax))+stat_summary(fun.y="mean",geom="line",aes(group=L_reg,linetype=L_reg))+theme(legend.position = "top", legend.direction = "horizontal")

par(mfrow = c(2,2))


# outlier
head(sort(gdp,decreasing = TRUE))
which(gdp >= .2) %>% state[.]
# fits with trend of data => kept in. then again:
df2 <- subset(df_q, state != "District_of_Columbia")
linmod_q_reduced <- lm(df2$tax ~ df2$forms + df2$unem + df2$ins + df2$govWrkrs, data = df2)
summary(linmod_q) #r^2 = .8567
summary(linmod_q_reduced)# r^2 = .6578
df3_q <- subset(df_q, state != "District_of_Columbia")
#ancova modeling
ancova_model_L <- lm(tax ~ forms + unem + ins + govWrkrs + gdp + L_reg, data = tax_data)     #treatment coding
ancova_model_L_d <- lm(tax ~ forms + unem + ins + govWrkrs + gdp + L_reg-1, data = tax_data) #dummy coding
Anova(ancova_model_L, type = 'III')
ancova_model_s <- lm(tax ~ forms + unem + ins + govWrkrs + gdp + s_reg, data = tax_data,)
Anova(ancova_model_s, type = 'III')
lmod_e<-lm(tax ~ forms + unem + ins + govWrkrs + gdp + L_reg, data = tax_data,
           contrasts=list(L_reg="contr.sum")) 
plot(ancova_model_L)
plot(ancova_model_L_d)

plot(ancova_model_s)


ancova_model_Ls <- lm(tax ~ forms + unem + ins + govWrkrs + gdp + L_reg + s_reg, data = tax_data)
Anova(ancova_model_L, type = 'III')
summary(ancova_model_Ls)

#model comparisons
summary(ancova_model_L)
summary(ancova_model_s) #categorical is significant
summary(lmod_e) #effect coding = no significant improvement
anova(ancova_model_L, ancova_model_s)
tci_L <- TukeyHSD(aov(tax ~ L_reg))
tci_L
tci_s <- TukeyHSD(aov(tax ~ s_reg))
tci_s
plot(tci_L)
plot(tci_s) #while almost all cross 0, all ranges are to big to use (lowwww p-val)
plot(ancova_model_L)

par(mfrow=c(1,1))

t.test(gdp ~ L_reg[2:3], df2)

df_IC <- df1[,c(2:7,1)]
#df_IC[7] <- factor(df1[,7])
names(df_IC)[7] <- 'y'
y_AIC <- bestglm(df_IC, IC='AIC')
y_BIC <- bestglm(df_IC, IC='BIC')
y_AIC$Subsets
y_BIC$Subsets

df_redIC <- df1[,c("unem","ins","gdp",'tax')]
names(df_redIC)[4] <- 'y'
y_redAIC <- bestglm(df_redIC, IC='AIC')
y_redBIC <- bestglm(df_redIC, IC='BIC')
y_redAIC$Subsets
y_redBIC$Subsets



