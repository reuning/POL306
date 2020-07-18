df <- read.csv("https://www.kevinreuning.com/assets/state_cig.csv")


mod <- lm(cig_tax~liberal, data=df)
summary(mod)


mod <- lm(cig_tax~female_leg, data=df)
summary(mod)
nobs(mod)

mod <- lm(healthspendpc~female_leg, data=df)
summary(mod)
nobs(mod)

confint(mod, level=0.99)

pdf("/Users/kevinreuning/Dropbox/Teaching_Kevin/Fall_2019/POL_306/Lectures/images/resid.pdf",
    height=8, width=8)
par(mfrow=c(2,2))
plot(rnorm(1000), ylab="Residual", main="Good!")

plot(rnorm(1000, sd=seq(.5,3, length.out = 1000)), ylab="Residual", main="Heteroskedasticity")

set.seed(1)
tmp <- rnorm(1000)
tmp2 <- tmp + .25*cumsum(tmp)
mean(tmp2)
plot(tmp2, ylab="Residual",main="Autocorrelation")
dev.off()



pdf("/Users/kevinreuning/Dropbox/Teaching_Kevin/Fall_2019/POL_306/Lectures/images/resid2.pdf",
    height=6, width=8)
par(mfrow=c(1,2))
x <- rnorm(1000)
y <- 2*x + rnorm(1000)
mod <- lm(y~x)

plot(y=mod$residuals, x=mod$fitted.values, xlab='Predicts', ylab='Residuals', main='Good')

x <- rnorm(1000)
z <- -x + rnorm(1000, 2)
y <- 2*x + rnorm(1000,sd=exp(x))
mod <- lm(y~x)
plot(y=mod$residuals, x=mod$fitted.values, xlab='Predicts', ylab='Residuals', main='Heteroskedasticity')
dev.off()



mod <- lm(cig_tax~female_leg, data=df)

pdf("/Users/kevinreuning/Dropbox/Teaching_Kevin/Fall_2019/POL_306/Lectures/images/resid3.pdf",
    height=6, width=8)
par(mfrow=c(1,2))
plot(mod$residuals, ylab='Residuals')
plot(y=mod$residuals, x=mod$fitted.values, xlab='Predicts', ylab='Residuals')
dev.off()


pdf("/Users/kevinreuning/Dropbox/Teaching_Kevin/Fall_2019/POL_306/Lectures/images/null_dist.pdf",
    height=6, width=8)
plot(y=dnorm(seq(-.05,.05,by=.001),
             sd=sqrt(diag(vcov(mod)))[2] ),
     x=seq(-.05,.05, by=.001), type='l', ylab='Density',
     xlab='Coefficient Value',
     main='Null Distribution')
abline(v=coef(mod)[2], lty=2)
text(x=coef(mod)[2], y=50, pos=3, labels="Estimated Value",
     srt=90)
dev.off()

pdf("/Users/kevinreuning/Dropbox/Teaching_Kevin/Fall_2019/POL_306/Lectures/images/crit_vals.pdf",
    height=6, width=8)
plot(y=dnorm(seq(-3,3,by=.1)),
     x=seq(-3,3, by=.1), type='l',
     ylab='Density',
     xlab="Value",
     main="t-distribution")


plot(y=dnorm(seq(-3,3,by=.1)),
     x=seq(-3,3, by=.1), type='l',
     ylab='Density',
     xlab="Value",
     main="t-distribution")
polygon(x=c(1.644854,seq(1.644854, 3, by=.1)),
        y=c(0.004992899,dnorm(seq(1.644854, 3, by=.1))),
        col='gray')
abline(v=1.644854)
arrows(y1=dnorm(2), x1=2,
       y0=0.15, x0=2.25
       )
text(x=2.25,  y=.15,
     label='0.05 Area', pos=3)
text(x=1.644854,  pos=3, y=.2,
     label='1.644854 Crit Value', xpd=T,
     srt=270)



plot(y=dnorm(seq(-3,3,by=.1)),
     x=seq(-3,3, by=.1), type='l',
     ylab='Density',
     xlab="Value",
     main="t-distribution")
polygon(x=c(seq(-3, -2.326348, length.out = 20), -2.326348),
        y=c(dnorm(seq(-3, -2.326348, length.out = 20)), 0.004992899),
        col='gray')
abline(v=-2.326348)
arrows(y1=dnorm(-2.5), x1=-2.5,
       y0=0.15, x0=-2.75
)
text(x=-2.75,  y=.15,
     label='0.01 Area', pos=3)
text(x=-2.326348,  pos=3, y=.2,
     label='-2.326348 Crit Value', xpd=T,
     srt=270)




plot(y=dnorm(seq(-3,3,by=.1)),
     x=seq(-3,3, by=.1), type='l',
     ylab='Density',
     xlab="Value",
     main="t-distribution")
polygon(x=c(seq(-3, -1.959964, length.out = 20), -1.959964),
        y=c(dnorm(seq(-3, -1.959964, length.out = 20)), 0.004992899),
        col='gray')
abline(v=-1.959964)
arrows(y1=dnorm(-2.25), x1=-2.25,
       y0=0.15, x0=-2.75
)
text(x=-2.75,  y=.15,
     label='0.025 Area', pos=3)
text(x=-1.959964,  pos=3, y=.2,
     label='-1.959964 Crit Value', xpd=T,
     srt=270)


polygon(x=c(1.959964,seq(1.959964, 3, by=.1)),
        y=c(0.004992899,dnorm(seq(1.959964, 3, by=.1))),
        col='gray')
abline(v=1.959964)
arrows(y1=dnorm(2.25), x1=2.25,
       y0=0.15, x0=2.75
)
text(x=2.65,  y=.15,
     label='0.025 Area', pos=3)
text(x=1.959964,  pos=3, y=.2,
     label='1.959964 Crit Value', xpd=T,
     srt=270)
dev.off()

pnorm(1.68)
qnorm(0.025)

pdf("/Users/kevinreuning/Dropbox/Teaching_Kevin/Fall_2019/POL_306/Lectures/images/null_dist2.pdf",
    height=6, width=8)
plot(y=dnorm(seq(-.05,.05,by=.001),
             sd=sqrt(diag(vcov(mod)))[2] ),
     x=seq(-.05,.05, by=.001), type='l', ylab='Density',
     xlab='Coefficient Value',
     main='Null Distribution')
abline(v=0.005, lty=2)
text(x=0.005, y=50, pos=3, labels="Estimated Value",
     srt=90)
dev.off()


df[df$state=="Ohio",]
df[df$state=="Indiana",]

pdf("/Users/kevinreuning/Dropbox/Teaching_Kevin/Fall_2019/POL_306/Lectures/images/corr_biv.pdf",
    height=8, width=8)
plot(df$liberal, df$female_leg, xlab="% Liberal Voters", ylab="% Women Leg")
dev.off()

cor(df$liberal, df$female_leg, use="complete.obs")

mod <- lm(cig_tax~female_leg + liberal, data=df)
summary(mod)
nobs(mod)
confint(mod)

mod <- lm(cig_tax~female_leg , data=df)
summary(mod)
nobs(mod)

mod <- lm(cig_tax~liberal , data=df)
summary(mod)
nobs(mod)



mod <- lm(healthspendpc~cig_tax , data=df)
summary(mod)
nobs(mod)
confint(mod)


pdf("/Users/kevinreuning/Dropbox/Teaching_Kevin/Fall_2019/POL_306/Lectures/images/assums_ex.pdf",
    height=8, width=12)
par(mfrow=c(1,2))
plot(mod$residuals)
plot(y=mod$residuals, x=mod$fitted.values)
dev.off()


full_df <- read.csv("/Users/kevinreuning/Dropbox/Teaching_Kevin/Fall_2019/POL_306/COSP_Limited.csv")

tmp <- full_df[,c("z_cigarette_taxes", "pctfemaleleg",
                  "liberal", "conservative", "state", "year",
                  "democrat",
                  "hou_chamber")]

tmp <- tmp[tmp$year > 2000,]
tmp <- tmp[tmp$state!="Delaware",]
names(tmp)[1:2] <- c("cig_taxes", "female_leg")
mod1 <- lm(cig_taxes~liberal+hou_chamber, data=tmp)
mod2 <- lm(cig_taxes~hou_chamber, data=tmp)
mod3 <- lm(cig_taxes~liberal, data=tmp)
summary(mod1)

stargazer::stargazer(mod1, mod2, mod3)

pdf("/Users/kevinreuning/Dropbox/Teaching_Kevin/Fall_2019/POL_306/Lectures/images/multico.pdf",
    height=8, width=8)
plot(tmp$liberal, tmp$hou_chamber, xlab="Percentage Liberals",
     ylab="Legislative Ideology")
dev.off()

cor(tmp$liberal, tmp$hou_chamber, use='complete.obs')


df <- subset(df, dem_gov!=.5)

pdf("/Users/kevinreuning/Dropbox/Teaching_Kevin/Fall_2019/POL_306/Lectures/images/dummy.pdf",
     height=8, width=8)
boxplot(cig_tax~dem_gov, data=df, names=c("Rep Gov", "Dem Gov"), ylab="Cig Tax")
dev.off()

mod <- lm(cig_tax~dem_gov, data=df)
summary(mod)
nobs(mod)

with(df, by(cig_tax, dem_gov, mean))
with(df, by(cig_tax, dem_gov, sd))
t.test(cig_tax~dem_gov, data=df)



full_df <- read.csv("/Users/kevinreuning/Dropbox/Teaching_Kevin/Fall_2019/POL_306/COSP_Limited.csv")

tmp <- full_df[,c("z_cigarette_taxes", "pctfemaleleg",
                  "govparty_a", "hs_dem_prop_all",
                  "sen_dem_prop_all", "healthspendpc",
                  "liberal", "conservative", "state", "year",
                  "democrat", "z_education_expenditures_per_pup", "incomepcap")]


tmp <- tmp[tmp$year > 2000,]
# tmp <- tmp[tmp$state!="Delaware",]
names(tmp)[1:5] <- c("cig_taxes", "female_leg", "dem_gov",
                     "house_dem", "sen_dem")
tmp$house_dem <- 1*(tmp$house_dem > .5)
tmp$sen_dem <- 1*(tmp$sen_dem > .5)

mod <- lm(cig_taxes~house_dem, data=tmp)
summary(mod)


tmp$control <- tmp$house_dem + tmp$sen_dem + tmp$dem_gov
tmp$control <- ifelse(tmp$control==0, "Republican",
                      ifelse(tmp$control==3, "Democrat", "Mixed"))
pdf("/Users/kevinreuning/Dropbox/Teaching_Kevin/Fall_2019/POL_306/Lectures/images/dummy2.pdf",
    height=8, width=8)
boxplot(cig_taxes~control, data=tmp, ylab="Cigarette Tax")
dev.off()


mod <- lm(cig_taxes~control, data=tmp)
summary(mod)
nobs(mod)

tmp$control <- as.factor(tmp$control)
mod <- lm(cig_taxes~relevel(control, ref = "Mixed"), data=tmp)
summary(mod)
nobs(mod)

mod <- lm(cig_taxes~relevel(control, ref = "Republican"), data=tmp)
summary(mod)
nobs(mod)

mod <- lm(z_education_expenditures_per_pup~control + incomepcap  + liberal , data=tmp)
summary(mod)
nobs(mod)







full_df <- read.csv("/Users/kevinreuning/Dropbox/Teaching_Kevin/Fall_2019/POL_306/COSP_Limited.csv")

tmp <- full_df[,c("unemployment",
                  "govparty_a", "hs_dem_prop_all",
                  "sen_dem_prop_all",
                  "state", "year", "labor_minwage_abovefed",
                  "z_education_expenditures_per_pup",
                  "welfare_spending_percap",
                  "incomepcap")]

tmp <- tmp[tmp$year > 1995,]
# tmp <- tmp[tmp$state!="Delaware",]
names(tmp)[2:4] <- c("dem_gov",
                     "house_dem", "sen_dem")
tmp$house_dem <- 1*(tmp$house_dem > .5)
tmp$sen_dem <- 1*(tmp$sen_dem > .5)
tmp$control <- tmp$house_dem + tmp$sen_dem + tmp$dem_gov
tmp$control <- ifelse(tmp$control==0, "Republican",
                      ifelse(tmp$control==3, "Democrat", "Mixed"))
tmp$control <- as.factor(tmp$control)


tmp <- tmp[,c("welfare_spending_percap", "control",
              "unemployment", "incomepcap", "state", "year")]

names(tmp) <- c("welf_percap", "gov_control", "unemp_rate",
                "inc_percap", "state", "year")

tmp$inc_percap <- tmp$inc_percap/1000
tmp <- tmp[tmp$year<2005,]
tmp <- tmp[order(tmp$year), ]
tmp$south <- 0
tmp$south[tmp$state %in% c("Alabama", "Arkansas", "Florida",
                           "Georgia", "Kansas", "Kentucky",
                           "Louisiana", "Mississippi",
                           "Missouri", "North Carolina", "South Carolina",
                           "Tennessee", "Texas", "Virginia")] <- 1
mod <- lm(welf_percap~ gov_control +  unemp_rate +
              (inc_percap) + south,
          data=tmp)
summary(mod)
nobs(mod)
plot(mod$residuals)
plot(mod$residuals, mod$fitted.values)
summary(tmp)
write.csv(tmp, "/Users/kevinreuning/Dropbox/Teaching_Kevin/Fall_2019/POL_306/MV_Assignment.csv",
          row.names = F)




mod <- lm(welf_percap~ relevel(gov_control, ref="Mixed") +
              unemp_rate +
              (inc_percap) + south,
          data=tmp)
summary(mod)



pdf("/Users/kevinreuning/Dropbox/Teaching_Kevin/Fall_2019/POL_306/Lectures/images/outliers.pdf",
    height=4, width=4)
par(mar=c(2,2,2,0)+2)
n <- 20
set.seed(3)
x <- rnorm(n, 0, 5)
y <- .25*x + rnorm(n, 0, .5)
plot(y=y, x=x, ylim=c(-6, 6), xlim=c(-12, 22), xlab="X", ylab="Y", main="Good Data!")
abline(lm(y~x))


y_tmp <- c(y, 5)
x_tmp <- c(x, 0)
plot(y=y_tmp, x=x_tmp, ylim=c(-6, 6),  xlim=c(-12, 22), xlab="X", ylab="Y", main="Not so Problematic")
abline(lm(y_tmp~x_tmp))
abline(lm(y~x), lty=2)

y_tmp <- c(y, .25*15)
x_tmp <- c(x, 15)
plot(y=y_tmp, x=x_tmp, ylim=c(-6, 6),  xlim=c(-12, 22), xlab="X", ylab="Y", main="Not so Problematic")
abline(lm(y_tmp~x_tmp))
abline(lm(y~x), lty=2)


y_tmp <- c(y, -5)
x_tmp <- c(x, 20)
plot(y=y_tmp, x=x_tmp, ylim=c(-6, 6),  xlim=c(-12, 22), xlab="X", ylab="Y", main="Very Problematic")
abline(lm(y_tmp~x_tmp))
abline(lm(y~x), lty=2)
dev.off()



n <- 100
set.seed(3)
x <- runif(n, 20, 10000)
y <- log(1*x) + rnorm(n, 0, .2)
plot(y=(y), x=(x))
abline(lm((y)~x))



pdf("/Users/kevinreuning/Dropbox/Teaching_Kevin/Fall_2019/POL_306/Lectures/images/log.pdf",
    height=6, width=7)
plot(y=log(seq(1:100)), x=1:100, type='l', ylab='log(x)', xlab='x', main='log (or ln) Transformation')
dev.off()




tmp <- full_df[,c("unemployment",
                  "govparty_a", "hs_dem_prop_all",
                  "sen_dem_prop_all",
                  "state", "year", "labor_minwage_abovefed",
                  "z_education_expenditures_per_pup",
                  "welfare_spending_percap",
                  "incomepcap", "liberal")]

tmp <- tmp[tmp$year > 1995,]
# tmp <- tmp[tmp$state!="Delaware",]
names(tmp)[2:4] <- c("dem_gov",
                     "house_dem", "sen_dem")
tmp$house_dem <- 1*(tmp$house_dem > .5)
tmp$sen_dem <- 1*(tmp$sen_dem > .5)
tmp$control <- tmp$house_dem + tmp$sen_dem + tmp$dem_gov
tmp$control <- ifelse(tmp$control==0, "Republican",
                      ifelse(tmp$control==3, "Democrat", "Mixed"))
tmp$control <- as.factor(tmp$control)


tmp <- tmp[,c("welfare_spending_percap", "control",
              "unemployment", "incomepcap", "state", "year",
              "z_education_expenditures_per_pup", "liberal")]

names(tmp) <- c("welf_percap", "gov_control", "unemp_rate",
                "inc_percap", "state", "year", "ed_spending",
                "liberal")

tmp$log_incpercap <- log(tmp$inc_percap)
tmp$inc_percap <- (tmp$inc_percap)/1000
tmp$log_welfpercap <- log(tmp$welf_percap)
mod_normal <- lm(inc_percap~
              (welf_percap) ,
          data=tmp)
summary(mod_normal)
pdf("/Users/kevinreuning/Dropbox/Teaching_Kevin/Fall_2019/POL_306/Lectures/images/lin_log.pdf",
    height=6, width=7)
plot(y=tmp$inc_percap, x=tmp$welf_percap, ylim=c(15,50), ylab="Income per Capita ($1k)",
     xlab="Welfare per Capita")


plot(y=tmp$inc_percap, x=tmp$welf_percap, ylim=c(15,50), ylab="Income per Capita ($1k)",
     xlab="Welfare per Capita", col='gray')
abline(mod_normal)

mod <- lm(inc_percap~
              (log_welfpercap) ,
          data=tmp)
summary(mod)
nobs(mod)
plot(y=tmp$inc_percap, x=exp(tmp$log_welfpercap), ylim=c(15,50), ylab="Income per Capita ($1k)",
     xlab="Welfare per Capita", col='gray')
x <- seq(log(1),
         8, by=.1)
y <- coef(mod)[1] + coef(mod)[2]*x
lines(y=y, x=exp(x))
abline(mod_normal, lty=2)
dev.off()


mod <- lm(inc_percap~
              (log_welfpercap) + unemp_rate + gov_control ,
          data=tmp)
summary(mod)
summary(lm(inc_percap~
               (log_welfpercap), data=mod$model))

mod <- lm(log_welfpercap~
              (inc_percap) ,
          data=tmp)
summary(mod)
plot(y=tmp$welf_percap, x=(tmp$inc_percap))
x <- seq(min(tmp$inc_percap, na.rm=T),
         max(tmp$inc_percap, na.rm=T), by=.1)
y <- coef(mod)[1] + coef(mod)[2]*x
lines(y=exp(y), x=(x))
abline(mod_normal)



# tmp <- tmp[tmp$state!="Delaware",]
tmp$ed_spending <- tmp$ed_spending/1000
mod_normal<- lm(welf_percap~
              (ed_spending) ,
          data=tmp)
summary(mod_normal)

mod<- lm(log_welfpercap~
                    (ed_spending) ,
                data=tmp)
summary(mod)
pdf("/Users/kevinreuning/Dropbox/Teaching_Kevin/Fall_2019/POL_306/Lectures/images/log_lin.pdf",
    height=6, width=7)
plot(y=tmp$welf_percap, x=(tmp$ed_spending), col='black', ylab="Welfare per Capita", xlab="Ed Spending per Pupil ($1k)")
plot(y=tmp$welf_percap, x=(tmp$ed_spending), col='gray', ylab="Welfare per Capita", xlab="Ed Spending per Pupil ($1k)")
abline(mod_normal, lty=1)

plot(y=tmp$welf_percap, x=(tmp$ed_spending), col='gray', ylab="Welfare per Capita", xlab="Ed Spending per Pupil ($1k)")

x <- seq(1, 17, by=.1)
y <- coef(mod)[1] + coef(mod)[2]*x
lines(y=exp(y), x=(x))
abline(mod_normal, lty=2)
dev.off()

mod <- lm(log_welfpercap~
              (ed_spending) + unemp_rate + gov_control ,
          data=tmp)
summary(mod)
summary(lm(log_welfpercap~
               (ed_spending), data=mod$model))



tmp <- full_df[,c("unemployment",
                  "govparty_a", "hs_dem_prop_all",
                  "sen_dem_prop_all",
                  "state", "year", "labor_minwage_abovefed",
                  "z_education_expenditures_per_pup",
                  "welfare_spending_percap",
                  "incomepcap", "liberal")]

# tmp <- tmp[tmp$year > 1995,]
# tmp <- tmp[tmp$state!="Delaware",]

tmp$hs_dem_prop_all <- tmp$hs_dem_prop_all*100
tmp$z_education_expenditures_per_pup <- tmp$z_education_expenditures_per_pup/1000

pdf("/Users/kevinreuning/Dropbox/Teaching_Kevin/Fall_2019/POL_306/Lectures/images/poly.pdf",
    height=6, width=7)
plot(tmp$hs_dem_prop_all, tmp$z_education_expenditures_per_pup,xlab="Percentage Dems in House",
     ylab="Ed Spending per Pupil ($1k)")
mod_normal <- lm(z_education_expenditures_per_pup~hs_dem_prop_all, data=tmp)
mod <- lm(z_education_expenditures_per_pup~hs_dem_prop_all + I(hs_dem_prop_all^2), data=tmp)
summary(mod)
summary(mod_normal)

plot(tmp$hs_dem_prop_all, tmp$z_education_expenditures_per_pup,xlab="Percentage Dems in House",
     ylab="Ed Spending per Pupil ($1k)", col='gray')
abline(mod_normal, lty=1)







plot(tmp$hs_dem_prop_all, tmp$z_education_expenditures_per_pup,xlab="Percentage Dems in House",
     ylab="Ed Spending per Pupil ($1k)", col='gray')

x <- seq(10, 100, by=1)
y <- coef(mod)[1] + coef(mod)[2] * x + coef(mod)[3] *( x^2)
lines(y=y, x=x)
abline(mod_normal, lty=2)

dev.off()

plot(full_df$hs_dem_prop_all, full_df$z_tanf_maxpayment)


full_df$hs_dem_prop_all <- full_df$hs_dem_prop_all*100
mod <- lm(z_education_expenditures_per_pup~hs_dem_prop_all + I(hs_dem_prop_all^2) +
              log(welfare_spending_percap) + unemployment  +
              labor_minwage_abovefed + I(100*pop5to17/poptotal), data=full_df)
summary(mod)

new_df <- as.data.frame(t(apply(mod$model, 2, median)))
new_df <- new_df[rep(1, 76),]
new_df$hs_dem_prop_all <- 12:87
new_df$`I(hs_dem_prop_all^2)` <- (12:87)^2

y <- as.matrix(cbind(1, new_df[,-1])) %*% (coef(mod))

pdf("/Users/kevinreuning/Dropbox/Teaching_Kevin/Fall_2019/POL_306/Lectures/images/preds.pdf",
    height=6, width=7)
plot(y=y, x=12:87, type='l', ylab="Ed Spending per Pupil ($1k)", xlab="Percent Dems")



new_df <- as.data.frame(t(apply(mod$model, 2, median)))
new_df <- new_df[rep(1, 23),]
new_df$`log(welfare_spending_percap)` <- seq(5.5, 7.7, by=.1)

y <- as.matrix(cbind(1, new_df[,-1])) %*% (coef(mod))
plot(y=y, x=exp(seq(5.5, 7.7, by=.1)), type='l', ylab="Ed Spending per Pupil ($1k)",
     xlab="Welfare Spending per Capita")
dev.off()
summary(mod)
nobs(mod)




full_df$south <- 0
full_df$south[full_df$state %in% c("Alabama", "Arkansas", "Florida",
                           "Georgia", "Kansas", "Kentucky",
                           "Louisiana", "Mississippi",
                           "Missouri", "North Carolina", "South Carolina",
                           "Tennessee", "Texas", "Virginia")] <- 1

tmp_df <- full_df[full_df$year<1990,]
plot(tmp_df$hs_dem_prop_all, tmp_df$z_education_expenditures_per_pup, pch=tmp_df$south+1)
mod <- lm(z_education_expenditures_per_pup~hs_dem_prop_all*south, data=tmp_df)
summary(mod)

library(ggplot2)
library(ggthemes)
tmp_df$south <- factor(tmp_df$south, labels=c("Not-South", "South"))

pdf("/Users/kevinreuning/Dropbox/Teaching_Kevin/Fall_2019/POL_306/Lectures/images/inter.pdf",
    height=6, width=7)
ggplot(tmp_df, aes(y=z_education_expenditures_per_pup, x=hs_dem_prop_all)) +
    # geom_point(aes(color=south), alpha=.5) +
    geom_point( alpha=.5) +
    scale_color_calc("South") +
    labs(y="Education Spending per Capita", x="Democrats in Lower Leg") +
    # facet_wrap(~south) +
    theme_minimal() +
    # geom_smooth(method='lm', se=F) +
    NULL

ggplot(tmp_df, aes(y=z_education_expenditures_per_pup, x=hs_dem_prop_all)) +
    # geom_point(aes(color=south), alpha=.5) +
    geom_point( alpha=.5) +
    scale_color_calc("South") +
    labs(y="Education Spending per Capita", x="Democrats in Lower Leg") +
    # facet_wrap(~south) +
    theme_minimal() +
    geom_smooth(method='lm', se=F, color='black') +
    NULL



ggplot(tmp_df, aes(y=z_education_expenditures_per_pup, x=hs_dem_prop_all)) +
    geom_point(aes(color=south), alpha=.5) +
    scale_color_calc("South") +
    labs(y="Education Spending per Capita", x="Democrats in Lower Leg") +
    # facet_wrap(~south) +
    theme_minimal() +
    geom_smooth(method='lm', se=F, color='black') +
    NULL


# ggplot(tmp_df, aes(y=z_education_expenditures_per_pup, x=hs_dem_prop_all)) +
#     geom_point(aes(color=south), alpha=.5) +
#     scale_color_calc("South") +
#     labs(y="Education Spending per Capita", x="Democrats in Lower Leg") +
#     # facet_wrap(~south) +
#     theme_minimal() +
#     # geom_smooth(method='lm', se=F) +
#     NULL

ggplot(tmp_df, aes(y=z_education_expenditures_per_pup, x=hs_dem_prop_all)) +
    geom_point(aes(color=south), alpha=.5) +
    scale_color_calc("South") +
    labs(y="Education Spending per Capita", x="Democrats in Lower Leg") +
    facet_wrap(~south) +
    theme_minimal() +
    geom_smooth(method='lm', se=F, color='black') +
    NULL


ggplot(tmp_df, aes(y=z_education_expenditures_per_pup, x=hs_dem_prop_all)) +
    geom_point(aes(color=south), alpha=.5) +
    scale_color_calc("South") +
    labs(y="Education Spending per Capita", x="Democrats in Lower Leg") +
    # facet_wrap(~south) +
    theme_minimal() +
    geom_smooth(method='lm', se=F,  aes(color=south)) +
    NULL

dev.off()



tmp_df <- full_df[full_df$year>2000,]
tmp_df$gini_coef
mod <- lm(welfare_spending_percap~hs_dem_prop_all*gini_coef, data=tmp_df)
summary(mod)
nobs(mod)
# plot(mod)

range(mod$model$gini_coef)
gini <- seq(.52, .71, by=.01)
coef <- coef(mod)[2] + coef(mod)[4]*gini

se <- sqrt(vcov(mod)[2,2] + gini^2 * vcov(mod)[4,4] + 2* gini* vcov(mod)[2,4])

pdf("/Users/kevinreuning/Dropbox/Teaching_Kevin/Fall_2019/POL_306/Lectures/images/inter_cont.pdf",
    height=6, width=7)
plot(y=coef, x=gini, type="l", ylim=c(-5,25), ylab="Effect of % Dems", xlab="Gini")
lines(y=coef + 2*se, x=gini, lty=2)
lines(y=coef - 2*se, x=gini, lty=2)
abline(h=0, col='gray')
dev.off()





mod
range(mod$model$hs_dem_prop_all)
hs <- seq(12, 87, by=1)
coef <- coef(mod)[3] + coef(mod)[4]*hs

se <- sqrt(vcov(mod)[3,3] + hs^2 * vcov(mod)[4,4] + 2* hs* vcov(mod)[3,4])

pdf("/Users/kevinreuning/Dropbox/Teaching_Kevin/Fall_2019/POL_306/Lectures/images/inter_cont2.pdf",
    height=6, width=7)
plot(y=coef, x=hs, type="l", ylim=c(-5000,6500), ylab="Effect of Gini", xlab="% Dems")
lines(y=coef + 2*se, x=hs, lty=2)
lines(y=coef - 2*se, x=hs, lty=2)
abline(h=0, col='gray')
dev.off()
