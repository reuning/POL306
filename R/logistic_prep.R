setwd("/Users/kevinreuning/Dropbox/Teaching_Kevin/POL306/")
df <- read.csv("ANES_Par.csv")


# table(df$POSTVOTE_PRESVTWHO)
# df$mill_tax <- df$POSTVOTE_PRESVTWHO
# df$mill_tax[df$mill_tax %in% c("Gary Johnson", "Jill Stein", "Other candidate")] <- NA
# df$mill_tax <- 1*(df$mill_tax == "Donald Trump")

table(df$MILLN_MILLTAX)
df$mill_tax <- df$MILLN_MILLTAX
df$mill_tax[df$mill_tax %in% c("Neither favor nor oppose")] <- NA
df$mill_tax <- 1*(df$mill_tax == "Favor")

levels(df$pid7) <- c("4 Independent", "3 Lean Democrat",
                     "5 Lean Republican", NA, "2 Not very strong Democrat",
                     "6 Not very strong Republican", "1 Strong Democrat",
                     "7 Strong Republican")
df$age <- 2016 - df$birthyr
df$bornagain <- 1*(df$pew_bornagain=="Yes")
df$college <- df$educ
df$college <- 1*(df$college %in% c("2-Year", "4-Year", "Post-grad"))
df$white <- df$race
df$white <- 1*(df$white=="White")
df$male <- 1*(df$gender=="Male")
df$trump <- 1*(df$POSTVOTE_PRESVTWHO =="Donald Trump")
df$pid7_interv <- as.numeric(as.factor(as.character(df$pid7)))

sub_df <- df[,c("mill_tax", "pid7", "pid7_interv",
                "age", "bornagain",
                "college", "white", "male")]
write.csv(sub_df, "tariff_supp.csv", row.names = F)

df <-read.csv("tariff_supp.csv")
mod <- glm(mill_tax~pid7_interv , family=binomial, data=df)
summary(mod)

plot(plogis(coef(mod)[1] + coef(mod)[2]*1:7), type="l")


mod <- glm(mill_tax~pid7_interv +  white + male, family=binomial, data=df)
summary(mod)

plot(plogis(coef(mod)[1] + coef(mod)[2]*1:7 +  coef(mod)[4]) , type="l", ylim=c(0, 1))
lines(plogis(coef(mod)[1] + coef(mod)[2]*1:7 ), lty=2)


