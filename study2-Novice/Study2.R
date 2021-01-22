##
## Run IAT_Project_July2020.R first, use the resulting file
## in the python script clean_mo.py and load the result here 
##
install.packages("reticulate")
library(reticulate)
py_run_file("script.py")

dat2 <- read.csv("/home/aida/Projects/Stereotype/Data/survey_long.csv")

stereo_mod <- glmer(hate_labels ~ warmth + competence + (1| SGT) + (1|user_idx), family=poisson, data=dat2)
disagree_mod <- glmer(disagree ~ warmth + competence + (1| SGT) + (1|user_idx), family=poisson, data=dat2)
# dis_neg_mod <- glmer(hate_labels ~ warmth + competence + (1| SGT) + (1|user_idx), family=poisson, data=dat2)
# dis_pos_mod <- glmer(hate_labels ~ warmth + competence + (1| SGT) + (1|user_idx), family=poisson, data=dat2)


dat <- read.csv("/home/aida/Projects/Stereotype/Data/survey_SGT.csv")
library(gridExtra)

dat$SD_tot <- (dat$SD_black_tot + dat$SD_communist_tot + dat$SD_gay_tot + dat$SD_immigrant_tot + 
  dat$SD_jewish_tot + dat$SD_woman_tot + dat$SD_liberal_tot + dat$SD_muslim_tot) / 8

dat$warm_tot <- (dat$SD_black_warm + dat$SD_communist_warm + dat$SD_gay_warm + dat$SD_immigrant_warm + 
                 dat$SD_jewish_warm + dat$SD_woman_warm + dat$SD_liberal_warm + dat$SD_muslim_warm) / 8

dat$competent_tot <- (dat$SD_black_competent + dat$SD_communist_competent + dat$SD_gay_competent + dat$SD_immigrant_competent + 
                 dat$SD_jewish_competent + dat$SD_woman_competent + dat$SD_liberal_competent + dat$SD_muslim_competent) / 8


dat2 = dat %>% 
  mutate(gay_hate = rowSums(dat%>%select(starts_with("Gay_"))),
       black_hate = rowSums(dat%>%select(starts_with("Black_"))),
       communist_hate = rowSums(dat%>%select(starts_with("Communist_"))),
       immigrant_hate = rowSums(dat%>%select(starts_with("Immigrant_"))),
       jewish_hate = rowSums(dat%>%select(starts_with("Jew_"))),
       woman_hate = rowSums(dat%>%select(starts_with("Woeman_"))),
       muslim_hate = rowSums(dat%>%select(starts_with("Muslim_"))),
       liberal_hate = rowSums(dat%>%select(starts_with("Liberal_"))),
       ) %>% 
  pivot_longer(cols= dat %>% select(ends_with("hate")))

stereo_hate <- glm(hate_tot ~ competent_tot + warm_tot, family=poisson(), data=dat)

gg = list()
## Analyzing the total number of hate labels
implicit_hate <- glmer(hate_SGT_tot ~ D_score + (1|SGT), family=poisson(), data=dat)
summary(implicit_hate)
#ggpredict(implicit_hate, c("D_score", "SGT")) %>% plot(rawdata = TRUE, jitter = .01)
g <- ggplot(dat, aes(x=D_score, y=hate_tot)) + 
  geom_point(aes(color=SGT)) + 
  geom_smooth(method=lm, level=0.95) + 
  theme_bw() + 
  labs(title ="", y = "", colour = "", x = "") + 
  theme(text = element_text(size=20, family ="times"), legend.position = "none")
gg[[2]] <- g



explicit_hate <- glm(hate_tot ~ SD_tot, family=poisson(), data=dat)
summary(explicit_hate)
# ggpredict(explicit_hate, c("SD_tot")) %>% plot(rawdata = TRUE, jitter = .01)
g <- ggplot(dat, aes(x=SD_tot, y=hate_tot)) + 
  geom_point() + 
  geom_smooth(method=lm, level=0.95) + 
  theme_bw() + 
  labs(title ="", y = "Number of Hate Labels", x = "") + 
  theme(text = element_text(size=20, family ="times"))
gg[[1]] <- g

## Analyzing the disagreements
implicit_dis <- glmer(disagree_tot ~ D_score + (1|SGT), family=poisson(), data=dat)
summary(implicit_dis)
#ggpredict(implicit_dis, c("D_score", "SGT")) %>% plot(rawdata = TRUE, jitter = .01)

implicit_dis_p <- glmer(disagree_pos ~ D_score + (1|SGT), family=poisson(), data=dat)
summary(implicit_dis_p)
#ggpredict(implicit_dis_p, c("D_score", "SGT")) %>% plot(rawdata = TRUE, jitter = .01)

implicit_dis_n <- glmer(disagree_neg ~ D_score + (1|SGT), family=poisson(), data=dat)
summary(implicit_dis_n)
#ggpredict(implicit_dis_n, c("D_score", "SGT")) %>% plot(rawdata = TRUE, jitter = .01)

g <- ggplot(dat, aes(x=D_score, y=disagree_tot)) + 
  geom_point(aes(color=SGT)) + 
  geom_smooth(method=lm, level=0.95) + 
  theme_bw() + 
  labs(title ="", y = "", colour = "IAT Test", x = "") + 
  theme(text = element_text(size=20, family ="times"), legend.position = "none")
gg[[4]] <- g

explicit_dis <- glm(disagree_tot ~ SD_tot, family=poisson(), data=dat)
summary(explicit_dis)
#ggpredict(explicit_dis, c("SD_tot")) %>% plot(rawdata = TRUE, jitter = .01)

explicit_dis_p <- glm(disagree_pos ~ SD_tot, family=poisson(), data=dat)
summary(explicit_dis_p)
#ggpredict(explicit_dis_p, c("SD_tot")) %>% plot(rawdata = TRUE, jitter = .01)

explicit_dis_n <- glm(disagree_neg ~ SD_tot, family=poisson(), data=dat)
summary(explicit_dis_n)
#ggpredict(explicit_dis_n, c("SD_tot")) %>% plot(rawdata = TRUE, jitter = .01)

g <- ggplot(dat, aes(x=SD_tot, y=disagree_tot)) + 
  geom_point() + 
  geom_smooth(method=lm, level=0.95) + 
  theme_bw() + 
  labs(title ="", y = "Disagreement", x = "") + 
  theme(text = element_text(size=20, family ="times"))
gg[[3]] <- g

## Item response Rasch

library(eRm)
ir <- read.csv("/home/aida/Projects/Stereotype/Data/item_response.csv")
res_rm_1 <- RM(ir)
betas <- -coef(res_rm_1)
round(sort(betas), 2)

pers <- person.parameter(res_rm_1)
dat$ability <- coef(pers)

implicit_rasch <- lmer(ability ~ D_score + (1 | SGT), data=dat)
summary(implicit_rasch)
#ggpredict(implicit_rasch, c("D_score", "SGT")) %>% plot(rawdata = TRUE, jitter = .01)

g <- ggplot(dat, aes(x=D_score, y=ability)) + 
  geom_point(aes(color=SGT)) + 
  geom_smooth(method=lm, level=0.95) + 
  theme_bw() + labs(title ="", y = "", colour = "IAT Test", x = "Implicit Bias") + 
  theme(text = element_text(size=20, family ="times"), legend.position="none") 
gg[[6]] <- g

explicit_rasch <- lm(ability ~ SD_tot, data=dat)
summary(explicit_rasch)
#ggpredict(explicit_rasch, c("SD_tot")) %>% plot(rawdata = TRUE, jitter = .01)

g <- ggplot(dat, aes(x=SD_tot, y=ability)) + 
  geom_point() + 
  geom_smooth(method=lm, level=0.95) + 
  theme_bw() + 
  labs(title ="", y = "Sensitivity", x = "Explicit Bias") + 
  theme(text = element_text(size=20, family ="times"))

gg[[5]] <- g

do.call("grid.arrange", c(gg, nrow = 3))


ggplotRegression <- function (fit, x_l, y_l) {
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    xlim(-1.5,1.5) + ylim(0, 7) +
    stat_smooth(method = "lm", col = "red", fullrange=TRUE) +
    theme_bw() + theme(text = element_text(size=20, family ="times")) +labs(y=y_l, x=x_l)  
}


# Black implicit
dat_black <- dat[dat$SGT == "Black",]
m <- glm(hate_black_tot ~ D_score, data=dat_black, family=poisson(link = "log"))
summary(m)
ggplotRegression(m, "Implicit Bias", "Hate Labels")

###### Inaro avaz kon
# Jewish implicit
dat_black <- dat[dat$SGT == "Black",]
m <- glm(hate_black_tot ~ D_score, data=dat_black, family=poisson(link = "log"))
summary(m)
ggplotRegression(m, "Implicit Bias", "Hate Labels")

dat_black <- dat[dat$SGT == "Black",]
m <- glm(hate_black_tot ~ D_score, data=dat_black, family=poisson(link = "log"))
summary(m)
ggplotRegression(m, "Implicit Bias", "Hate Labels")

dat_black <- dat[dat$SGT == "Black",]
m <- glm(hate_black_tot ~ D_score, data=dat_black, family=poisson(link = "log"))
summary(m)
ggplotRegression(m, "Implicit Bias", "Hate Labels")

dat_black <- dat[dat$SGT == "Black",]
m <- glm(hate_black_tot ~ D_score, data=dat_black, family=poisson(link = "log"))
summary(m)
ggplotRegression(m, "Implicit Bias", "Hate Labels")

dat_black <- dat[dat$SGT == "Black",]
m <- glm(hate_black_tot ~ D_score, data=dat_black, family=poisson(link = "log"))
summary(m)
ggplotRegression(m, "Implicit Bias", "Hate Labels")

dat_black <- dat[dat$SGT == "Black",]
m <- glm(hate_black_tot ~ D_score, data=dat_black, family=poisson(link = "log"))
summary(m)
ggplotRegression(m, "Implicit Bias", "Hate Labels")
#####

ggplotRegression <- function (fit, x_l, y_l) {
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    xlim(-1.5,1.5) + ylim(0, 56) +
    stat_smooth(method = "lm", col = "red", fullrange=TRUE) +
    theme_bw() + theme(text = element_text(size=20, family ="times")) +labs(y=y_l, x=x_l)  
}

m <- glm(hate_tot ~ D_score, data=dat_black, family=poisson(link = "log"))
summary(m)
ggplotRegression(m, "Implicit Bias", "Hate Labels")


ggplotRegression <- function (fit, x_l, y_l) {
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    xlim(0, 32) + ylim(0, 7) +
    stat_smooth(method = "lm", col = "red", fullrange=TRUE) +
    theme_bw() + theme(text = element_text(size=20, family ="times")) +labs(y=y_l, x=x_l)  
}

m <- glm(hate_gay_tot ~ SD_gay_tot, data=dat, family=poisson(link = "log"))
summary(m)
ggplotRegression(m, "Implicit Bias", "Hate Labels")

ggplotRegression <- function (fit, x_l, y_l) {
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    xlim(0, 32) + ylim(0, 56) +
    stat_smooth(method = "lm", col = "red", fullrange=TRUE) +
    theme_bw() + theme(text = element_text(size=20, family ="times")) +labs(y=y_l, x=x_l)  
}

m <- glm(hate_tot ~ SD_gay_tot, data=dat, family=poisson(link = "log"))
summary(m)
ggplotRegression(m, "Implicit Bias", "Hate Labels")


gay_dat <- dat %>% group_by(sexualorientation) %>% summarize(hate_gay = mean(hate_gay_tot))
ggplot(data=gay_dat, aes(x=sexualorientation, y=hate_gay, fill=sexualorientation)) +
  geom_bar(stat="identity") + theme_minimal() + theme(text = element_text(size=20, family ="times"))


