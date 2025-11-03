## ----setup, include=FALSE--------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = F, warning = F, message = F, error = F)


## ----echo = F--------------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(cowplot)
library(extrafont)
library(tidyverse)
library(magick)
bg_col <- "#ac3939"
theme_set(
  theme_bw() + 
    theme(plot.background = element_rect(fill = bg_col, color = bg_col),
          legend.box.background = element_rect(color = "black"), 
          axis.text = element_text(color = "white"),
          axis.title = element_text(color = "white"))
)


## ----echo = F, fig.height=6, fig.width = 4---------------------------------------------------------------------
dat <- data.frame(t = seq(0, 2*pi, length.out = 1000)) %>% 
  mutate(x = cos(t), y = sin(t))
dat_arrow <- data.frame(c = rep(1:3, each = 2),
                        x1 = c(-1 + 0.1, 0, 1 - 0.1),
                        x2 = c(-2/3 + 0.1, 0, 2/3 - 0.1),
                        y1 = c(0,-1 + 0.1, 0),
                        y2 = c(-2,2/3 - 2 - 0.1, -2))

ggplot(dat) +
  geom_polygon(aes(x, y), fill = "#fcc2ff", col = "black") +
  geom_polygon(aes(x / 1.5, y / 1.5 - 2), fill = "#bffcc6", col = "black") +
  geom_segment(aes(x1, y1, xend = x2, yend = y2), data = dat_arrow, 
               arrow = arrow(length = unit(0.30,"cm"), type = "closed")) +
  annotate("text", 0, 0, label = "Population", size = 8, family = "Garamond") +
  annotate("text", 0, -2, label = "Sample", size = 8, family = "Garamond") +
  theme_nothing() +
  theme(panel.background = element_rect(fill = bg_col, color = bg_col),
        plot.background = element_rect(fill = bg_col, color = bg_col)) +
  ylim(-3,1)


## ----echo = F, fig.height=6, fig.width = 4---------------------------------------------------------------------
dat <- data.frame(t = seq(0, 2*pi, length.out = 1000)) %>% 
  mutate(x = cos(t), y = sin(t))
dat_arrow <- data.frame(c = rep(1:3, each = 2),
                        x1 = c(-1 + 0.1, 0, 1 - 0.1),
                        x2 = c(-2/3 + 0.1, 0, 2/3 - 0.1),
                        y1 = c(0,-1 + 0.1, 0),
                        y2 = c(-2,2/3 - 2 - 0.1, -2))

ggplot(dat) +
  geom_polygon(aes(x, y), fill = "#fcc2ff", col = "black") +
  geom_polygon(aes(x / 1.5, y / 1.5 - 2), fill = "#bffcc6", col = "black") +
  geom_segment(aes(x1, y1, xend = x2, yend = y2), data = dat_arrow, 
               arrow = arrow(length = unit(0.30,"cm"), type = "closed")) +
  annotate("text", 0, 0, label = "Population", size = 8, family = "Garamond") +
  annotate("text", 0, -2, label = "Sample", size = 8, family = "Garamond") +
  theme_nothing() +
  theme(panel.background = element_rect(fill = bg_col, color = bg_col),
        plot.background = element_rect(fill = bg_col, color = bg_col)) +
  ylim(-3,1)


## ----fig.width=5, fig.height=4, fig.align='center'-------------------------------------------------------------
dat_norm <- tibble(x = seq(-4,4,0.1), y_c = dnorm(x))

ggplot(dat_norm) +
  geom_line(aes(x, y_c)) +
  scale_x_continuous(breaks = -1:1, labels = c("μ - σ", "μ", "μ + σ")) +
  theme(axis.title = element_blank(), text = element_text(family = "serif", size = 15, color = "white"),
        axis.text = element_text(family = "serif", color = "white"),
        axis.ticks = element_line(color = "white"),
        panel.grid = element_blank())


## ----echo=F, fig.height=4, fig.width=14, fig.align='center'----------------------------------------------------
dat_dist <- tibble(type = "Continuous numeric\nNormal",
                   x = seq(-4,4,0.1),
                   y_c = dnorm(x)) %>% 
  bind_rows(tibble(type = "Binary\nBernoulli",
            x = 0:1, y_d = c(0.3,0.7))) %>% 
  bind_rows(tibble(type = "Discrete numeric\nPoisson",
            x = 0:10, y_d = dpois(x, 3.4))) %>% 
  bind_rows(tibble(type = "Proportion (discrete)\nBinomial",
                   x = 0:5, y_d = dbinom(x, 5, 0.3)))

dat_dist %>% 
  ggplot() +
  geom_col(aes(x, y_d), width = 0.6) +
  geom_line(aes(x, y_c)) +
  facet_wrap(~ type, scales = "free", ncol = 4) +
  scale_x_continuous(breaks = -10:10) +
  theme(axis.title = element_blank(), text = element_text(family = "serif", size = 15, color = "white"),
        axis.text = element_text(family = "serif", color = "white"),
        axis.ticks = element_line(color = "white"),
        panel.grid = element_blank())


## ----eval=FALSE, echo=FALSE------------------------------------------------------------------------------------
# tab_test <- tibble("Response \\ Explanatory" = c("Nominal", "Ordinal", "Interval/Ratio", "Binary", "Non-parametric"),
#                    Categorical = c("$\\chi^2$", "$\\chi^2$", "t-test", "$\\chi^2$- or z-test", "Kruskal-Wallis"),
#                    Numerical = c("X", "X", "Correlation or regression", "X", "Spearman correlation"))


## ----echo = T--------------------------------------------------------------------------------------------------
library(tidyverse)
set.seed(250310)
dat <- tibble(group_1 = rep(c("A","B"), each = 40),
              group_2 = rep(c("a", "b", "c", "d"), each = 20),
              numeric_explanatory = runif(80),
              nominal_response = sample(c("a", "b", "c"), 80, replace = T),
              binary_response = sample(c("a", "b"), 80, replace = T),
              numeric_response = rnorm(80) + (group_1 == "A") + numeric_explanatory)
dat


## ----echo = T, fig.height=4, fig.width = 4---------------------------------------------------------------------
ggplot(dat, aes(group_2, fill = nominal_response)) +
  geom_bar(color = "black")


## ----echo=T----------------------------------------------------------------------------------------------------
table(dat$group_2, dat$nominal_response)


## ----echo=T----------------------------------------------------------------------------------------------------
chisq.test(table(dat$group_2, dat$nominal_response))


## ----echo = T, fig.height=4, fig.width = 4---------------------------------------------------------------------
ggplot(dat, aes(group_2, fill = binary_response)) +
  geom_bar(color = "black")


## ----echo=T----------------------------------------------------------------------------------------------------
table(dat$group_2, dat$binary_response)


## ----echo=T----------------------------------------------------------------------------------------------------
chisq.test(table(dat$group_1, dat$nominal_response))


## ----echo = T, fig.height=4, fig.width = 4---------------------------------------------------------------------
ggplot(dat, aes(group_1, fill = binary_response)) +
  geom_bar(color = "black")


## ----echo=T----------------------------------------------------------------------------------------------------
table(dat$group_1, dat$binary_response)


## ----echo=T----------------------------------------------------------------------------------------------------
prop.test(x = c(15,16), n = c(40, 40))


## ----echo = T, fig.height=4, fig.width = 4---------------------------------------------------------------------
ggplot(dat, aes(group_1, numeric_response)) +
  geom_point()


## ----echo=T----------------------------------------------------------------------------------------------------
t.test(numeric_response ~ dat$group_1, dat)


## ----echo = T, fig.height=4, fig.width = 4---------------------------------------------------------------------
ggplot(dat, aes(group_2, numeric_response)) +
  geom_point()


## ----echo=T----------------------------------------------------------------------------------------------------
mod <- lm(numeric_response ~ group_2, dat)
anova(mod)


## ----echo=T----------------------------------------------------------------------------------------------------
library(emmeans)
emmeans(mod, pairwise ~ group_2)


## ----echo = T, fig.height=4, fig.width = 4---------------------------------------------------------------------
ggplot(dat, aes(numeric_explanatory, numeric_response)) +
  geom_point()


## ----echo=T----------------------------------------------------------------------------------------------------
cor.test(dat$numeric_explanatory, 
         dat$numeric_response)


## ----echo=T----------------------------------------------------------------------------------------------------
mod <- lm(numeric_response ~ numeric_explanatory, dat)
anova(mod)


## ----fig.align='center', fig.width=5, fig.height=5, echo=F-----------------------------------------------------
ggplot(dat, aes(numeric_explanatory, numeric_response)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme(axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"))


## ----echo = T, fig.height=4, fig.width = 4---------------------------------------------------------------------
ggplot(dat, aes(group_2, rank(numeric_response))) +
  geom_point()


## ----echo=T----------------------------------------------------------------------------------------------------
kruskal.test(numeric_response ~ group_2, dat)


## ----echo = T, fig.height=4, fig.width = 4---------------------------------------------------------------------
ggplot(dat, aes(rank(numeric_explanatory), 
                rank(numeric_response))) +
  geom_point()


## ----echo=T----------------------------------------------------------------------------------------------------
cor.test(dat$numeric_explanatory, 
         dat$numeric_response, 
         method = "spearman")


## ----echo=T----------------------------------------------------------------------------------------------------
t.test(numeric_response ~ group_1, dat, var.equal = T)$p.value


## ----echo=T----------------------------------------------------------------------------------------------------
mod <- lm(numeric_response ~ group_1, dat)
anova(mod)


## ----echo=T----------------------------------------------------------------------------------------------------
chisq.test(table(dat$group_2, dat$nominal_response))$p.value


## ----echo=T----------------------------------------------------------------------------------------------------
dat_freq <- dat %>% count(group_2, nominal_response)
mod <- glm(n ~ group_2 * nominal_response, dat_freq, family = "poisson")
anova(mod, test = "Rao")


## ----echo=T----------------------------------------------------------------------------------------------------
kruskal.test(numeric_response ~ group_1, dat)$p.value


## ----echo=T----------------------------------------------------------------------------------------------------
t.test(rank(numeric_response) ~ group_1, dat)$p.value


## ----echo=T----------------------------------------------------------------------------------------------------
cor.test(dat$numeric_explanatory, dat$numeric_response, method = "spearman", exact = F)$p.value


## ----echo=T----------------------------------------------------------------------------------------------------
cor.test(rank(dat$numeric_explanatory), rank(dat$numeric_response), method = "pearson")$p.value

