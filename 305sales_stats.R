source("001package.R")
source("203df_pl.R")
source("005function.R")


# Numerical Data Descriptive Statistics----

#+ overview
psych::describe(df_pl$revenue)
Hmisc::describe(df_pl$revenue)
skimr::skim_to_wide(df_pl[-1])
skimr::skim(df_pl[-1])


#+ Central Tendency: mean, mdedian, mode
get_mode <- function(v) {
  unique_value <- unique(v)
  unique_value[which.max(tabulate(match(v, unique_value)))]
}

get_mode(df_pl$revenue)

df_pl[2] %>% 
  summarise_all(tibble::lst(sum, mean, median, max, min, var, sd, mad)) 


#+ Variability: spread out
var(df_pl$revenue)
apply(df_pl, 2, var) # each column
apply(df_pl, 2, scale)

sd(df_pl$revenue)
mad(df_pl$revenue, center = mean(df_pl$revenue))
mad(df_pl$revenue, center = median(df_pl$revenue))

IQR(df_pl$revenue)
quantile(df_pl$revenue, probs = seq(from = 0, to = 1, by = .1), na.rm = TRUE)

summary(df_pl$revenue)
lapply(df_pl, summary)
sapply(df_pl, summary)
fivenum(df_pl$revenue) # min, 25%, 50% (median), 75%, and max


#+ Shape
#' positive right-skewed distribution: more extreme values to the right causing the mean to be more than the median.
#' near-zero value indicates a normal (mesokurtic) distribution.

psych::skew(df_pl$revenue)
psych::kurtosi(df_pl$revenue)


#+ outliers
library(outliers)
outlier(df_pl$revenue) # the most extreme from the mean (same as range)
outlier(df_pl$revenue, opposite = TRUE)

z_scores <- scores(df_pl$revenue, type = "z") # based on z-scores
which(abs(z_scores) > 1.96)
which(scores(df_pl$revenue, type = "iqr", lim = 1.5)) # based on boxplot "whiskers"

df_pl[which(abs(z_scores) > 1.96), 1:6]
df_pl[which(abs(scale(df_pl$revenue)) > 1.96), 1:6] # scale = zscore (same result)***
  

#+ histogram
df_pl %>% 
  ggplot(aes(revenue)) +
  scale_x_log10(labels = scales::dollar) +
  geom_histogram(show.legend = FALSE) +
  geom_vline(data = df_pl %>% summarise(mean = mean(revenue)), 
             aes(xintercept = mean),
             linetype = "dashed", size = 1, color = "red", 
             show.legend = FALSE) # mean didn't tranform (log10) because of 0***


#+ dotplot
df_pl %>% 
  ggplot(aes(x = revenue)) + 
  geom_dotplot() +
  scale_x_continuous(labels = scales::dollar) # not useful


# Categorical Data Descriptive Statistics----

#+
table(df_ar$month, df_ar$weekday) %>% addmargins()
table(df_ar$month, df_ar$weekday) %>% prop.table(margin = 1) %>% round(2) %>% addmargins(2)
mosaicplot(~ month + weekday, data = df_ar, color = TRUE, las = 1)


#+
table(df_ar$month, df_ar$weekday) %>% ftable() # count

table(df_ar$month, df_ar$weekday) %>% addmargins() # add total
table(df_ar$month, df_ar$weekday) %>% margin.table(1) # add one line total (row/column)
table(df_ar$month, df_ar$weekday) %>% margin.table(2)

table(df_ar$month, df_ar$weekday) %>% prop.table() %>% round(2) # proportion
table(df_ar$month, df_ar$weekday) %>% prop.table(margin = 1) %>% round(2) # rowwise 1%

table(mtcars$am) %>% chisq.test() # compare two groups***
table(mtcars$am) %>% prop.test() # Hypothesis testing for the proportion


#+
by(df_ar, df_ar$month, summary)
by(df_ar$month, df_ar$weekday, summary) # summary the first param like table

psych::describeBy(df_ar$credit, df_ar$weekday, mat = TRUE)
table1::table1(~credit + month | weekday, data = df_ar[df_ar$subaccount == "Revenue", ])


#+
mytable <- xtabs(credit ~ weekday + month, df_ar[df_ar$subaccount == "Revenue", ]) # By default, xtabs gets sum
ftable(mytable) %>% summary()
vcd::assocstats(mytable)
kappa(mytable)


#+
table(cut(df_pl$revenue,
          breaks = c(-1, 1000, 6000, 15000, 32000),
          labels = c("Less than 1000", "6000", "15000", "More than 15000")))


#+ ggridge plot
library(ggridges)
library(viridis)

df_ar %>% 
  ggplot(aes(credit, month, fill = ..x..)) +
  geom_density_ridges_gradient(color = "white", 
                               size = 0.5, 
                               scale = 3, 
                               rel_min_height = 0.01, 
                               gradient_lwd = 1.) + 
  scale_fill_viridis(option = "inferno", labels = comma_format(), name = "") +
  scale_x_continuous(labels = comma_format()) +
  facet_wrap(~subaccount) +
  theme(panel.background = element_rect(fill = "white"), 
        # panel.grid = element_blank(), 
        # aspect.ratio = 1, 
        # axis.title = element_blank(), 
        # axis.text = element_blank(), 
        # axis.ticks = element_blank(), 
        legend.position = "none") +
  labs(title = "Monthly credit density", 
       x = "",
       y = "") # ar credit amount (payment) density



#' [Tidy Rain cloud plot](https://orchid00.github.io/tidy_raincloudplot)
#' [The ultimate EDA visualization](https://towardsdatascience.com/the-ultimate-eda-visualization-in-r-e6aff6afe5c1)
#+ violin plot
getPalette <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(12) 
lb <- function(x) mean(x) - sd(x)
ub <- function(x) mean(x) + sd(x)

df_sum <- df_ar %>% 
  filter(subaccount == "Revenue") %>% 
  group_by(month) %>% 
  summarise_at(vars(credit), list(ymin = lb, ymax = ub, mean = mean))

groups(df_sum)

df_ar %>% 
  filter(subaccount == "Revenue") %>% 
  ggplot(aes(factor(month), credit, fill = factor(month))) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), trim = TRUE, alpha = .8, scale = "width") +
  geom_point(aes(y = credit, color = factor(month)), 
             position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  geom_point(data = df_sum, aes(x = month, y = mean), 
             position = position_nudge(x = 0.3), size = 2.5) +
  geom_errorbar(data = df_sum, aes(ymin = ymin, ymax = ymax, y = mean), 
                position = position_nudge(x = 0.3), width = 0) +
  expand_limits(x = 5.25) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = getPalette) +
  scale_fill_manual(values = getPalette) +
  theme_light() +
  theme(legend.position="none") +
  labs(title = "", 
       x = "", 
       y = "")
 
  
#+ boxplot
df_ar %>% 
  filter(subaccount == "Revenue") %>% 
  mutate(weekday = fct_relevel(weekday, rev(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))) %>%
  ggplot(aes(weekday, credit)) +
  geom_boxplot() +
  geom_jitter(shape = 16, position = position_jitter(0.4), alpha = .3) +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar) +
  stat_summary(fun.y = mean, geom = "point", shape = 13, size = 4, color = "firebrick") +
  theme_light() +
  labs(title = "Sales outliers by weekday",
       x = "", 
       y = "")


#+ barplot
df_ar %>% 
  filter(subaccount == "Revenue") %>% 
  ggplot(aes(month, credit)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  scale_y_continuous(breaks= seq(0, 32000, by=2000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..)) # median of sales (0) for each month




