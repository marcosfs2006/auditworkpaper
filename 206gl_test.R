source("001package.R")
source("202df_tb.R")


# range of transaction dates----
range(tb$date)
skimr::skim(tb)


# missing values----
visdat::vis_dat(tb)
naniar::vis_miss(tb) # return %
inspectdf::inspect_na(tb) # return a table


# round numbers----
tb %>% 
  filter(credit != 0 & credit %% 1000 == 0) 

  
# 999 numbers----
tb %>% 
  filter((grepl(".(99)$", debit))|(grepl(".(99)$", credit))) # use viewto see full

# tb %>%
#   filter(str_detect(debit, ".[9][9]$")) # same result
# 
# tb %>% 
#   filter((grepl(".(99)$", debit)))


# weekday----
tb %>% 
  mutate(weekday = fct_relevel(weekday, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) %>% 
  group_by(weekday, month) %>% 
  summarise(trans = n()) %>% 
  ggplot(aes(month, weekday, fill = trans)) +
  geom_tile() + # geom_raster()
  scale_fill_gradient2(low = "#EDD96E", mid = "#FFFFFF", high = "#7A002D", midpoint = 200) + 
  coord_equal() +
  theme_light() +
  theme(panel.grid = element_blank()) +
  labs(title = "JV posts on weekends", 
       caption = "RAudit Solution LLP | Stewart Li",
       x = "", 
       y ="", 
       fill = "")

tb %>% 
  filter(weekday %in% c("Sat", "Sun")) %>% 
  count(type, account, sort = TRUE) %>%
  top_n(10)


# appendix----

#' jv amount
tb %>% 
  filter(debit != 0 | credit != 0) %>% 
  group_by(debit, credit) %>% 
  filter(n() > 50) %>% 
  group_by(account, subaccount, debit, credit) %>% 
  summarise(n()) # same amt appears more than 50 times








