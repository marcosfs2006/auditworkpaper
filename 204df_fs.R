source("001package.R")
source("202df_tb.R")


# control total----
tb %>% 
  summarise_at(vars(debit, credit), sum) 


# completeness----
yr_fs <- tb %>% 
  group_by(subaccount) %>% 
  summarise_at(vars(debit, credit), sum) 


# monthly accumulated fs----
mth_fs <- tb %>% 
  group_by(subaccount, month) %>%
  arrange(date) %>% 
  slice(n()) %>%
  ungroup() %>% 
  select(subaccount, month, balance) %>% 
  pivot_wider(names_from = month, values_from = balance) %>% 
  select(subaccount, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec) %>% 
  replace(is.na(.), 0) # no prior year and market value for the calculation of mscore and zscore 
  # mutate_all(~(replace_na(.,0))) 
  

# monthly sales----
mth_fs %>% 
  filter(subaccount == "Revenue") %>% 
  gather(month, tcredit) %>% 
  rename(Accumlated_sales = tcredit)

mth_sales <- df_ar %>% 
  filter(subaccount == "Revenue") %>% 
  group_by(month) %>% 
  summarise(monthly_sales = sum(credit)) %>% 
  janitor::adorn_totals()

# identical(FS, Sales)


# wow comparison----
#+
wow_sales <- df_ar %>% 
  filter(subaccount == "Revenue") %>% 
  mutate(weekstarting  = floor_date(date, unit = "weeks")) %>% 
  group_by(weekstarting, name) %>% 
  summarise(weeklysales = sum(credit)) %>% 
  arrange(weekstarting, name) %>% 
  ungroup() %>% 
  group_by(name) %>% 
  mutate(mom = (weeklysales - lag(weeklysales)) / lag(weeklysales),
         mom = round(mom * 100, 1),
         yoy = (weeklysales - lag(weeklysales , 52)) / lag(weeklysales, 52),
         yoy = round(yoy * 100, 1))


#+
mom_sales <- df_ar %>% 
  filter(subaccount == "Revenue") %>% 
  mutate(yearmonth = format(date, "%Y-%m")) %>% 
  group_by(yearmonth, name) %>% 
  summarise(monthlysales = sum(credit)) %>% 
  arrange(yearmonth, name) %>% 
  ungroup() %>% 
  group_by(name) %>% 
  mutate(mom = (monthlysales - lag(monthlysales)) / lag(monthlysales),
         mom = round(mom * 100, 1),
         yoy = (monthlysales - lag(monthlysales , 12)) / lag(monthlysales, 12),
         yoy = round(yoy * 100, 1))

mom_sales %>% 
  filter(!is.na(mom)) %>% 
  select(-yoy) %>%  # compare to prior months (not direct prior months)
  arrange(desc(abs(mom)))

mom_sales %>% 
  ggplot(aes(yearmonth, mom, fill = name)) +
  geom_col(position = "dodge", show.legend = FALSE) +
  geom_text(aes(yearmonth, mom, label = name, angle = 45), check_overlap = TRUE, size = 3, hjust = 0.4, vjust = 0) +
  expand_limits(y = 500) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90)) # not pretty due to the data



  







