source("001package.R")
source("202df_tb.R")


# pl dataset----
#+
unique(tb$account)[17:45]
pl <- unique(tb$subaccount)[19:62] %>% dput


#+
df_pl <-tb %>% 
  filter(subaccount %in% pl, 
         !subaccount %in% c("Mileage", "Employee Bonus", "Sick/Holiday & Vacation Pay")) %>% # no levels
  group_by(date, subaccount) %>% 
  summarise_at(vars(debit, credit), sum) %>% # daily summarized data***
  mutate(amt = abs(credit - debit)) %>% 
  select(date, subaccount, amt) %>% 
  pivot_wider(names_from = subaccount, values_from = amt) %>% 
  replace(is.na(.), 0) %>% 
  select(date, Revenue, everything()) %>% 
  ungroup() %>% 
  janitor::clean_names()


#+ levels 0
# length(which(df_pl$revenue == 0))
# sapply(df_pl, function(x) length(which(x == 0)))
# 
# apply(df_pl, 2, function(x) length(unique(x))) # find duplication amt
# apply(df_pl[2:42], 2, function(x) length(as.data.frame(table(x))$Freq) > 12)
# 
# apply(df_pl[2:42], 2, function(x) round(sum(x == 0)/nrow(df_pl), 2))


#+ check df_pl
# setdiff(names(df_pl), pl)
# plot(df_pl$revenue, df_pl$purchases_cost_of_goods)

# tb %>% 
#   filter(subaccount %in% c("revenue", "purchases_cost_of_goods", "payroll_tax_expenses")) %>% 
#   group_by(subaccount) %>% 
#   summarise(sum(debit), sum(credit))
# 
# df_pl %>% 
#   select(revenue, purchases_cost_of_goods, `payroll_tax_expenses`) %>% 
#   colSums()


# model dataset----
#+
cor(df_pl[2:42]) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  select(c(1:2, 5)) %>% 
  arrange(desc(abs(revenue))) %>% 
  slice(1:12) %>% 
  pluck(1) %>% 
  dput()


#+
df_model <- df_pl %>% 
  select(c("revenue", "purchases_cost_of_goods", "wages_sales_inside", 
           "payroll_tax_expenses", "wages_office_staff", "wages_warehouse", 
           "conferences_and_seminars", "supplies", "dues_and_subscriptions", 
           "interest_expense", "maintenance_janitorial", "accounting_fees"))







