source("001package.R")
source("202df_tb.R")


# all customers----
length(unique(df_ar$name))


#+
df_ar %>% 
  filter(subaccount == "Revenue") %>% 
  janitor::tabyl(name, sort = TRUE)

df_ar %>% 
  filter(subaccount == "Revenue") %>% 
  table1::table1(~name | month, data = .)


#+
df_ar %>% 
  filter(subaccount %in% c("Revenue")) %>%
  group_by(name, month) %>% 
  summarise_at(vars(debit, credit), sum) %>% 
  mutate(amount = credit - debit) %>% 
  ggplot(aes(month, amount, fill = name)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma_format()) +
  theme_light() +
  theme(legend.position  = "none") +
  labs(title = "Total monthly sales for FY2018",
       subtitle = "Breakdown by customers",
       caption = "RAudit Solution LLP | Stewart Li",
       x = "",
       y = "") # not useful


# cumulative sales----
df_ar %>% 
  filter(subaccount == "Revenue") %>% 
  group_by(name, month) %>% 
  summarise(freq = n(),
            credit = sum(credit)) %>% 
  mutate(pct = credit / sum(credit),
         cum_amt = cumsum(credit),
         cum_pct = cumsum(pct)) 


# RSF----
df_ar %>% 
  filter(subaccount == "Revenue") %>% 
  group_by(name) %>% 
  arrange(desc(credit)) %>% 
  slice(1:2) %>% 
  mutate(rsf = credit / lag(credit)) %>% 
  filter(rsf >= 0.6)  # dataset by name


#+ slice
df_ar %>% 
  filter(subaccount == "Revenue") %>% 
  group_by(name) %>% 
  mutate(sales_invrease = case_when(credit > lag(credit, 1) ~ 1, 
                                    TRUE ~ 0)) %>% 
  arrange(date) %>% 
  slice(seq_len(min(which(sales_invrease == 0)))) # first decrease in sales by customer

df_ar %>% 
  filter(subaccount == "Revenue") %>% 
  arrange(desc(credit)) %>% 
  slice(c(1:2, (n()-1):n())) # large vs small, cut-off etc.

df_ar %>% 
  filter(subaccount == "Revenue") %>% 
  group_by(month) %>% 
  arrange(desc(credit)) %>% 
  slice(c(1:2, (n()-1):n())) 

df_ar %>% 
  filter(subaccount == "Revenue") %>% 
  group_by(name, month) %>% 
  arrange(desc(credit)) %>% 
  slice(c(1:2, (n()-1):n())) 


# top 10 customers----
# https://stackoverflow.com/questions/57911389/how-to-change-font-size-in-a-formattable-table-in-r
df_ar %>%
  filter(subaccount == "Revenue") %>% 
  group_by(name = fct_lump(name, 10, w = credit)) %>% # top_n
  summarise(amt = sum(credit),
            freq = n()) %>%  
  arrange(desc(amt)) %>%  
  mutate(pct = round(amt / sum(amt), 2),
         pct = percent(pct, "d", digits = 0L),
         amt = currency(amt, "$", digits = 0L)) %>% # others 40%
  formattable(list(amt = color_bar("#FA614B"),
                   freq = color_bar("pink"),
                   pct = color_bar("lightpink"))) 


#+
df_ar %>% 
  filter(account %in% c("Revenue")) %>%
  group_by(name) %>% 
  summarise_at(vars(debit, credit), sum) %>% 
  mutate(amount = credit - debit) %>% 
  arrange(desc(amount)) %>% 
  top_n(10) %>% 
  mutate(name = fct_reorder(name, amount)) %>% 
  ggplot(aes(name, amount)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  geom_text(aes(label = amount), check_overlap = TRUE, hjust = 1.5, size = 4, col = "pink") +
  scale_y_continuous(labels = comma_format(), expand = c(0,0)) +
  theme_light() +
  labs(title = "Top 10 customers for FY2018", 
       subtitle = "Based on sales amount",
       caption = "RAudit Solution LLP | Stewart Li",
       x = "",
       y = "") # not better than formattable


# RFM---- 
df_ar %>% 
  filter(subaccount == "Revenue") %>% 
  select(date, name, credit) %>% 
  mutate(date = ceiling_date(date, "day")) %>% 
  group_by(name) %>% 
  mutate(revenue = sum(credit),
         last_visit = last(date),
         last_days =  as.double(as.Date("2018-12-31") - last_visit),
         orders = n()) %>% 
  select(-c(date, credit)) %>% 
  distinct(last_visit, .keep_all = TRUE) %>% 
  ggplot(aes(last_days, orders, size = revenue)) +
  geom_point() +
  geom_text(aes(label = paste0 (name, " | ", last_visit)), 
            hjust = 0, vjust = 1, 
            check_overlap = TRUE, size = 3, col = "steelblue") +
  scale_x_continuous(labels = function(x) paste0(x, "/days")) +
  scale_y_continuous(labels = function(x) paste0(x, "/orders")) +
  scale_size_continuous(name = "Revenue", labels = comma_format()) +
  theme_minimal() +
  theme(legend.justification=c(1,1),
        legend.position = c(0.95, 0.95),
        legend.background = element_blank()) +
  labs(title = "Customer churn analysis as at 2018/12/31", 
       subtitle = "Based on RFM",
       caption = "RAudit Solution LLP | Stewart Li",
       x = "",
       y = "")


# sales outliers----
z <- df_ar %>% 
  filter(subaccount == "Revenue") %>% 
  mutate(z = ifelse(abs(scale(credit)) > 1.96, "z", "ok")) %>% 
  filter(z == "z")
  
df_ar %>% 
  filter(subaccount == "Revenue") %>% 
  ggplot() +
  geom_point(aes(credit, date, size = credit > 6000)) +
  geom_point(data = z, aes(credit, date, color = "red")) +
  geom_vline(xintercept = 3000, lty = 2, col = "red", size = 1) +
  geom_text(data = subset(df_ar %>% filter(subaccount == "Revenue"), credit > 3000),
            aes(credit, date, label = name), 
            vjust = 1,
            hjust = .1,
            check_overlap = TRUE, 
            col = "steelblue",
            size = 3) +
  scale_colour_manual(values = c("red"), guide = FALSE) +
  scale_size_manual(values = c(1, 3), guide = FALSE) +
  scale_x_continuous(labels = scales::comma_format()) +
  # scale_y_date(date_breaks = "1 week", date_labels = "%W") +
  annotate("text", x = 3300, y = as.Date("2019-01-06"), label = "3,000", size = 3) +
  theme(legend.position = "none") +
  theme_minimal() + 
  labs(title = "Identification of sales outliers for FY2018",
       subtitle = "Based on ML and Z score",
       caption = "RAudit Solution LLP | Stewart Li",
       x = "",
       y = "") 






