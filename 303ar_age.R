source("001package.R")
source("202df_tb.R")


# reconcile sales to ar----

#+ 
df_join <- df_ar %>% 
  filter(subaccount == "Accounts Receivable") %>% 
  mutate(amt = debit,
         amt = ifelse(amt == 0, -credit, amt)) %>% 
  group_by(num) %>% 
  full_join(df_ar %>% 
              filter(subaccount == "Revenue") %>% 
              select(num, type, debit, credit) %>% 
              group_by(num) %>% 
              summarise(tdebit = sum(debit), 
                        tcredit = sum(credit), 
                        trans = n()), 
            by = c("num")) %>% 
  select(subaccount:split, amt, debit, credit, trans, tdebit, tcredit) %>%  
  ungroup()


#+ YE ar balance by customers 
df_join %>% 
  group_by(name, type) %>% 
  summarise_at(vars(tdebit, tcredit, trans, debit, credit), sum) %>% # reconcile sales to ar
  summarise_at(vars(debit, credit), sum) %>% 
  mutate(cf = debit - credit) %>% 
  filter(cf != 0)


#+ 
df_join %>% 
  group_by(name, type, num) %>% 
  summarise_at(vars(debit, credit), sum) %>%  
  rownames_to_column() %>% 
  mutate(status = ifelse(rowname %in% c(4, 7, 55, 96, 123, 172), "unpaid", "paid")) %>% 
  filter(status == "unpaid") # not automatic

df_ar %>% 
  filter(subaccount == "Accounts Receivable") %>% 
  group_by(name, type) %>% 
  summarise_at(vars(debit, credit), sum) %>% 
  mutate(ar = debit,
         ar = ifelse(ar == 0, -credit, ar)) %>% 
  select(-c(credit, debit)) %>% 
  spread(type, ar) %>% 
  ungroup() %>% 
  mutate_all(~(replace_na(.,0))) %>% 
  ggplot(aes(Invoice, Payment)) +
  geom_point() +
  geom_abline(slope = -1, col = "red", lty = 2) +
  scale_x_continuous(labels = comma_format()) +
  scale_y_continuous(labels = comma_format()) +
  labs(title = "Paid vs Unpaid invoices for FY2018") # not very useful


# trend----
df_ar %>% 
  group_by(subaccount, month) %>% 
  summarise_at(vars(debit, credit), sum) %>% 
  mutate(amt = case_when(subaccount == "Accounts Receivable" ~ debit - credit,
                         subaccount == "Revenue" ~ credit - debit)) %>% 
  ggplot(aes(month, amt, color = subaccount)) +
  geom_point(show.legend = FALSE) +
  geom_path(aes(group = subaccount), show.legend = FALSE) +
  geom_hline(yintercept = 0, lty = 2, col = "black", size = 1) +
  scale_y_continuous(breaks = seq(-30000, 80000, 10000), labels = comma_format()) +
  directlabels::geom_dl(aes(label = subaccount), method = list(dl.combine("last.points"), cex = 0.6)) +
  theme_light() +
  labs(title = "Monthly movement for FY2018", 
       caption = "RAudit Solution LLP | Stewart Li",
       x = "",
       y = "", 
       color = "") # advance payment


# appendix----

#' ar invoice/payment mean 
df_ar %>% 
  filter(subaccount == "Accounts Receivable") %>% 
  group_by(type) %>% 
  summarize(mean_debit = mean(debit),
            mean_credit = mean(credit),
            n = n()) %>%
  pivot_wider(names_from = type,
              values_from = c(mean_debit, mean_credit, n))


#' good customers
d15p <- df_ar %>% 
  filter(subaccount == "Accounts Receivable") %>% 
  arrange(desc(credit)) %>% 
  filter(credit > quantile(credit, prob = .85)) # not sure it is a correct approach?

c15p <- df_ar %>% 
  filter(subaccount == "Accounts Receivable") %>% 
  arrange(desc(debit)) %>% 
  head(nrow(d15p))

goodcustomers <- intersect(d15p$name, c15p$name)




