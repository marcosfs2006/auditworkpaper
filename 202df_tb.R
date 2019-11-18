source("001package.R")


# clean tb----
tb <- read_csv("./data/gl.csv") %>% 
  rename(id = X1) %>% 
  select(-adj) %>% 
  filter(!is.na(type)) %>% 
  mutate(weekday = wday(date, label = TRUE),
         month = month(date, label = TRUE))


# df_ar dataset----
ar <- c("Accounts Receivable", "Revenue")

df_ar <- tb %>% 
  filter(subaccount %in% ar)


# data dictionary----
dd <- tribble(~Original, ~Definition, ~Rename,
        "", "Row number" , "id",
        "account", "Charter of Accounts", "account",
        "subaccount", "Charter of Accounts", "subaccount",
        "type", "Invoice/Payment", "type",
        "date", "JV posting date", "date",
        "num", "JV number", "num",
        "adj", "", "adj",
        "name", "Customers/Suppliers", "name",
        "memo", "JV description", "memo",
        "split", "Double entries", "split",
        "debit", "JV amount", "debit", 
        "credit", "JV amount", "credit", 
        "balance", "Cumulated JV amount", "balance", 
        "weekday", "Mutated variable", "weekday", 
        "month", "Mutated variable", "month") %>% 
  DT::datatable(rownames = F, options = list(paging= T))









