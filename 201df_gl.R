source("001package.R")


# clean gl----
cells <- xlsx_cells("data/gl_stewart.xlsx") %>%
  filter(!is_blank) %>%
  select(row, col, data_type, numeric, date, character)

gl <- cells %>%
  behead("N", "field1") %>%
  select(-col) %>%
  spatter(field1) %>%
  select(-row) %>%
  mutate(account = coalesce(account, bf),
         subaccount = coalesce(subaccount, account)) %>%
  fill(account, .direction = "down") %>%
  fill(subaccount, .direction = "down") %>%
  select(account, subaccount, Type, Date, Num, Adj, Name, Memo, Split, Debit, Credit, Balance) %>%
  mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
  janitor::clean_names() %>%
  replace_na(list(debit = 0, credit = 0, balance = 0))

write.csv(gl, "./data/gl.csv")







