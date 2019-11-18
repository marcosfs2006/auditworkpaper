source("001package.R")
source("202df_tb.R")


# duplication jv numbers---- 
d <- df_ar %>% filter(subaccount == "Revenue") 

n_occur <- data.frame(table(d$num))
# d[duplicated(d$num), ] 

d[d$num %in% n_occur$Var1[n_occur$Freq > 1], ] %>% 
  group_by(num) %>% 
  summarise(n())

d %>% 
  filter(num == "71047") 


# SSS----
df_ar %>% 
  filter(subaccount == "Revenue") %>% 
  group_by(num, date, name, credit) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq))

df_ar %>% 
  filter(subaccount == "Revenue") %>% 
  filter(num == "71052", credit == 210) # different memo (ok)


# gap----
df_ar %>% 
  filter(subaccount == "Revenue") %>% 
  arrange(num) %>%
  distinct(num, .keep_all = TRUE) %>% 
  mutate(gap = as.numeric(num) - lag(as.numeric(num))) %>% 
  filter(gap > 1)


# Benford law----
bfd.cp <- benford((df_ar %>% filter(subaccount == "Revenue"))$credit, 
                  number.of.digits = 2, 
                  sign = "both", round = 3) 

plot(bfd.cp) 

getSuspects(bfd.cp, (df_ar %>% filter(subaccount == "Revenue")), 
            by="absolute.diff", how.many = 1) %>% 
  as_tibble()


# NDT----
df_ar %>% 
  filter(subaccount == "Revenue", 
         credit >= 10) %>% 
  group_by(credit) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq)) %>% 
  mutate(first_two = sapply(credit, function(x) substring(x, first=c(1), last=c(2)))) %>% # end of NDT (75)
  count(first_two, sort = TRUE) # conform to benford based on frequency


# weekday calender----
#+ table by freq
with((df_ar %>% filter(subaccount == "Revenue")), table(month, weekday)) %>% addmargins()


#+ monthly sales proporton by freq
df_ar %>% 
  filter(subaccount == "Revenue") %>% 
  .$month %>% 
  table()

df_ar %>% 
  ggplot(aes(month)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  theme_light() + 
  # theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "Sales proportion by month", 
       caption = "RAudit Solution LLP | Stewart Li",
       x = "")


#+ calender by freq
df_ar %>%
  filter(subaccount == "Revenue") %>% 
  group_by(month, weekday) %>%
  count() %>% 
  ggplot(aes(month, weekday, fill = n)) +
  geom_tile(color = "#1D2024", size = 0.5, stat = "identity") +
  scale_fill_viridis_c(option = "B") +
  coord_equal() +
  labs(x = "", 
       y = "") +
  theme(plot.background = element_rect(fill = "#1D2024", color = "#1D2024"),
        panel.background = element_rect(fill = "#1D2024", color = "#1D2024"),
        legend.background = element_rect(fill = "#1D2024", color = "#1D2024"),
        text = element_text(color = "#FAFAFA"),
        axis.text = element_text(color = "#FAFAFA"),
        axis.text.x = element_text(vjust = 1),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right",
        legend.title = element_blank())


# frequency vs value----
df_ar %>%
  filter(subaccount == "Revenue") %>% 
  mutate(weekday = fct_relevel(weekday, rev(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))) %>%
  group_by(weekday, month) %>%
  summarise(freq = n(),
            amt = sum(credit)) %>% 
  gather(var, value, -c(weekday, month)) %>%
  ggplot(aes(month, value)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) + # unit_format(unit = "K")
  facet_grid(weekday ~ var, scales = "free_x") +
  theme_light() +
  labs(title = "Sales Amount vs Frequency",
       x = "",
       y = "")


# appendix----
rmarkdown::pandoc_version() 












