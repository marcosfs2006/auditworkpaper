source("001package.R")
source("202df_tb.R")


# no. of accounts----
length(unique(tb$account))
length(unique(tb$subaccount))


# treemap----
coa <- tb %>% 
  group_by(account, subaccount) %>% 
  summarise(record = n()) %>% 
  ungroup() 

treemap::treemap(coa,
                 index=c("account", "subaccount"),
                 vSize="record",
                 vColor="record",
                 type="value") # useful for other categorical vars


# datatree----
library(data.tree)

coa_datatree <- coa %>% 
  mutate(pathString = paste("COA", account, subaccount, sep = "||")) %>% 
  as.data.frame() %>% 
  as.Node(pathDelimiter = "||") # https://rdrr.io/cran/data.tree/man/as.Node.data.frame.html

print(coa_datatree, pruneMethod = "simple", "record", limit = 20)
print(coa_datatree, pruneMethod = "dist", limit = 20)


# network----
library(networkD3)

acmeNetwork <- ToDataFrameNetwork(coa_datatree, "subaccount")
simpleNetwork(acmeNetwork[-3], fontSize = 12)

useRtreeList <- ToListExplicit(coa_datatree, unname = TRUE)
radialNetwork(useRtreeList) # I prefer this


# collapsibleTree----
library(collapsibleTree)

tb %>% 
  collapsibleTree(hierarchy = c("account", "subaccount", "name"),
                  width = 800,
                  zoomable = TRUE) # I prefer this

tb %>% 
  group_by(account, subaccount) %>% 
  summarise(volumn = n()) %>% 
  collapsibleTreeSummary(
    hierarchy = c("account", "subaccount"),
    root = "GL",
    width = 800,
    attribute = "volumn",
    zoomable = FALSE)









