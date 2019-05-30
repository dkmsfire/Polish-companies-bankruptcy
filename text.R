### try to figure out the combing attributes.
library(tm)
text = "net profit/total assets/total liabilities/total assets/working capital/total assets/current assets/short-term liabilities/cash/short-term securities/receivables/short-term liabilities/operating expenses/depreciation/retained earnings/total assets/EBIT/total assets/book value of equity/total liabilities/sales/total assets/equity/total assets/gross profit/extraordinary items/financial expenses/total assets/gross profit/short-term liabilities/gross profit/depreciation/sales/gross profit/interest/total assets/total liabilities/gross profit/depreciation/gross profit/depreciation/total liabilities/total assets/total liabilities/gross profit/total assets/gross profit/sales/inventory/sales/sales/sales/profit on operating activities/total assets/net profit/sales/gross profit/total assets/equity/share capital/total assets/net profit/depreciation/total liabilities/profit on operating activities/financial expenses/working capital/fixed assets/logarithm of total assets/total liabilities/cash/sales/gross profit/interest/sales/current liabilities/cost of products sold/operating expenses/short-term liabilities/operating expenses/total liabilities/profit on sales/total assets/total sales/total assets/current assets/inventories/long-term liabilities/constant capital/total assets/profit on sales/sales/current assets/inventory/receivables/short-term liabilities/total liabilities/profit on operating activities/depreciation/profit on operating activities/sales/rotation receivables/inventory turnover in days/receivables/sales/net profit/inventory/current assets/inventory/short-term liabilities/inventory/cost of products sold/EBITDA/depreciation/total assets/EBITDA/depreciation/sales/current assets/total liabilities/short-term liabilities/total assets/short-term liabilities/cost of products sold/equity/fixed assets/constant capital/fixed assets/working capital/sales/cost of products sold/sales/current assets/inventory/short-term liabilities/sales/gross profit/depreciation/total costs/total sales/long-term liabilities/equity/sales/inventory/sales/receivables/short-term liabilities/sales/sales/short-term liabilities/sales/fixed assets"
text_split = strsplit(text, "/")
term_unique = unique(text_split[[1]])
unique_term = data.frame(term = unique(text_split[[1]]), count = 0)

for(j in 1:length(unique(text_split[[1]]))){
  for(i in 1:length(text_split[[1]])){
    if(text_split[[1]][[i]] == unique(text_split[[1]])[[j]]){
      unique_term$count[[j]] = unique_term$count[[j]] + 1
      print(c(i,j))
    }
  }
}

text_all = "X1	net profit / total assets 
X2	total liabilities / total assets 
X3	working capital / total assets 
X4	current assets / short-term liabilities 
X5	[(cash + short-term securities + receivables - short-term liabilities) / (operating expenses - depreciation)] * 365 
X6	retained earnings / total assets 
X7	EBIT / total assets 
X8	book value of equity / total liabilities 
X9	sales / total assets 
X10	equity / total assets 
X11	(gross profit + extraordinary items + financial expenses) / total assets 
X12	gross profit / short-term liabilities 
X13	(gross profit + depreciation) / sales 
X14	(gross profit + interest) / total assets 
X15	(total liabilities * 365) / (gross profit + depreciation) 
X16	(gross profit + depreciation) / total liabilities 
X17	total assets / total liabilities 
X18	gross profit / total assets 
X19	gross profit / sales 
X20	(inventory * 365) / sales 
X21	sales (n) / sales (n-1) 
X22	profit on operating activities / total assets 
X23	net profit / sales 
X24	gross profit (in 3 years) / total assets 
X25	(equity - share capital) / total assets 
X26	(net profit + depreciation) / total liabilities 
X27	profit on operating activities / financial expenses 
X28	working capital / fixed assets 
X29	logarithm of total assets 
X30	(total liabilities - cash) / sales 
X31	(gross profit + interest) / sales 
X32	(current liabilities * 365) / cost of products sold 
X33	operating expenses / short-term liabilities 
X34	operating expenses / total liabilities 
X35	profit on sales / total assets 
X36	total sales / total assets 
X37	(current assets - inventories) / long-term liabilities 
X38	constant capital / total assets 
X39	profit on sales / sales 
X40	(current assets - inventory - receivables) / short-term liabilities 
X41	total liabilities / ((profit on operating activities + depreciation) * (12/365)) 
X42	profit on operating activities / sales 
X43	rotation receivables + inventory turnover in days 
X44	(receivables * 365) / sales 
X45	net profit / inventory 
X46	(current assets - inventory) / short-term liabilities 
X47	(inventory * 365) / cost of products sold 
X48	EBITDA (profit on operating activities - depreciation) / total assets 
X49	EBITDA (profit on operating activities - depreciation) / sales 
X50	current assets / total liabilities 
X51	short-term liabilities / total assets 
X52	(short-term liabilities * 365) / cost of products sold) 
X53	equity / fixed assets 
X54	constant capital / fixed assets 
X55	working capital 
X56	(sales - cost of products sold) / sales 
X57	(current assets - inventory - short-term liabilities) / (sales - gross profit - depreciation) 
X58	total costs /total sales 
X59	long-term liabilities / equity 
X60	sales / inventory 
X61	sales / receivables 
X62	(short-term liabilities *365) / sales 
X63	sales / short-term liabilities 
X64	sales / fixed assets"
text_all = gsub("\t", "", text_all)
text_all = gsub("\n", "", text_all)

text_all_split = strsplit(text_all, "X")[[1]][2:65]

attribute_detector = matrix(0L, nrow = 65, ncol = 37)
colnames(attribute_detector) = c(term_unique, "All")
rownames(attribute_detector) = c(c(1:64), "All")

for(i in 1:length(text_all_split)){
  for(j in 1:length(term_unique)){
    if(grepl(term_unique[[j]], text_all_split[[i]])){
      attribute_detector[i,j] = 1
    }
  }
}

for(i in 1:length(text_all_split)){
  attribute_detector[i,37] = sum(attribute_detector[i,])
}
for(j in 1:length(term_unique)){
  attribute_detector[65,j] = sum(attribute_detector[,j])
}

attribute_detector[65,37] = sum(attribute_detector[1:64,1:36])
