### try to figure out the combing attributes.
text = "net profit/total assets/total liabilities/total assets/working capital/total assets/current assets/short-term liabilities/cash/short-term securities/receivables/short-term liabilities/operating expenses/depreciation/retained earnings/total assets/EBIT/total assets/book value of equity/total liabilities/sales/total assets/equity/total assets/gross profit/extraordinary items/financial expenses/total assets/gross profit/short-term liabilities/gross profit/depreciation/sales/gross profit/interest/total assets/total liabilities/gross profit/depreciation/gross profit/depreciation/total liabilities/total assets/total liabilities/gross profit/total assets/gross profit/sales/inventory/sales/sales/sales/profit on operating activities/total assets/net profit/sales/gross profit/total assets/equity/share capital/total assets/net profit/depreciation/total liabilities/profit on operating activities/financial expenses/working capital/fixed assets/logarithm of total assets/total liabilities/cash/sales/gross profit/interest/sales/current liabilities/cost of products sold/operating expenses/short-term liabilities/operating expenses/total liabilities/profit on sales/total assets/total sales/total assets/current assets/inventories/long-term liabilities/constant capital/total assets/profit on sales/sales/current assets/inventory/receivables/short-term liabilities/total liabilities/profit on operating activities/depreciation/profit on operating activities/sales/rotation receivables/inventory turnover in days/receivables/sales/net profit/inventory/current assets/inventory/short-term liabilities/inventory/cost of products sold/EBITDA/depreciation/total assets/EBITDA/depreciation/sales/current assets/total liabilities/short-term liabilities/total assets/short-term liabilities/cost of products sold/equity/fixed assets/constant capital/fixed assets/working capital/sales/cost of products sold/sales/current assets/inventory/short-term liabilities/sales/gross profit/depreciation/total costs/total sales/long-term liabilities/equity/sales/inventory/sales/receivables/short-term liabilities/sales/sales/short-term liabilities/sales/fixed assets"

library(tm)

text = gsub("\t", "", text)
text = gsub("\n", "", text)
text_split = strsplit(text, "/")
unique(text_split[[1]])
unique_term = data.frame(term = unique(text_split[[1]]), count = 0)

for(j in 1:length(unique(text_split[[1]]))){
  for(i in 1:length(text_split[[1]])){
    if(text_split[[1]][[i]] == unique(text_split[[1]])[[j]]){
      unique_term$count[[j]] = unique_term$count[[j]] + 1
      print(c(i,j))
    }
  }
}