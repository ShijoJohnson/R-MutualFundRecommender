library(tidyverse)
my_mfs <- read.csv("Mutual_Funds.csv")
head(my_mfs)

my_mfs$fund_yield_bucket <- as.numeric(cut(my_mfs$fund_yield, 5))
#my_mfs$net_assets_bucket <- as.numeric(cut(my_mfs$net_assets, 5))
#my_mfs$net_annual_expense_ratio_fund_bucket <- as.numeric(cut(my_mfs$net_annual_expense_ratio_fund, 5))
#my_mfs$net_annual_expense_ratio_category_bucket <- as.numeric(cut(my_mfs$net_annual_expense_ratio_category, 5))
my_mfs$portfolio_cash_bucket <- as.numeric(cut(my_mfs$portfolio_cash, 5))
my_mfs$portfolio_stocks_bucket <- as.numeric(cut(my_mfs$portfolio_stocks, 5))
my_mfs$portfolio_bonds_bucket <- as.numeric(cut(my_mfs$portfolio_bonds, 5))
my_mfs$portfolio_others_bucket <- as.numeric(cut(my_mfs$portfolio_others, 5))
my_mfs$portfolio_preferred_bucket <- as.numeric(cut(my_mfs$portfolio_preferred, 5))
my_mfs$portfolio_convertable_bucket <- as.numeric(cut(my_mfs$portfolio_convertable, 5))
#my_mfs$price_book_bucket <- as.numeric(cut(my_mfs$price_book, 5))
#my_mfs$price_cashflow <- as.numeric(cut(my_mfs$price_cashflow, 5))
#my_mfs$median_market_cap <- as.numeric(cut(my_mfs$median_market_cap, 5))
#my_mfs$basic_materials_bucket <- as.numeric(cut(my_mfs$basic_materials, 5))
# my_mfs$consumer_cyclical_bucket <- as.numeric(cut(my_mfs$consumer_cyclical, 5))
# my_mfs$financial_services_bucket <- as.numeric(cut(my_mfs$financial_services, 5))
# my_mfs$real_estate_bucket <- as.numeric(cut(my_mfs$real_estate, 5))
# my_mfs$consumer_defensive_bucket <- as.numeric(cut(my_mfs$consumer_defensive, 5))
# my_mfs$healthcare_bucket <- as.numeric(cut(my_mfs$healthcare, 5))
# my_mfs$utilities_bucket <- as.numeric(cut(my_mfs$utilities, 5))
# my_mfs$communication_services_bucket <- as.numeric(cut(my_mfs$communication_services, 5))
# my_mfs$energy_bucket <- as.numeric(cut(my_mfs$energy, 5))
# my_mfs$industrials_bucket <- as.numeric(cut(my_mfs$industrials, 5))
# my_mfs$technology_bucket <- as.numeric(cut(my_mfs$technology, 5))
# my_mfs$ytd_return_bucket <- as.numeric(cut(my_mfs$ytd_return, 5))
my_mfs$fund_return_ytd_bucket <- as.numeric(cut(my_mfs$fund_return_ytd, 5))
my_mfs$fund_return_1month_bucket <- as.numeric(cut(my_mfs$fund_return_1month, 5))
my_mfs$fund_return_1year_bucket <- as.numeric(cut(my_mfs$fund_return_1year, 5))
my_mfs$fund_return_3years_bucket <- as.numeric(cut(my_mfs$fund_return_3years, 5))
my_mfs$fund_return_5years_bucket <- as.numeric(cut(my_mfs$fund_return_5years, 5))
my_mfs$fund_return_10years_bucket <- as.numeric(cut(my_mfs$fund_return_10years, 5))

View(my_mfs)
#View(mf_df)
ncol(my_mfs)
my_mfs <- my_mfs[1:150,c(2,7,47,81,126:138)]

cosine_sim <- function(a, b) crossprod(a,b)/sqrt(crossprod(a)*crossprod(b))
ncol(my_mfs)
my_mfs_colnames<-colnames(my_mfs)




#my_mfs_spread <- spread(my_mfs, fund_extended_name, my_mfs_colnames[-1], fill = 0)
my_mfs_spread <- spread(my_mfs, fund_extended_name, morningstar_rating, fill = 0)

head(my_mfs_spread)
my_mfs_colnames[-1]
head(my_mfs)







#my_mfs_mat <- as.matrix(my_mfs)
#head(my_mfs_mat)
#lsa::cosine(as.numeric(my_mfs_mat[1,-1]), as.numeric(my_mfs_mat[2,-1]))
#lsa::cosine(as.numeric(my_mfs_mat[4,-1]), as.numeric(my_mfs_mat[3,-1]))


my_mfs$cosine_sim_calc <- 0
for (i in 1:150){
  my_mfs[i,"cosine_sim_calc"] <- lsa::cosine(as.numeric(my_mfs[i,-1]), as.numeric(my_mfs[3,-1]))
}


my_mfs %>%
  #filter(song_id != song_code) %>% # remove self reference
  arrange(desc(cosine_sim_calc)) %>%
  top_n(5, cosine_sim_calc) %>%
  select(fund_extended_name)



#cos_sims <- apply(as.numeric(my_mfs_mat[,-1]), 1, lsa::cosine(as.numeric(my_mfs_mat[1,-1]), as.numeric(my_mfs_mat[3,-1])))
#cos_sims <- apply(as.numeric(my_mfs_mat[,-1]), 1, lsa::cosine(as.numeric(my_mfs_mat[3,-1]), as.numeric(my_mfs_mat[3,-1])))



#cosine_sim <- function(a, b) crossprod(a,b)/sqrt(crossprod(a)*crossprod(b))
#mf_col_index <- which(colnames(my_mfs_mat) == "Aberdeen Income Builder Fund Class A")
#cosine_sim(my_mfs_mat[,mf_col_index],y)



#subject <- c('English', 'Hindi', 'Punjabi', 'Maths', 'Science', 'Social Science', 'English', 'Hindi', 'Punjabi', 'Maths', 'Science', 'Social Science')
#marks <- as.character ( c('38/40', '40/40', '32/40', '29/40', '27/40', '35/40', '30/40', '33/40', '36/40', '31/40', '39/40', '34/40') )
#student <- c('Gurnoor', 'Gurnoor', 'Gurnoor', 'Gurnoor', 'Gurnoor', 'Gurnoor', 'Dhruv', 'Dhruv', 'Dhruv', 'Dhruv', 'Dhruv', 'Dhruv')


#reportCard_Nov18 <- data.frame(Student = student, Subject = subject, Marks = marks)
#head(reportCard_Nov18)

#reportCard_Nov18.wide <- reportCard_Nov18 %>% spread(Student, Marks)
#head(reportCard_Nov18.wide)


