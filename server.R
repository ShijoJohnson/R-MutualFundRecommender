library(shiny)
#library("recommenderlab")
library("Matrix")
library(reshape2)
library(DT)

## Reading in Data ---------------------------------------------------------------------
mf_df <- read.csv("Mutual_Funds.csv")

#server.R

shinyServer(
  
  function(input, output, session){
    observeEvent(input$submit, {
      print('testa')
      print('testb')
      
    })
    
  }
)


shinyServer(function(input, output) {
  
  formulaText1 <- reactive({
    paste(input$input_item)
  })

    output$fund1 <- renderText({
    formulaText1()
  })

  # Table containing recommendations
  output$table <- DT::renderDataTable({
    
    print('test0')
    
fund_recommendation <- function(input1){
  row_num <- which(mf_df[,2] == input1)
  

  mf_df$fund_yield_bucket <- as.numeric(cut(mf_df$fund_yield, 30))
  mf_df$net_assets_bucket <- as.numeric(cut(mf_df$net_assets, 30))
  mf_df$net_annual_expense_ratio_fund_bucket <- as.numeric(cut(mf_df$net_annual_expense_ratio_fund, 30))
  # mf_df$net_annual_expense_ratio_category_bucket <- as.numeric(cut(mf_df$net_annual_expense_ratio_category, 5))
  mf_df$portfolio_cash_bucket <- as.numeric(cut(mf_df$portfolio_cash, 30))
  mf_df$portfolio_stocks_bucket <- as.numeric(cut(mf_df$portfolio_stocks, 30))
  mf_df$portfolio_bonds_bucket <- as.numeric(cut(mf_df$portfolio_bonds, 30))
  mf_df$portfolio_others_bucket <- as.numeric(cut(mf_df$portfolio_others, 30))
  mf_df$portfolio_preferred_bucket <- as.numeric(cut(mf_df$portfolio_preferred, 30))
  mf_df$portfolio_convertable_bucket <- as.numeric(cut(mf_df$portfolio_convertable, 30))
  mf_df$ytd_return_bucket <- as.numeric(cut(mf_df$ytd_return, 30))
  mf_df$fund_return_ytd_bucket <- as.numeric(cut(mf_df$fund_return_ytd, 30))
  mf_df$fund_return_1month_bucket <- as.numeric(cut(mf_df$fund_return_1month, 30))
  mf_df$fund_return_1year_bucket <- as.numeric(cut(mf_df$fund_return_1year, 30))
  mf_df$fund_return_3years_bucket <- as.numeric(cut(mf_df$fund_return_3years, 30))
  mf_df$fund_return_5years_bucket <- as.numeric(cut(mf_df$fund_return_5years, 30))
  mf_df$fund_return_10years_bucket <- as.numeric(cut(mf_df$fund_return_10years, 30))
  # mf_df$price_book_bucket <- as.numeric(cut(mf_df$price_book, 5))
  # mf_df$basic_materials_bucket <- as.numeric(cut(mf_df$basic_materials, 5))
  # mf_df$consumer_cyclical_bucket <- as.numeric(cut(mf_df$consumer_cyclical, 5))
  # mf_df$financial_services_bucket <- as.numeric(cut(mf_df$financial_services, 5))
  # mf_df$real_estate_bucket <- as.numeric(cut(mf_df$real_estate, 5))
  # mf_df$consumer_defensive_bucket <- as.numeric(cut(mf_df$consumer_defensive, 5))
  # mf_df$healthcare_bucket <- as.numeric(cut(mf_df$healthcare, 5))
  # mf_df$utilities_bucket <- as.numeric(cut(mf_df$utilities, 5))
  # mf_df$communication_services_bucket <- as.numeric(cut(mf_df$communication_services, 5))
  # mf_df$energy_bucket <- as.numeric(cut(mf_df$energy, 5))
  # mf_df$industrials_bucket <- as.numeric(cut(mf_df$industrials, 5))
  # mf_df$technology_bucket <- as.numeric(cut(mf_df$technology, 5))

  
#  mf_df <- mf_df[1:150,c(2,7,47,81,126:154)]
  #print(colnames(mf_df))
  mf_df <- mf_df[,c(2,7,47,81,126:141)]  
  ncol(mf_df)
#  mf_df_colnames<-colnames(mf_df)
  print("test1")
  
  
  mf_df$cosine_sim_calc <- 0
#  for (i in 1:150){
  for (i in 1:nrow(mf_df)){
    mf_df[i,"cosine_sim_calc"] <- round(lsa::cosine(as.numeric(mf_df[i,-1]), as.numeric(mf_df[row_num,-1])), digits = 7)
  }
  
  #View(mf_df)
  mf_df$cosine_sim_calc <- as.double(mf_df$cosine_sim_calc)
#  mf_df %>%
  #    #filter(song_id != song_code) %>% # remove self reference
  #  arrange(desc(cosine_sim_calc)) %>%
  #  top_n(5, cosine_sim_calc) %>%
  #    select(fund_extended_name)

#  return(mf_df[row_num,2])
  
  return(mf_df %>%
    #filter(song_id != song_code) %>% # remove self reference
    arrange(desc(round(cosine_sim_calc,7))) %>%
    filter(cosine_sim_calc < 0.9935894)  %>%
    top_n(25, round(cosine_sim_calc,7)) %>%
#    select(fund_extended_name, cosine_sim_calc))
    mutate(fund_extended_name, round(cosine_sim_calc,7), fund_yield_bucket, net_assets_bucket, net_annual_expense_ratio_fund_bucket,portfolio_cash_bucket, portfolio_stocks_bucket, portfolio_bonds_bucket, portfolio_others_bucket, portfolio_preferred_bucket, portfolio_convertable_bucket, ytd_return_bucket, fund_return_ytd_bucket, fund_return_1month_bucket, 
         fund_return_1year_bucket, fund_return_3years_bucket, fund_return_5years_bucket, fund_return_10years_bucket))  
#      print(row_num)
#      print('test1a')
#      print(length(unique(animes$anime_id)))
#      userSelect <- matrix(NA,length(unique(ar_select$anime_id)))
#      userSelect[row_num] <- 5 #hard code first selection to rating 5
#      userSelect[row_num2] <- 4 #hard code second selection to rating 4
#      userSelect[row_num3] <- 4 #hard code third selection to rating 4
#      userSelect <- t(userSelect)
#      print('test2')
#      ratingmat <- dcast(ar_select, user_id~anime_id, value.var = "rating", na.rm=FALSE)
#      ratingmat <- ratingmat[,-1]
#      colnames(userSelect) <- colnames(ratingmat)
 #     print('test3')
      #Convert rating matrix into a sparse matrix

    }
    







    fund_recommendation(input$input_item)
    
  })
  

}
)
