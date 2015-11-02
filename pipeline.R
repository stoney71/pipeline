nt_port_codes <- c("TCM04", "TCM03", "TCA02", "TCM02")


get_clean_data <- function(filename) {
        ## load filename (which is a pipelince csv file) and remove
        ## unwanted data, such as mobiles & voice opps, lost opps and NT opps. 
        
        dataset <- read.csv(filename)
        
        ## remove MOBILES & VOICE
        dataset <- dataset[(dataset$Product.Group != "MOBILES") & 
                            (dataset$Product.Group != "VOICE") , ]
        
        ## remove NT opps
        dataset <- dataset[!(dataset$Portfolio.Code %in% nt_port_codes), ]
        
        ## only inclue Open status opps
        dataset <- dataset[(dataset$Product.Status == "Open"), ]
        
        ## add total revenue column (keep this step last)
        colnames(dataset)[which(names(dataset) == "New.Income.Revenue.1")] <-
                "Total.Revenue"
        start_rev_cols <- which(names(dataset) == "New.Income.Revenue")
        end_rev_cols <- which(names(dataset) == "Renewal.Revenue")
        
        for (i in 1:nrow(dataset)) {
                dataset[i, "Total.Revenue"] <- sum(dataset[i, 
                        start_rev_cols:end_rev_cols])
        }
        
        ## return cleaned dataframe
        dataset
        
}

top_opps_AE <- function(dataset, port = "TCR52", num = 10) {
        ## for given dataframe called 'dataset', return a new dataframe  
        ## which consists of the top 'num' opps for the AE 
        ## identified by portfolio code 'port'. Num defaults to 10. The 
        ## returned dataframe consists of the customer name and total
        ## revenue for the opp.
        
        dataset <- dataset[(dataset$Portfolio.Code == port), ]
        
        dataset <- dataset[order(dataset$Total.Revenue, 
                                 decreasing = TRUE), ]
        
        if (num > nrow(dataset)) {num <- nrow(dataset)}
        if (num < 1) {num <- 1}
        
        Customer <- as.vector(dataset[1:num, "Customer.Name"])
        Opportunity <- as.vector(dataset[1:num, "Opportunity.Name"])
        Revenue <- dataset[1:num, "Total.Revenue"]
        
        opp_rev <- data.frame(Customer, Opportunity, Revenue)
        
        ## return the new dataframe
        opp_rev
}

tot_val_opps_by_AE <- function(dataset, port = "TCR52") {
        ## for given dataframe called 'dataset', return a dataframe which
        ## consists of the sum of all Revenue columns for all opps for the AE
        ## identified by the portfolio code 'port'.
        
        dataset <- dataset[(dataset$Portfolio.Code == port), ]
        
        tot_val <- sum(dataset$New.Income.Revenue, dataset$Acquisition.Revenue,
                       dataset$Once.Off.Revenue, dataset$Renewal.Revenue)
        
        tot_val
}

