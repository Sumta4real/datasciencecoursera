#function named 'pollutantmean' that calculates the mean of a pollutant 
#(sulfate or nitrate) across a specified list of monitors. 
#The function 'pollutantmean' takes three arguments: 'directory', 'pollutant'
#and 'id'

pollutantmean <- function(directory,pollutant,id=1:332)
    {
    if (getwd() == "C:/Users/Sumaya/Documents/R_Projects/R programming Coursera/specdata")
        {
    filenames <- list.files()
    merge_file <- do.call(rbind,lapply(filenames,read.csv))
    data_needed <- subset(merge_file, ID %in% id)
    mean_pollutant = mean(data_needed[[pollutant]],na.rm=TRUE)
    mean_pollutant
    }
    
    else{
        setwd(directory)
        filenames <- list.files()
        merge_file <- do.call(rbind,lapply(filenames,read.csv))
        data_needed <- subset(merge_file, ID %in% id )
        mean_pollutant = mean(data_needed[[pollutant]],na.rm=TRUE)
        mean_pollutant
    }
    
   
    
    
}


# a function that reads a directory full of files and reports the number of 
#completely observed cases in each data file. The function should return a data
#frame where the first column is the name of the file and the second column is 
#the number of complete cases.

complete <- function(directory,id=1:332){
    if (getwd() == "C:/Users/Sumaya/Documents/R_Projects/R programming Coursera/specdata")
    {
        filenames <- list.files()
        merge_file <- do.call(rbind,lapply(filenames,read.csv))
        lst = subset(merge_file, ID %in% id & !is.na(sulfate) &!is.na(nitrate))
        tab = table(lst$ID)
        real <- data.frame(tab)
        names(real)<-c('id','nobs')
        real
    }
    
    else{
        setwd(directory)
        filenames <- list.files()
        merge_file <- do.call(rbind,lapply(filenames,read.csv))
        lst = subset(merge_file, ID %in% id & !is.na(sulfate) &!is.na(nitrate))
        table(lst$ID)
        tab = table(lst$ID)
        real <- data.frame(tab)
        names(real)<-c('id','nobs')
        real
       
        
    }
    
}




#a function that takes a directory of data files and a threshold for complete 
#cases and calculates the correlation between sulfate and nitrate for monitor 
#locations where the number of completely observed cases (on all variables) is 
#greater than the threshold. The function should return a vector of correlations 
#for the monitors that meet the threshold requirement. If no monitors meet the
#threshold requirement, then the function should return a numeric vector of 
#length







corr <- function(directory,threshold = 0){
    
       data <- complete(directory)
       filenames <- list.files()
       merge_file <- do.call(rbind,lapply(filenames,read.csv))
       corr_vector <- vector(mode="numeric")
       rel_id <- data$id[data$nobs > threshold]
       for (i in rel_id){ 
          lst = subset(merge_file, ID == i & !is.na(sulfate) &!is.na(nitrate))
           corr_ <- cor(lst$sulfate,lst$nitrate)
           corr_vector <- c(corr_vector,corr_)
           
       }
    corr_vector
        
    
    
}
