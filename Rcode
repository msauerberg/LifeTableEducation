library(dplyr)
library(eurostat)
#please load these packages and download the data like this:
data <- get_eurostat("demo_mlexpecedu", time_format = "num")

#rename and redefine the file 
data$isced11 <- as.character(data$isced11)
data$isced11 <- ifelse(data$isced11=="ED0-2", "lower", data$isced11)
data$isced11 <- ifelse(data$isced11=="ED3_4", "middle", data$isced11)
data$isced11 <- ifelse(data$isced11=="ED5-8", "higher", data$isced11)
data$isced11 <- ifelse(data$isced11=="TOTAL", "total", data$isced11)

data$age <- as.character(data$age)
data$age <- ifelse(data$age=="Y_LT1", "Y0", data$age)
data$age <- ifelse(data$age=="Y_GE85", "Y85", data$age)
data$age <- substring(data$age, 2)

data <- data[,-1]
colnames(data) <- c("sex","age","edu","country","year","ex")
data$age <- as.numeric(data$age)

#We focus on the year 2016 as an example
data <- filter(data, year==2016)

#The following function has the arguments "country.select", "edu.select" and "sex.select".

my.function <- function(country.select, edu.select, sex.select) {

    select.country <- arrange(filter(data, country==country.select ,edu==edu.select &
                                               sex==sex.select),age)

#smooth to get more decimals by applying the loess function
#and then predicting ex with more decimals
    grab.LE <- select.country$ex
    smooth.it <- loess(grab.LE~select.country$age, span=0.2)
    predict.it <- predict(smooth.it, seq(0,85,1))
    select.country$ex.decimals <- predict.it


    LT.derive <- data.frame(Age=0:85)
    LT.derive$lx <- NA
    LT.derive$Tx <- NA

    LT.derive$ex <- select.country$ex.decimals
    LT.derive$lx[1] <- 100000
    LT.derive$Tx[1] <- 100000*select.country$ex.decimals[1]
#this loop refers to equation 1 in the paper
    for (j in 2:86) {

        upper <- (LT.derive$lx[j-1]^2)-2*LT.derive$lx[j-1]*LT.derive$Tx[j-1]
        bottom <- (LT.derive$ex[j-1]-LT.derive$ex[j])*2*
                   LT.derive$lx[j-1]-2*LT.derive$Tx[j-1]-LT.derive$lx[j-1]
        LT.derive$Tx[j] <- upper/bottom*LT.derive$ex[j]
        LT.derive$lx[j] <- upper/bottom

    }
#I check, whether lx is monotonic decreasing, i.e., no resurrection in the life table
    lx.diff <- diff(LT.derive$lx)
    lx.diff <- round(lx.diff, 5)

    if (all(diff(lx.diff) < 0)) {

        px <- c(LT.frame$lx[-1]/LT.frame$lx[-86],0)

    }else{
#sometimes, it is not, then I force it =)
#please note, this occurs usually at very young ages and won't affect
#LE at age 30 or older
        lx.diff[lx.diff>=0] <- -runif(length(lx.diff[lx.diff>=0]), 1, 5)
        lx.monotonic <- cumsum(c(100000, lx.diff))
        px <- c(lx.monotonic[-1]/lx.monotonic[-86],0)

        }
#from here, the life table is constructed very standard
    lx <- round(c(100000, (cumprod(px)*100000)[1:(length(px)-1)]))
    dx <- round(c(-diff(lx), lx[length(lx)]))
    LT.derive$lx <- lx
    LT.derive$dx <- dx
    LT.derive$px <- px
    Lx1 <- lx[-1]+0.5[-length(px)]*dx[-length(dx)]
    Lx.open <- LT.derive$Tx[1]-sum(Lx1)
    LT.derive$Lx <- round(c(Lx1, Lx.open))
    LT.derive$Tx <- rev(cumsum(rev(LT.derive$Lx)))
    LT.derive$ex.derived <- LT.derive$Tx/LT.derive$lx
    LT.derive$ex.original <- select.country$ex
    LT.derive$diff <- LT.derive$ex.original-LT.derive$ex.derived
    LT.derive$Country <- country.select
    LT.derive$Edu <- edu.select
    LT.derive$Sex <- sex.select

    return(LT.derive[,c("Country","Edu","Sex","Age","px","lx","dx","Lx",
                        "Tx","ex.derived","ex.original","diff")])
}

#The following code applies the function to all 16 selected European countries by educational attainment, separated by sex.


#these are the country codes
edu.countries <- c("BG","CZ","DK","EE","EL","HR","IT","HU",
                   "PL","PT","RO","SI","SK","FI","SE","NO")


###Females###
out.females <- c()

for (country.select in edu.countries) {

    for (edu.select in c("higher","middle","lower")) {

        out.females <- rbind(out.females,my.function(country.select, edu.select, "F"))
}
}


###Males###
out.males <- c()

for (country.select in edu.countries) {

    for (edu.select in c("higher","middle","lower")) {

        out.males <- rbind(out.males,my.function(country.select, edu.select, "M"))
}
}


#Last but not least, I plot the difference between the original $e_x$ and the derived $e_x$.

par(mfrow=c(3,3))
for (edu in c("higher","middle","lower")) {
    plot(1,1, type="n", xlim=c(1,16), ylim=c(-0.2,0.2),
         main=paste("Females",edu,sep=" "), xlab="Countries",
         ylab="LE 30 original - LE30 derived")
    points(1:16,out.females$diff[out.females$Edu==edu & out.females$Age==30])
    text(1:16,out.females$diff[out.females$Edu==edu & out.females$Age==30], 1:16,
         label=out.females$Country[out.females$Edu==edu & out.females$Age==30])
}

for (edu in c("higher","middle","lower")) {
    plot(1,1, type="n", xlim=c(1,16), ylim=c(-0.2,0.2),
         main=paste("Males",edu,sep=" "), xlab="Countries",
         ylab="LE 30 original - LE30 derived")
    points(1:16,out.males$diff[out.males$Edu==edu & out.males$Age==30])
    text(1:16,out.males$diff[out.males$Edu==edu & out.males$Age==30], 1:16,
         label=out.males$Country[out.males$Edu==edu & out.males$Age==30])
}

#End
