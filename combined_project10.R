

library(repr)
library(dplyr)
library(corrplot)
library(ggplot2)
library(stats)
library(car)
library(ggpubr)
library(PerformanceAnalytics)

data <- read.csv("/Users/babin/Desktop/NTU/Academics/Y2S2/MH3511 Data Analysis/Project/HousePrices/train.csv")


###################################### PRELIMINARY NUMERICAL ANALYSIS ######################################

## numeric data only:
numeric_data <- select_if(data, is.numeric)

## manually removing columns that are not actually numeric, but just hve numbers as values
numeric_data <- select(numeric_data, -MoSold, -MSSubClass, -MasVnrArea, -BsmtFinSF1, -BsmtFinSF2, -LowQualFinSF, -BsmtFullBath, -BsmtHalfBath, -FullBath, -HalfBath, -BedroomAbvGr, -KitchenAbvGr, -TotRmsAbvGrd, -Fireplaces, -GarageCars, -EnclosedPorch, -X3SsnPorch, -ScreenPorch, -MiscVal) 


## convert to data frame:
dframe <- data.frame(numeric_data)

## remove NA values in certain columns (the ones which have NA values):
dframe <- dframe[complete.cases(dframe$LotFrontage), ]
# dframe <- dframe[complete.cases(dframe$GarageYrBlt), ]

## check for duplicates
duplicates <- dframe[duplicated(dframe), ]
any(duplicates)

# head(dframe)



######################################### PRELIM CORR MATRIX #############################################

cor_matrix <- cor(dframe)												## generate corr matrix
order_ind <- order(abs(cor_matrix[,ncol(dframe)]), decreasing=TRUE)		## ordering matrix
reorder_mat <- cor_matrix[order_ind, order_ind]

## plotting:
# corrplot(reorder_mat, method="color", addCoef.col="black", number.cex=0.5, tl.cex=0.5)



####################################### CHOOSING NUMERICAL VARS #########################################

dframe$totalSF <- dframe$GrLivArea + dframe$X1stFlrSF + dframe$X2ndFlrSF	## new column, totalSF
dframe <- select(dframe, -GrLivArea, -X1stFlrSF, -X2ndFlrSF)				## removing added SFs

## filtering df by pool or no pool:
havepool <- subset(dframe, PoolArea != 0)								
nopool <- subset(dframe, PoolArea == 0)			

	


## HOUSES W/O POOL <<-- because only 6 houses with a pool
chosen <- nopool[,c("Id", "LotFrontage", "LotArea", "GarageArea", "YearBuilt", "TotalBsmtSF", "totalSF", "SalePrice")]
col_names <- names(chosen)




########################################## HISTOGRAMS ###########################################

## will not print:
if(FALSE) {
	par(mfrow=c(2,4))
	for (col in col_names) {
		if (col == "Id") {next}
		hist(chosen[[col]], main=col, xlab=col)
	}
}


#################################### LOG TRANSFORMATIONS #######################################

## initializing new "log" dataframe:
log_chosen <- data.frame( matrix( ncol=ncol(chosen), nrow=nrow(chosen) ) )

for (col in colnames(chosen)) {
	## making sure YearBuilt and Id aren't transformed:
	if (col == "YearBuilt" | col == "Id") {
		log_chosen[[paste0(col)]] <- chosen[[col]]
	} else {
		## transforming:
		log_chosen[[paste0("log", col)]] <- log(chosen[[col]])
	}
}

## removing some randoly appearing columns lol
log_chosen <- subset(log_chosen, select = -c(X1, X2, X3, X4, X5, X6, X7, X8))
head(log_chosen)
before_outlier <- nrow(log_chosen)
print(before_outlier)


################################# LOG HISTOGRAMS ###################################

## wil not print:
if (FALSE) {
	
par(mfrow=c(2,4))

for(col in colnames(log_chosen)) {
	
	## YearBuilt does not work with a log transform
	if (col == "YearBuilt" | col == "Id") {
		next
	}
	hist(log_chosen[[col]], main=paste(col, "with outliers"), xlab=col)
}

## plotting YearBuilt as it is since log doesn't work with it
hist(log_chosen[["YearBuilt"]], main="YearBuilt", xlab="YearBuilt")

}


############################################# MERGE WITH CATEGORICAL DATA #############################################

## adding the categorical columns by the Id of the remaining houses
merged <- merge(log_chosen, data[, c("Id", "LotShape", "MSSubClass", "MasVnrType")], by="Id")
head(merged, 20)
nrow(merged)


############################### REMOVING IRRELEVANT AND NA VALUES ###################################

## removing Planned Unit Developments in MSSubClass 
filtered <- subset(merged, !(MSSubClass %in% c(120, 150, 160, 180, 190)))
nrow(filtered)

filtered <- na.omit(filtered, MasVnrType)
nrow(filtered)


##################################### REMOVING OUTLIERS FROM LOG #########################################

log_chosen <- filtered[, 1:8]
# head(log_chosen)
# nrow(log_chosen)

for (col in 2:ncol(log_chosen)) {
	x <- filtered[,col]
	
	q1 <- quantile(x, 0.25)
	q3 <- quantile(x, 0.75)
	iqr <- q3-q1
	lower <- q1 - 1.5*iqr
	print(paste(colnames(log_chosen)[col], "lower = ", lower))
	upper <- q3 + 1.5*iqr
	print(paste(colnames(log_chosen)[col], "upper = ", upper))
	
	## if too low or too high, replace with NA
	x <- ifelse(x<lower | x>upper, NA, x)
	
	## removing rows w NA
	filtered <- filtered[complete.cases(x),]
	print(nrow(filtered))
	
}

## reassigning log_chosen since it will be used for later analysis
log_chosen <- filtered[, 1:8]
# head(log_chosen)

head(filtered)
after_outlier <- nrow(filtered)
print(after_outlier)

outliers <- before_outlier - after_outlier
print(outliers)



########################################## LOG BOXPLOTS AFTER OUTLIERS GONE ###########################################


## will not print:
if (FALSE) {
	
par(mfrow=c(2,4))
for (col in colnames(log_chosen)) {
	## don't plot irrelevant stuff
	if (col == "Id") {next}
	boxplot(log_chosen[,col], main=paste(col, "without outliers"))
}

}

############################################## CORRELATION MATRIX ##################################################

## CORRELATION WITH LOG (NO OUTLIERS):


cor_mat_chosen <- cor(log_chosen)
order_ind_chosen <- order(abs(cor_mat_chosen[,ncol(log_chosen)]), decreasing=TRUE)
reorder_mat_chosen <- cor_mat_chosen[order_ind_chosen, order_ind_chosen]


## ORDERING the df (chosen variables) by correlation
corr_values <- cor_mat_chosen["logSalePrice", -ncol(log_chosen)]
corr_order <- order(-abs(corr_values))
log_chosen <- log_chosen[,c("logSalePrice", names(log_chosen)[corr_order])]
head(log_chosen)


## will not print:
# par(mfrow=c(1,1))
# corrplot(reorder_mat_chosen[1:7, 1:7], method="color", addCoef.col="black", number.cex=0.7, tl.cex=1)


########################################## REWRITING CHART.CORRELATION() ##########################################
chart.Correlation function (R, histogram = TRUE, method = c("pearson", "kendall", 
    "spearman"), ...) 
{
    x = checkData(R, method = "matrix")
    if (missing(method)) 
        method = method[1]
    cormeth <- method
    panel.cor <- function(x, y, digits = 2, prefix = "", use = "pairwise.complete.obs", 
        method = cormeth, cex.cor, ...) {
        usr <- par("usr")
        on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r <- cor(x, y, use = use, method = method)
        txt <- format(c(r, 0.123456789), digits = digits)[1]
        txt <- paste(prefix, txt, sep = "")
        if (missing(cex.cor)) 
            cex <- 2
        test <- cor.test(as.numeric(x), as.numeric(y), method = method)
        Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
            cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", 
                "**", "*", ".", " "))
        text(0.5, 0.5, txt, cex = cex)
        text(0.8, 0.8, Signif, cex = 2, col = 2)
    }
    f <- function(t) {
        dnorm(t, mean = mean(x), sd = sd.xts(x))
    }
    dotargs <- list(...)
    dotargs$method <- NULL
    rm(method)
    hist.panel = function(x, ... = NULL) {
        par(new = TRUE)
        hist(x, col = "light blue", probability = TRUE, axes = FALSE, 
            main = "", breaks = "FD")
        lines(density(x, na.rm = TRUE), col = "red", lwd = 2)
        rug(x)
    }
    panel.smooth <- function(x, y, col = "black", bg = NA, pch = 1, 
        cex = 1, col.smooth = "red", ...) {
        points(x, y, pch = pch, col = col, bg = bg, cex = cex)
        ok <- is.finite(x) & is.finite(y)
        if (any(ok)) {
            fit <- lm(y ~ x)
            abline(fit, col = col.smooth, ...)
            # r2 <- summary(fit)$r.squared
            # r2_text <- paste("R^2 =", format(round(r2, 2), nsmall = 2))
            # text(x = mean(x), y = max(y), labels = r2_text, pos = 1)
        }
    }
    if (histogram) 
        pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor, 
            diag.panel = hist.panel, ...)
    else pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor, 
        ...)
}
#############################################################################################################

## corrplot matrix between all variables
# par(mfrow=c(1,1))
# chart.Correlation(log_chosen[,1:7], histogram='FALSE', main="Correlation plot between numerical variables", cex.labels=1.75, cex.axis=2, cex.main=2)


####################################### REGRESSION OF CHOSEN NUMERICAL VARS #########################################

## will not print:
if (FALSE) {
	
par(mfrow=c(2,3))
y <- log_chosen$logSalePrice

for (col in 3:ncol(log_chosen)-1) {
	# if (col == "Id") {next}
	x <- log_chosen[,col]	
	## plotting each graph
	plot(x, y, main=paste0("Plot of ", colnames(log_chosen)[col]), xlab=colnames(log_chosen)[col], ylab="logSalePrice")
	model <- lm(y ~ x)
	abline(model, col="red")
}

}



####################################### SPEARMAN CORRELATION #########################################

## spearman for YearBuilt and two other seemingly related variables
cor.test(log_chosen$YearBuilt, log_chosen$logGarageArea, method="spearman")
cor.test(log_chosen$YearBuilt, log_chosen$logTotalBsmtSF, method="spearman")

## spearman for YearBuilt vs logSalePrice
spear <- cor.test(log_chosen$YearBuilt, log_chosen$logSalePrice, method = "spearman")
print(spear)

## plotting above correlation
## will not print
if(FALSE) {
par(mfrow=c(1,1))
ggplot(log_chosen, aes(x=YearBuilt, y=logSalePrice)) + 
	geom_point() + stat_smooth(method="lm", formula=y~x, se=FALSE) + 
	stat_cor(method="spearman", label.x=1973, label.y=11.5, size=5, color="blue") +
	labs(title="Scatterplot of logSalePrice and YearBuilt with Spearman Correlation Coefficient") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=18), plot.title=element_text(size=20))
}



########################################################################################################################################
#################################################       CATEGORICAL STUFF       ########################################################
########################################################################################################################################



################################################### MSSubClass ###################################################

## show boxplots without grouping
## converting MSSubClass values to "factor" data type
filtered$MSSubClass <- as.factor(filtered$MSSubClass)

par(mfrow=c(1,1))
ggplot(filtered, aes(x=MSSubClass, y=logSalePrice)) + geom_boxplot() + xlab("MSSubClass") + ylab("logSalePrice") + theme(axis.text = element_text(size=30), axis.title = element_text(size=25), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA))


## group the subcategories into larger ones

	
## combining the many subcategories of 1story, 2story and Split in MSSubClass
filtered$MSSubClass <- as.character(filtered$MSSubClass)
for (i in 1:nrow(filtered)) {
	## 1 story, 1.5 story = 1 story
	if (filtered$MSSubClass[i] %in% c("20", "30", "40", "45", "50")) {
		filtered$MSSubClass[i] <- "1story"
	}
	## 2 story, 2.5 story = 2 story
	if (filtered$MSSubClass[i] %in% c("60", "70", "75")) {
		filtered$MSSubClass[i] <- "2story"
	} 
	## Others = Split level
	if (filtered$MSSubClass[i] %in% c("80", "85", "90")) {
		filtered$MSSubClass[i] <- "Split"
	}
}


head(filtered, 20)
nrow(filtered)



## Converting MSSubClass values to "factor" data type
filtered$MSSubClass <- as.factor(filtered$MSSubClass)

## box plots of each category's logSalePrice
par(mfrow=c(1,1))
ggplot(filtered, aes(x=MSSubClass, y=logSalePrice)) + geom_boxplot() + xlab("MSSubClass") + ylab("logSalePrice") + theme(axis.text = element_text(size=30), axis.title = element_text(size=25), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA))

## Are the variances of the subgroups equal?
leveneTest(logSalePrice ~ MSSubClass, data=filtered)
## extremely small p-value 4.29e-07
## variances unequal
## no anova 
## kruskal-wallis instead

kw <- kruskal.test(logSalePrice ~ MSSubClass, data=filtered)
print(kw)
## small p-value 3.372e-07
## medians differ between groups

## MSSubClass matters in determining SalePrice.



################################################### MasVnrType ###################################################

par(mfrow=c(1,1))
ggplot(filtered, aes(x=MasVnrType, y=logSalePrice)) + geom_boxplot() + xlab("MasVnrType") + ylab("logSalePrice") + theme(axis.text = element_text(size=30), axis.title = element_text(size=25), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA))


## grouping brick
for (i in 1:nrow(filtered)) {
	if (filtered$MasVnrType[i] %in% c("BrkCmn", "BrkFace")) {
		filtered$MasVnrType[i] <- "Brick"
	}
}

## visualizing:
par(mfrow=c(1,1))
ggplot(filtered, aes(x=MasVnrType, y=logSalePrice)) + geom_boxplot() + xlab("MasVnrType") + ylab("logSalePrice") + theme(axis.text = element_text(size=30), axis.title = element_text(size=25), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA))


## test the variances:
filtered$MasVnrType <- as.factor(filtered$MasVnrType)
leveneTest(logSalePrice ~ MasVnrType, data = filtered)
## p-value is 0.1555
## variances are not the same
## 





################################################### LotShape ###################################################

## boxplots before grouping:
par(mfrow=c(1,1))
ggplot(filtered, aes(x=LotShape, y=logSalePrice)) + geom_boxplot() + xlab("LotShape") + ylab("logSalePrice") + theme(axis.text = element_text(size=30), axis.title = element_text(size=25), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA))

## grouping all irregulars
for (i in 1:nrow(filtered)) {
	if (filtered$LotShape[i] %in% c("IR1", "IR2", "IR3")) {
		filtered$LotShape[i] <- "IR"
	}
}

## visualizing:
par(mfrow=c(1,1))
ggplot(filtered, aes(x=LotShape, y=logSalePrice)) + geom_boxplot() + xlab("LotShape") + ylab("logSalePrice") + theme(axis.text = element_text(size=30), axis.title = element_text(size=25), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA))

## Histograms of each category to show normality
par(mfrow=c(1,2))
hist(filtered$logSalePrice[filtered$LotShape=="IR"], main="Histogram of logSalePrice (LotShape: IR)", xlab="logSalePrice", cex.lab=1.5)
hist(filtered$logSalePrice[filtered$LotShape=="Reg"], main="Histogram of logSalePrice (LotShape: Reg)", xlab="logSalePrice", cex.lab=1.5)


## test the variances:
subset_data <- filtered[c("logSalePrice", "LotShape")]					
group1 <- subset_data$logSalePrice[subset_data$LotShape == "IR"]
group2 <- subset_data$logSalePrice[subset_data$LotShape == "Reg"]

var.test(group1, group2)
## p-value is 0.052
## variances almost the same

## t-test
result <- t.test(group1, group2, var.equal=TRUE)
print(result)
## small p-value
## means are not the the same 

## LotShape does matter









