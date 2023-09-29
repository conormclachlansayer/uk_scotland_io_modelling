# Loading Libraries
library(tidyverse)
library(readxl)
library(data.table)

########### Scotland Only IO Modelling ########### 
### Import Mappings
scot_map <- read_xlsx("Mappings.xlsx",sheet="Scot_Mapping")

# Only keep first 3 cols
scot_map <- scot_map[,c(1:3)]

########### Importing & Cleaning Data ########### 

IO_2019 <- read_excel("Scotland_IO_2019.xlsx",sheet = "IxI")

# Cleaning redundant rows
IO_2019 <- IO_2019[-c(1:5),-c(1:2)]

# Setting first row as colnames
colnames(IO_2019) <- c("Industry", IO_2019[1,][-1])

IO_2019 <- IO_2019[-1,] #removing first row

# Converting all apart from first row to numeric, and put in £ terms 
IO_2019[,-1] <- sapply(IO_2019[,-1], as.numeric)*1000000 # as orig was in £m

# As IO_2019 is symmetric, setting colnames of industry to be identical to rows
colnames(IO_2019) <- c("Industry", as.matrix(IO_2019[c(1:98),1]),colnames(IO_2019)[100:115])

# Selecting required industry and household data
# First 98 rows and row 105, cols 1-99 and col 101
io_data <- IO_2019[c(1:98, 105), c(1:99, 101)]

########### Aggregating IO Table to match Regional GVA Sectors ########### 
# New row and column names mapping
new_rownames <- as.data.frame(cbind("Industry" = c(scot_map$`Industry Group`, "Compensation of employees"),
                                    "New_Industry" = c(scot_map$`New Industry`, "Compensation of employees")))

new_colnames <- as.data.frame(cbind("Industry" = c(scot_map$`Industry Group`, "Households"),
                                    "New_Industry" = c(scot_map$`New Industry`, "Households")))

# Get copy of IO
agg_io_data <- copy(io_data)

# Replace old industry names with new 
agg_io_data <- left_join(agg_io_data,
          new_rownames,
          by = c("Industry" = "Industry"))

# Rearrange columns, so last is first, and first is removed
agg_io_data <- agg_io_data[,c(ncol(agg_io_data), 2:(ncol(agg_io_data)-1))]

# Aggregate by row names
agg_io_data <- as.data.table(agg_io_data, keep.rownames = FALSE)[, lapply(.SD, sum), by = New_Industry]

# Transpose to aggregate cols
agg_io_data <- transpose(agg_io_data, keep.names = "Industry", make.names = "New_Industry")

# Replace old industry names with new 
agg_io_data <- full_join(agg_io_data,
                         new_colnames,
                         by = c("Industry" = "Industry"))

# Rearrange columns, so last is first, and first is removed
setcolorder(agg_io_data, c(ncol(agg_io_data), 2:(ncol(agg_io_data)-1)))

# Removing original industry
agg_io_data$Industry <- NULL

# Aggregate by row names
agg_io_data <- as.data.table(agg_io_data, keep.rownames = FALSE)[, lapply(.SD, sum), by = New_Industry]

# Transpose back to original col/rows
agg_io_data <- transpose(agg_io_data, keep.names = "Industry", make.names = "New_Industry")

### Convert to matrix
agg_io_data <- as.matrix(agg_io_data)
                  
# Setting first col as row names
rownames(agg_io_data) <- agg_io_data[,1]
agg_io_data <- agg_io_data[,-1]

# Converting to numeric matrix
agg_io_data <- matrix(as.numeric(agg_io_data),
                  ncol = ncol(agg_io_data),
                  dimnames = list(rownames(agg_io_data),
                                  colnames(agg_io_data)))

# Replacing NAs with 0
agg_io_data[which(is.na(agg_io_data))] <- 0

########### Extracting Total Output ########### 
# Total final demand
final_demand = as.matrix(IO_2019[which(IO_2019$Industry == "Total output at basic prices"),])
final_demand <- final_demand[1,c(2:99)]

# When calculating Type 2, use £156,241m as denominator? (As per SG methodology paper)
final_demand <- c(final_demand, "Total Household Income" = "156241000000")

# Converting to numeric matrix
final_demand <- as.data.frame(cbind("Industry" = names(final_demand),
                                    "Total Output" = as.numeric(final_demand)))
final_demand$`Total Output` <- as.numeric(final_demand$`Total Output`)

# Aggregating up
final_demand[,1] <- new_colnames[1:(nrow(new_colnames)),2]

final_demand <- as.data.table(final_demand, keep.rownames = FALSE)[, lapply(.SD, sum), by = Industry]

# Converting to matrix
# Converting to numeric matrix
final_demand <- as.matrix(final_demand)
rownames(final_demand) <- final_demand[,"Industry"]
final_demand <- final_demand[,2]

# Converting to numeric matrix
final_demand <- matrix(as.numeric(final_demand),
                       ncol = 1,
                       dimnames = list(names(final_demand),
                                       "Total Output"))

########### Constructing Type 1 Leontief ########### 
# REMEMBER: L = (I-A)^-1

# Removing Household Spend & Compensation of Employees
agg_io_data1 <- agg_io_data[-nrow(agg_io_data),-ncol(agg_io_data)]

# Removing total household spend
final_demand1 <- as.matrix(final_demand[-nrow(final_demand),])
 
# where A is the direct requirements matrix, each cell of the IxI divided by it's column total
A1 <- t(t(agg_io_data1)/final_demand1[,1])

# Identity matrix
I1 <- diag(ncol(A1))

# Type 1 Leontief Matrix
L1 <- solve(I1-A1)

# Cleanup
remove(agg_io_data1, final_demand1, A1, I1)

########### Constructing Type 2 Leontief ########### 
# REMEMBER: L = (I-A)^-1
# where A is the direct requirements matrix, each cell of the IxI divided by it's column total
A2 <- t(t(agg_io_data)/final_demand[,1])

# Identity matrix
I2 <- diag(ncol(A2))

# Type 2 Leontief Matrix
L2 <- solve(I2-A2)

# Fixing row and column names
L2cols <- rownames(L2)
rownames(L2) <- colnames(L2)
colnames(L2) <- L2cols

remove(L2cols)

# Removing last row & column
L2 <- L2[c(1:(nrow(L2)-1)),c(1:(ncol(L2)-1))]

# Cleanup
remove(A2, I2)

###### OVERALL MULTIPLIERS ######  

Type1 <- colSums(L1)
Type2 <- colSums(L2)

########### SPENDING VECTOR ########### 

# Get copy of IO
scot_spending <- read_xlsx("Spending_Vector.xlsx",sheet="Scot_Spending")

# Only need columns 2 & 3
scot_spending <- scot_spending[,c(2,3)]

# Replace old industry names with new 
scot_spending <- left_join(scot_spending,
                         new_rownames,
                         by = c("Industry Group" = "Industry"))

# Rearrange columns, so last is first, and first is removed
scot_spending <- scot_spending[,c(ncol(scot_spending), 2:(ncol(scot_spending)-1))]

# Aggregate by row names
scot_spending <- as.data.table(scot_spending, keep.rownames = FALSE)[, lapply(.SD, sum), by = New_Industry]

# Convert to matrix 
scot_spend_rows <- unlist(array(scot_spending[,1])) # extracting rownames

scot_spending <- as.matrix(scot_spending[,2]) 
rownames(scot_spending) <- scot_spend_rows
colnames(scot_spending) <- "Output"


########### ECONOMIC IMPACT ########### 

########### Output Calculations ########### 

# Vector of output in each industry given a spending vector (i.e. the impact of spending in ALL industries 
# each row corresponds to total output in given industry, as a result of TOTAL spend across all industries)
# Total Output Agriculture = Ag spend x [Agg, Agg L1 multiplier] + Manu spend x [Agg, Manu L1 multiplier] + etc..
# note: [Agg, Manu L1 multiplier] corresponds to production/output in Agg due to consumption/demand in Manu
L1_Output <- L1 %*% scot_spending

L2_Output <- L2 %*% scot_spending

# Combining into single matrix

Estimates_Output_Scot <- cbind(scot_spending,
                L1_Output,
                L2_Output)

colnames(Estimates_Output_Scot) <- c("Direct","Direct+Indirect","Direct+Indirect+Induced")

Estimates_Total_Output_Scot <- colSums(Estimates_Output_Scot)

########### GVA Calculations ########### 
# GVA & Output Info
GVA_Output <- IO_2019[which(IO_2019$Industry %in% 
                              c("Gross value added", "Total output at basic prices")),
                      c(1:which(colnames(IO_2019) == "Households as employers"))]

# Need to aggregate up to desired industries
# First transpose as we want to aggregate cols
GVA_Output <- transpose(GVA_Output, keep.names = "Industry", make.names = "Industry")


# Replace old industry names with new 
GVA_Output <- left_join(GVA_Output,
                         new_colnames,
                         by = c("Industry" = "Industry"))

# Rearrange columns, so last is first, and first is removed
GVA_Output_Scot <- GVA_Output[,c(ncol(GVA_Output), 2:(ncol(GVA_Output)-1))]

# Aggregate by row names
GVA_Output_Scot <- as.data.table(GVA_Output_Scot, keep.rownames = FALSE)[, lapply(.SD, sum), by = New_Industry]

# Estimate GVA - Output Ratios
GVA_Output_Scot$GVA_Output_Ratio <- GVA_Output_Scot$`Gross value added`/GVA_Output_Scot$`Total output at basic prices`

# Estimate GVA using ratio - cut off 
Estimates_GVA_Scot <- Estimates_Output_Scot * matrix(rep(GVA_Output_Scot$GVA_Output_Ratio,3), ncol = 3)

# Calculate total GVA by summing columns
Estimates_Total_GVA_Scot <- colSums(Estimates_GVA_Scot)

########### Jobs Calculations ########### 
# Import BRES data
Scotland_BRES_2019 <- read_excel("Scotland_BRES_2019_Employment.xlsx",sheet = "Data")

# Cleaning
Scotland_BRES_2019 <- Scotland_BRES_2019[c(7:(nrow(Scotland_BRES_2019)-4)),]
colnames(Scotland_BRES_2019) <- Scotland_BRES_2019[1,]
Scotland_BRES_2019 <- Scotland_BRES_2019[c(3:nrow(Scotland_BRES_2019)),]

# Removing columns containing 'flag'
Scotland_BRES_2019 <- Scotland_BRES_2019[,setdiff(c(1:ncol(Scotland_BRES_2019)), which(colnames(Scotland_BRES_2019) %in% "Flags"))]

# Mapping to new industries (remember to update "MAPPING" excel document)
scot_map_BRES <- read_xlsx("Mappings.xlsx",sheet="Mapping_BRES")

# Cleaning
scot_map_BRES <- scot_map_BRES[,c(1:2)]

# Aggregating up to match model industries
# Replace old industry names with new 
Scotland_BRES_2019 <- left_join(Scotland_BRES_2019,
                         scot_map_BRES,
                         by = c("Industry" = "Industry"))

# Rearrange columns, so last is first, and first is removed
Scotland_BRES_2019 <- Scotland_BRES_2019[,c(ncol(Scotland_BRES_2019), 2:(ncol(Scotland_BRES_2019)-1))]

# Convert all columns apart from first to numeric
Scotland_BRES_2019[,-1] <- sapply(Scotland_BRES_2019[,-1], as.numeric)

# Aggregate by row names
Scotland_BRES_2019 <- as.data.table(Scotland_BRES_2019, keep.rownames = FALSE)[, lapply(.SD, sum), by = `New Industry`]

# Calculate Scotland Total
Scotland_BRES_2019$Scotland <- rowSums(Scotland_BRES_2019[,-1])

# Checking colSums
colSums(Scotland_BRES_2019[,-1])

# Combining Scotland Total with GVA data
GVA_Jobs_Scot <- left_join(GVA_Output_Scot[,c(1,2)],
                      Scotland_BRES_2019[,c(1,34)],
                      by = c("New_Industry" = "New Industry"))

GVA_Jobs_Scot$GVA_Jobs_Ratio <- GVA_Jobs_Scot$`Gross value added`/GVA_Jobs_Scot$Scotland

# Estimate Jobs using ratio - multiply by £1m to get correct GVA per job numbers
Estimates_Jobs_Scot <- Estimates_GVA_Scot * 1000000 / matrix(rep(GVA_Jobs_Scot$GVA_Jobs_Ratio,3), ncol = 3)

# Total Jobs
Estimates_Total_Jobs_Scot <- colSums(Estimates_Jobs_Scot)

########### TOTAL SUMMARY ########### 
Total_Summary_Scot <- rbind(Estimates_Total_Output_Scot, Estimates_Total_GVA_Scot, Estimates_Total_Jobs_Scot)

# Only keeping final results
remove(list = setdiff(ls(), c("L1_UK", "L2_UK", "Total_Summary_UK", "Estimates_Output_UK", "Estimates_GVA_UK", "Estimates_Jobs_UK", "GVA_Output_UK", "GVA_Jobs_UK",
                              "L1_Scot", "L2_Scot", "Total_Summary_Scot", "Estimates_Output_Scot", "Estimates_GVA_Scot", "Estimates_Jobs_Scot", "GVA_Output_Scot", "GVA_Jobs_Scot")))