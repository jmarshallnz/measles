# Reads in raw data source, extracts data for each epidemic
# and generates a new .csv file with the data from all epidemics

# assumes the variable 'data_file' contains the data file to use

# columes that indicate an outbreak
outbreak_col      <- "Outbrk"
outbreak_code_col <- "OutbreakCode"

# column for report date
date_col          <- "reportdate"
date_format       <- "%d-%B-%y"

# output folder to create
output_dir        <- "outbreaks"

# Episurv has a weird week layout as follows:
# 1. All weeks are Saturday through Friday.
# 2. The first week of the year must end on the first Friday of the year.

# NOT SURE ABOUT THE NEXT TWO...
# 3. Each year has between 1 and 53?? such weeks.  Any further weeks are combined with week 1 of the next year.
# 4. Every week is 7 days, except possibly for week '1' of each year, which might be up to 14 days.

# survey weeks start on a saturday, so just find the previous saturday
surv_week_start <- function(date)
{
  # 1. Find the previous saturday
  prev_sat <- date
  while (weekdays(prev_sat) != "Saturday")
    prev_sat <- prev_sat - 1;
  return(as.character(prev_sat))
}

surv_month_start <- function(date)
{
  # Find the month
  return(format.Date(date, "%Y-%m-01"))
}

# read in data
all <- read.csv(data_file)

outbreak_rows <- all[,outbreak_col] == "Yes"
outbreak_rows_with_code <- !is.na(all[,outbreak_code_col])

table(outbreak_rows, outbreak_rows_with_code)

# there are 7 rows with Outbrk == TRUE and no OutbreakCode
# there are 35 rows with OutbreakCode specified and Outbrk = FALSE

# assume we want ones with the OutbreakCode specified - this throws away 7 unknowns
outbreak_rows <- outbreak_rows_with_code
all[!outbreak_rows,outbreak_code_col] <- 0

# now grab out the date field and convert to week
# TODO: for epidemic we could use onset date, but its not always available
notification_dates <- as.Date(all[,date_col], format=date_format)
notification_weeks <- sapply(notification_dates, surv_week_start)

# create output directory
dir.create(file.path(output_dir), showWarnings = FALSE)

# for each outbreak, compute the incidence
outbreak_nos <- unique(all[outbreak_rows,outbreak_code_col])
for (ob_num in outbreak_nos)
{
  # find the rows
  ob_rows  <- all[,outbreak_code_col] == ob_num

  # create a date range
  ob_range <- range(as.Date(notification_weeks[ob_rows]))
  ob_weeks <- rep(0, length=diff(ob_range)/7+1)
  names(ob_weeks) <- seq(ob_range[1], ob_range[2], by=7)

  # compute the incidence by tabling up
  t <- table(notification_weeks[ob_rows])
  ob_weeks[names(t)] <- t

  # write out an outbreak file
  outbreak_data <- data.frame(date=names(ob_weeks), week=1:length(ob_weeks), incidence=ob_weeks)
  outbreak_file <- file.path(output_dir, sprintf("outbreak%02d.csv", ob_num))
  write.csv(outbreak_data, outbreak_file, row.names=F)
}

# now the total incidence through time, with outbreak no's as well

pdf("incidence.pdf", width=20, height=8)

notification_months <- sapply(notification_dates, surv_month_start)

num_months <- (2014.5-1997)*12

ob_range <- range(as.Date(notification_months))
ob_weeks <- matrix(0, length(unique(all[,outbreak_code_col])), num_months)
colnames(ob_weeks) <- 1:num_months
for (i in 1:num_months)
  colnames(ob_weeks)[i] <- sprintf("%4d-%02d-01", 1997 + floor((i-1)/12), (i-1) %% 12 + 1)
rownames(ob_weeks) <- unique(all[,outbreak_code_col])
t <- table(all[,outbreak_code_col], notification_months)
ob_weeks[rownames(t), colnames(t)] <- t

# plot time series of total cases
ts <- colSums(ob_weeks)
pdf("incidence_1997_2014.pdf", width=10, height=5)
plot(NULL, type="l", ylab="Cases per month", xlim=c(0, num_months-1), ylim=c(0, max(ts)*1.05), xlab="", xaxt="n", xaxs="i", yaxs="i")
x <- c(0, 0:(num_months-1), num_months-1)
y <- c(0, ts, 0)
polygon(x, y, col="lightblue", border="lightblue")

axis(1, seq(0,floor(num_months/12)*12,by=12), labels=rep("", floor(num_months/12)+1))
mtext(1996+1:floor(num_months/12), side=1, at = (1:floor(num_months/12))*12 - 6, line=0.5, cex=0.8)
box()
dev.off()

barplot(ob_weeks)
dev.off()



