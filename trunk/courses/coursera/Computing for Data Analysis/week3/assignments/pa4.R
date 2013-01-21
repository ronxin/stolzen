outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
hospital <- read.csv("hospital-data.csv", colClasses = "character")

outcome.hospital <- merge(outcome, hospital, by = "Provider.Number")

death <- as.numeric(outcome.hospital[, 11])
npatient <- as.numeric(outcome.hospital[, 15])
owner <- factor(outcome.hospital$Hospital.Ownership)

xyplot(death ~ npatient | owner, 
   xlab = "Number of Patients Seen", 
   ylab = "30-day Death Rate", 
   main = "Heart Attack 30-day Death Rate by Ownership",
   panel = function(x, y, ..., subscripts) {
     panel.xyplot(x, y)
     panel.lmline(x, y)
   }) 
