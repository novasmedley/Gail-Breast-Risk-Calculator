# The goal of this script is to do some data collection.

# Load MySQL library
library(RMySQL)

# Connect to the database. Connection details are not committed.
dataDir <- "/Users/simonxhan/repos/breastriskcalc" # Simon @ MII
if(!exists("mydb")) source(paste(dataDir,"dbCon.R",sep="/"))

#--------------------------------------------------------------------
# Find all patients who had decreasing 5-year Gail scores.
#--------------------------------------------------------------------
# Currently only looking at white patients.
# First select all white women. In rare cases, race is not indicated, so a
# simple where clause would exclude those rows.
sql <- "SELECT DISTINCT( mrn )
        FROM   athena_survey
        WHERE  white = 1"
data <- dbGetQuery(mydb,sql) # This does send, fetch all, and clear
# Now retrieve all entries for these patients.
# We're looking at either g_5yearscore or vt_gailrisk*100
sql <- paste("SELECT mrn,vt_gailrisk*100",
             " FROM  athena_survey",
             " WHERE mrn IN ( ", paste(data[,1],collapse=','), " )",
             " ORDER BY mrn,visitdate")
data <- dbGetQuery(mydb,sql) # This does send, fetch all, and clear

# Preallocate data frame for target patients.
# This list cannot be bigger than the size of query data.
df <- data.frame(matrix(ncol=3,nrow=nrow(data)))
names(df) = c("MRN","INIT","LAST")

# Some variables to help with finding target patients
dfIdx <- 1 # Keeps track of which index we're on
mrnPrev <- ""
g_sInit <- 0
g_sLast <- 100 # So that in the first iteration (last <= init) == false
exams <- 0 # Keeps track of the number of exams per patient

# Loop through query data
for (i in 1:nrow(data)) {

  mrn <- data[i,1]
  g_s <- data[i,2]

  if (is.na(g_s)) next # Skip rows that don't have gail scores

  # New patient, check whether has lower gail score
  if (mrnPrev != mrn) {

    # Patients who have lower gail scores and more than 1 exam
    if (g_sLast <= g_sInit& exams > 1) {

#       cat("mrnPrev: ",mrnPrev,"\n")
#       cat("g_sInit: ",g_sInit,"\n")
#       cat("g_sLast: ",g_sLast,"\n")

      # Save patient to data frame. Increase index.
      df[dfIdx,] <- c(mrnPrev,g_sInit,g_sLast)
      dfIdx <- dfIdx + 1
    }

    # Reset inital gain score and exam count
    g_sInit <- g_s
    exams <- 0
  }

  # Update some vars and increase exam count
  mrnPrev <- mrn
  g_sLast <- g_s
  exams <- exams + 1
}

# Remove unpopulated rows
df <- na.omit(df)

# Write to file
write.table(df$MRN,row.names=FALSE,col.names=FALSE,quote=FALSE,
            eol=",\n",file=paste(dataDir,"decGailPatientsList.txt",sep="/"))

# Disconnect and clear connection variable
dbDisconnect(mydb)
rm(mydb)

# Some notes
# lapply(list, function(x) x[!is.na(x)])
#
# Random notes:
#
# dbListTables(mydb)
# dbListFields(mydb,'athena_survey')
# dbListConnections(MySQL())
# cons <- dbListConnections(MySQL())
# for (con in cons) dbDisconnect(con)