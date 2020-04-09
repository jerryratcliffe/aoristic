library(testthat)
library(aoristic2)

test_check("aoristic2")
context("aoristic2 program")


  
  ################## HABERMAN DATA
# setwd("D:/Box Sync/Work/Aoristic/aoristic.backup/Data")
# dcdata <- read.dbf("wash_dc_incidents_2016.dbf", as.is = FALSE)
# dcdata <- as.data.frame(dcdata[dcdata$OFFENSE=='BURGLARY',])
# dcdata$dcFrom <- paste(format(dcdata$FROM_DATE, "%m/%d/%Y"), " ", dcdata$FROM_TIME)
# dcdata$dcTo <-   paste(format(dcdata$TO_DATE, "%m/%d/%Y"), " ", dcdata$TO_TIME)
# dcdata$dcFrom = as.POSIXct(dcdata$dcFrom, format="%m/%d/%Y %H:%M", tz='')
# dcdata$dcTo = as.POSIXct(dcdata$dcTo, format="%m/%d/%Y %H:%M", tz='')
# aor.datacheck <- aoristic2.datacheck(dcdata, "XCOORD", "YCOORD", "dcFrom", "dcTo")
aor.datacheck <- aoristic2.datacheck(dcburglaries, "X", "Y", "StartDateTime", "EndDateTime")


test_that("aoristic.datacheck functions", {
  expect_equal(nrow(aor.datacheck), 1025)
})