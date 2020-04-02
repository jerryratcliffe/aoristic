#' Creates a plot showing aoristic values for one hour of the week
#'
#' @param data1 data.frame created by aoristic2.df
#' @param AorHour use seelected number for an hour in the week (range 1-168)
#' @return A ggplot object
#' @import ggplot2
#' @export
#' @references Ratcliffe, J. H. (2002). Aoristic Signatures and the Spatio-Temporal Analysis of High Volume Crime Patterns. Journal of Quantitative Criminology, 18(1), 23-43.

# Some useful keyboard shortcuts for package authoring: Install Package: 'Ctrl + Shift + B' Check
# Package: 'Ctrl + Shift + E' Test Package: 'Ctrl + Shift + T'

aoristic2.map <- function(data1, AorHour = "1") {
    
    if (!is.data.frame(data1)) {
        stop("The input data frame specified is not a data.frame object")
    }
    
    if (FALSE) {
        # testing only - JHR use
        data1 <- aor.df
        AorHour <- 168
    }
    
    aor.df <- data1
    chosenhour.num <- AorHour
    chosenhour.num <- as.numeric(chosenhour.num)
    if (chosenhour.num < 1 || chosenhour.num > 168) {
        stop("The `hour` option parameter must be in the range 1 to 168")
    }
    
    # prep the plot data
    mapdata <- aor.df[, c(1, 2, (chosenhour.num + 8))]
    colnames(mapdata) <- c("X", "Y", "Aoristic.value")
    mapdata <- mapdata[mapdata[3] > 0, ]
    
    # Write the title text
    day.num <- 1 + (chosenhour.num%/%24)  # integer divide operator finds the day
    if (day.num == 8) {
        day.num = 7
    }
    hour.num <- (chosenhour.num%%24) - 1  # modulus operator finds the hour
    if (hour.num == -1) {
        hour.num = 23
    }
    temp.daywords <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
    title.txt <- paste("Aoristic values for", temp.daywords[day.num], sep = " ")
    title.txt <- paste(title.txt, "at hour ", sep = " ")
    if (hour.num < 10) {
        title.txt <- paste(title.txt, "0", hour.num, "00 - 0", hour.num, "59", sep = "")
    }
    if (hour.num >= 10) {
        title.txt <- paste(title.txt, "", hour.num, "00 - ", hour.num, "59", sep = "")
    }
    
    # Set up the plot components
    my.color.pal <- scale_colour_gradientn(colours = c("red3", "steelblue4", "gray80"), values = c(1, 0.25, 
        0), limits = c(0, 1))
    
    # create the ggplot map object
    map.plot <- ggplot(mapdata, aes(x = X, y = Y, colour = Aoristic.value)) + geom_point(size = 4, alpha = 0.6) + 
        my.color.pal + labs(title = title.txt, x = "X coordinate or Longitude", y = "Y coordinate or Latitude")
    
    return(map.plot)
}

