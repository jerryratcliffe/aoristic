#' Plot aoristic probabilities for one hour
#' 
#' Creates a plot showing aoristic probability values for one hour of the week. The X, Y coordinate pair (or latitude
#' and longitude) are used to create a plot that shows each event that could have occured in the user-
#' selected hour. Each event is color coded to represent the aoristic weight, range >0 to 1. Events
#' with weight 1 definitely occurred during that hour, while events with values at the lower end of the
#' range could have occured at one of many hours. For guidance on which day/hour is represented by a
#' number, use aoristic.ref()
#'
#' @param data1 a data frame output from the aoristic.df function
#' @param AorHour user-selected number for an hour in the week (range 1-168)
#' @return A ggplot object
#' @import ggplot2
#' @examples 
#' \dontrun{
#' 
#' aor.plot <- aoristic.map(aor.df, 25)
#' }
#' @export
#' @references Ratcliffe, J. H. (2002). Aoristic signatures and the spatio-temporal analysis of high volume crime patterns. Journal of Quantitative Criminology, 18(1), 23-43.


aoristic.map <- function(data1, AorHour = "1") {
    
    X <- Y <- Aoristic.value <- NULL #prevents R CMD check flagging 'no visible global function definition'
    
    if (!is.data.frame(data1)) {
        stop("The input data frame specified is not a data.frame object")
    }
    
    chosenhour.num <- AorHour
    chosenhour.num <- as.numeric(chosenhour.num)
    if (chosenhour.num < 1 || chosenhour.num > 168) {
        stop("The `hour` option parameter must be in the range 1 to 168")
    }
    
    # prep the plot data
    mapdata <- data1[, c(1, 2, (chosenhour.num + 5))]
    colnames(mapdata) <- c("X", "Y", "Aoristic.value")
    
    # establish the plot limits, with 5% buffer for points plotted at edge
    x.max <- max(mapdata[1], na.rm = TRUE)
    x.min <- min(mapdata[1], na.rm = TRUE)
    y.max <- max(mapdata[2], na.rm = TRUE)
    y.min <- min(mapdata[2], na.rm = TRUE)
    x.max <- x.max + ((x.max - x.min)*.05)
    x.min <- x.min - ((x.max - x.min)*.05)
    y.max <- y.max + ((y.max - y.min)*.05)
    y.min <- y.min - ((y.max - y.min)*.05)   
    
    mapdata <- mapdata[mapdata[3] > 0, ]    # Limit the data to only rows with aoristic values >0
    
    # Write the title text
    day.num <- 1 + (chosenhour.num%/%24)  # integer divide operator finds the day
    if (day.num == 8) {
        day.num <- 7
    }
    hour.num <- (chosenhour.num%%24) - 1  # modulus operator finds the hour
    if (hour.num == -1) {
        hour.num <- 23
    }
    temp.daywords <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
    title.txt <- paste("Aoristic values for", temp.daywords[day.num], "at hour ", sep = " ")
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
        scale_x_continuous( limits = c(x.min, x.max), expand = c(0, 0) ) +
        scale_y_continuous( limits = c(y.min, y.max), expand = c(0, 0) ) +
        my.color.pal + 
        labs(title = title.txt, x = "X coordinate or Longitude", y = "Y coordinate or Latitude")
   
    return(map.plot)
}