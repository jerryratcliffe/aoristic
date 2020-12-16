#' Create aoristic distribution chart
#'
#' Takes the output from the aoristic.summary() function and converts that data frame
#' into a series of eight charts for each day of the week (and a total chart) based on the
#' aggregate aoristic distribution of the events. 
#' Option (marks = TRUE) adds small tick marks showing the (y-axis adjusted) overall weekly 
#' distribution for comparison to the daily value. 
#' 
#' @param data1 a data frame output from the aoristic2.df function
#' @param marks marks=FALSE. TRUE shows tick marks for week distribution. Default is FALSE.
#' @examples 
#' aoristic.graph(dcburgsum)
#' aoristic.graph(dcburgsum, TRUE)
#' @export
#' @import grid
#' @references Ratcliffe, J. H. (2002). Aoristic signatures and the spatio-temporal analysis of high volume crime patterns. Journal of Quantitative Criminology, 18(1), 23-43.
# Some useful keyboard shortcuts for package authoring: Install Package: 'Ctrl + Shift + B' Check
# Package: 'Ctrl + Shift + E' Test Package: 'Ctrl + Shift + T'



aoristic.graph <- function(data1, marks = FALSE) {
    
    plots.df <- data1[, c(1, 3, 4, 5, 6, 7, 8, 2)]  # Reorder columns to put weekend at the end
    all.dots <- ifelse(marks == TRUE, T, F)  # T = Show small dots with 'all' distribution
    
    plots.df$all <- as.numeric(plots.df$Sun) + as.numeric(plots.df$Mon) + as.numeric(plots.df$Tue)
    plots.df$all <- plots.df$all + as.numeric(plots.df$Wed) + as.numeric(plots.df$Thu)
    plots.df$all <- plots.df$all + as.numeric(plots.df$Fri) + as.numeric(plots.df$Sat)
    max.value.all <- max((plots.df$all), na.rm = TRUE)
    
    stack <- active <- scaled <- NULL #prevents R CMD check flagging 'no visible global function definition'
    
    df5 <- (stack(plots.df[, 2:8]))
    df5$values <- as.numeric(df5$values)
    max.value <- max((df5$values), na.rm = TRUE)
    revised.names <- c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", 
        "14", "15", "16", "17", "18", "19", "20", "21", "22", "23")
    
    bar.color <- "steelblue4"       # color for the day-to-day chart
    bar.color.all <- "darkorange4"  # color for the entire week chart
    bar.width <- 0.7
    plots.df$scaled <- (plots.df$all * (1 / (max.value.all / max.value)))  # Create the proportional line value
    
    plot_data_column <- function(data.title, show.dots) {
        ggplot(plots.df, aes(x = revised.names, y = active)) + ylim(0, max.value) + ggtitle(data.title) + 
            geom_bar(stat = "identity", width = bar.width, fill = bar.color) + {
            if (show.dots) 
                geom_point(plots.df, mapping = aes(x = revised.names, y = (as.numeric(scaled))), group = 1, 
                  show.legend = FALSE, size = 1, colour = "grey63", shape = 0)
        } + xlab("") + ylab("Aoristic total")
    }
    
    plot_data_column_all <- function(data.title, show.dots) {
        ggplot(plots.df, aes(x = revised.names, y = active)) + ylim(0, max.value.all) + ggtitle(data.title) + 
            geom_bar(stat = "identity", width = bar.width, fill = bar.color.all) + 
        xlab("") + ylab("Aoristic total")
    }
    
    plots.df$active <- as.numeric(plots.df$Mon)
    plot.Mon <- plot_data_column("Monday", all.dots)
    plots.df$active <- as.numeric(plots.df$Tue)
    plot.Tue <- plot_data_column("Tuesday", all.dots)
    plots.df$active <- as.numeric(plots.df$Wed)
    plot.Wed <- plot_data_column("Wednesday", all.dots)
    plots.df$active <- as.numeric(plots.df$Thu)
    plot.Thu <- plot_data_column("Thursday", all.dots)
    plots.df$active <- as.numeric(plots.df$Fri)
    plot.Fri <- plot_data_column("Friday", all.dots)
    plots.df$active <- as.numeric(plots.df$Sat)
    plot.Sat <- plot_data_column("Saturday", all.dots)
    plots.df$active <- as.numeric(plots.df$Sun)
    plot.Sun <- plot_data_column("Sunday", all.dots)
    plots.df$active <- as.numeric(plots.df$all)
    plot.all <- plot_data_column_all("All days", all.dots)
    
    multiplot1(plot.all, plot.Tue, plot.Thu, plot.Sat, plot.Mon, plot.Wed, plot.Fri, plot.Sun, cols = 2)
}


multiplot1 <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots <- length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel ncol: Number of columns of plots nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)), ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots == 1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row, layout.pos.col = matchidx$col))
        }
    }
}

