varxtimeplot <- function(data, variables, seriesVar, timeVar, timeLabel="time steps", lineType='l', ...){

	# "data" is a data frame
	# "variables" is a numeric vector that includes the column numbers referencing those variables to be plotted as y axis
	# "seriesVar" is the number referencing the variable distinguishing series
	# "timeVar" is the number referencing the variable distinguishing time steps
	# "..." stands for "par" arguments
	data_per_series <- split(data, data[,seriesVar])
	numseries <- length(data_per_series)
	
	xlimit <- c(min(data[,timeVar]),max(data[,timeVar]))
	cl <- rainbow(numseries)

	par(cex.main = 1 , cex.lab = 1.2 , cex.axis=.9, font.main = 4 , font.lab = 3 , 
		mar = c(2,4,1,10), xpd=TRUE, oma=c(3,0,0,0),xaxs="i", yaxs="i" , family="serif")

	layout(matrix(1:length(variables),length(variables),1))
	
	for (v in variables){
	
		plot(0,0,xlim = xlimit,ylim = c(min(data[,v]),max(data[,v])),
			type = "n", xlab = "", ylab = names(data)[v] )

		for (series in 1:numseries){
    			lines(data_per_series[[series]][,timeVar],
				data_per_series[[series]][,v],
				col = cl[series],type = lineType)

		legend('topright', inset=c(-0.2,0), levels(factor(data[,seriesVar])), col = cl, lty=1)
		}
	
	}
	mtext(timeLabel, side=1, adj=.55, outer=TRUE, font=4)
}
