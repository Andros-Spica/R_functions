heatggplot <- function(summdat,MEANvar, SDvar=c(NULL), sdLeg=FALSE,
				colRange=c("gold","darkgreen"), sdRange=c(10,50),
				transect=c(NULL,NULL), transColour="black", transType=2, transSize=2,
				xName=names(summdat[,1]), yName=names(summdat[,2]),
				zMEANName=paste(names(summarydata[,MEANvar]),"\n"),
				zSDName=paste(names(summarydata[,SDvar]),"\n"),
				zVarsNames=c(NULL),
				fName="heatggplot", outType=c(NULL),
				strip.text.size=60, strip.text.vjust=-1, strip.text.hjust=.5,strip.text.angle=0, strip.text.face="italic",
				strip.background = element_rect(color="white",fill="white"),
				legend.title.size=40, legend.title.face="italic", legend.title.align=0.5, 
				MEANbarheight=unit(3,"cm"),
				legend.text.size=40, legend.text.align=0, legend.key=element_blank(),
				legend.key.height=unit(3,"cm"), legend.key.width=unit(5,"cm"), legend.margin=unit(3,"cm"), 
				axis.title.size=60, axis.title.x.vjust=-10, axis.title.y.vjust=0.2, 
				axis.text.size=40, width=2000, height=1500, 
				family='serif') {

if (length(MEANvar) >  1) {
	
	dt <- cbind(summdat[,1:2],summdat[,MEANvar],summdat[,SDvar])
	MEANnames <- names(summdat)[MEANvar]
	SDnames <- names(summdat)[SDvar]
	row.names(dt) <- NULL
	
	# format data frame: x, y, variable, value
	data.long <- melt(dt, id.vars=names(dt)[c(1,2)])
	facetNum <- vector()
	facetNames <- vector()
	for ( i in 1:nrow(data.long) ) {
		num <- match(data.long$variable[i],MEANnames)
		if (is.na(num)) {
			num <- match(data.long$variable[i],SDnames)
		}
		facetNum <- c(facetNum,num)
		if (is.null(zVarsNames)==FALSE) {
			facetNames <- c(facetNames, zVarsNames[num])
		}
	}

	if (is.null(zVarsNames)==FALSE) {
		data.long <- cbind(data.long, facetNum, facetNames)
	} else {
		data.long <- cbind(data.long, facetNum)
	}

	row.names(data.long) <- NULL
	
	p1 <- ggplot(data.long, aes(x=factor(x),y=factor(y)))
	
	for (v in MEANnames){
		thisSD <- SDnames[match(v,MEANnames)]
		for (i in 1:2) {
			if (i == 1) {
				# draw MEAN
				p1 <- p1 + geom_tile(data=subset(data.long,facetNum==match(v,MEANnames) & variable==v),aes(fill=value))
			} else {
				# draw SD
				if (is.null(SDvar) == FALSE) {
					p1 <- p1 + geom_point(data=subset(data.long,facetNum==match(v,MEANnames) & variable==thisSD),aes(size=value),shape="O")
				}
			}
		}
		if (is.null(transect[1]) == FALSE) {
			p1 <- p1 + geom_vline(data=data.long,xintercept = transect[1], colour=transColour, linetype=transType, size=transSize)
		}
		if (is.null(transect[2]) == FALSE) {
			p1 <- p1 + geom_hline(data=data.long,yintercept = transect[2], colour=transColour, linetype=transType, size=transSize)
		}
	}
	# set scales
	p1 <- p1 + scale_fill_continuous(name=zMEANName,low=colRange[1],high=colRange[2]) +
		guides(fill = guide_colorbar(barheight=MEANbarheight)) 
	if  ( sdLeg==TRUE ) {
		p1 <- p1 + scale_size(name=zSDName, range=sdRange) +
			guides(fill = guide_colorbar(order = 1, barheight=MEANbarheight), size = guide_legend(order = 2,reverse=T))					
	} else {
		p1 <- p1 + scale_size(name=zSDName, range=sdRange,guide=F)
	}
	if (is.null(zVarsNames)==FALSE) {
		p1 <- p1 + facet_grid(facetNames~.)
	} else {
		p1 <- p1 + facet_grid(facetNum~.)
	}
	p2 <- p1 +
		labs(x = xName, y = yName) +
		theme_bw() +
		theme(plot.margin = unit(c(.01, .01, .05, .02), "npc"),
		strip.text = element_text(size=strip.text.size, face=strip.text.face, family=family, vjust=strip.text.vjust, hjust=strip.text.hjust, angle=strip.text.angle),
        	strip.background=strip.background,
		legend.title = element_text(size=legend.title.size, face=legend.title.face, family=family),
        	legend.title.align=legend.title.align,
        	legend.text = element_text(size=legend.text.size, family=family),
        	legend.text.align = legend.text.align,
        	legend.key = element_blank(),
        	legend.key.height = legend.key.height,
        	legend.key.width = legend.key.width,
       	legend.margin = legend.margin,
       	axis.title.x = element_text(size = axis.title.size, family=family, face="italic", vjust=-2),
       	axis.title.y = element_text(size = axis.title.size, family=family, face="italic", vjust=0.2),
       	axis.text.x = element_text(size = axis.text.size, family=family),
        	axis.text.y = element_text(size = axis.text.size, family=family))

} else {
	dt <- summdat[,c(1,2,MEANvar)]

	names(dt) <- c("x","y","MEANvar")

	row.names(dt) <- 1:nrow(summdat)

	if (is.null(SDvar) == FALSE) {
		SD <- summdat[,SDvar]
		dt <- cbind(dt,SD)
	}

	p1 <- ggplot(dt, aes(x=factor(x),y=factor(y)))

	if (is.null(SDvar) == FALSE) {
		if  ( sdLeg==FALSE ) {
			p1 <- p1 + geom_tile(data=dt,aes( fill=MEANvar )) +
			geom_point(data=dt,aes(size=SD),shape="O")
		} else {
			p1 <- p1 + geom_tile(data=dt,aes( fill=MEANvar )) +
			geom_point(data=dt,aes(size=SD),shape="O")
		}
	} else {
		p1 <- p1 + geom_tile(aes(fill=MEANvar))
	}

	if (is.null(transect[1]) == FALSE) {
		p1 <- p1 + geom_vline(data=dt,xintercept = transect[1], colour=transColour, linetype=transType, size=transSize)
	}
	if (is.null(transect[2]) == FALSE) {
		p1 <- p1 + geom_hline(data=dt,yintercept = transect[2], colour=transColour, linetype=transType, size=transSize)
	}

	# Scales
	p1 <- p1 + scale_fill_continuous(name=zMEANName,low=colRange[1],high=colRange[2]) +
		guides(fill = guide_colorbar(barheight=MEANbarheight)) 
	if  ( sdLeg==TRUE ) {
		p1 <- p1 + scale_size(name=zSDName, range=sdRange) +
			guides(fill = guide_colorbar(order = 1, barheight=MEANbarheight), size = guide_legend(order = 2,reverse=T))					
	} else {
		p1 <- p1 + scale_size(name=zSDName, range=sdRange,guide=F)
	}

	p2 <- p1 + labs(x = xName, y = yName) +
		theme_bw() +
		theme(plot.margin = unit(c(.01, .01, .05, .02), "npc"),
        	legend.title = element_text(size=legend.title.size, face=legend.title.face, family=family),
        	legend.title.align=legend.title.align,
        	legend.text = element_text(size=legend.text.size, family=family),
        	legend.text.align = legend.text.align,
        	legend.key = element_blank(),
        	legend.key.height = legend.key.height,
        	legend.key.width = legend.key.width,
       	legend.margin = legend.margin,
       	axis.title.x = element_text(size = axis.title.size, family=family, face="italic", vjust=-2),
       	axis.title.y = element_text(size = axis.title.size, family=family, face="italic", vjust=0.2),
       	axis.text.x = element_text(size = axis.text.size, family=family),
        	axis.text.y = element_text(size = axis.text.size, family=family))
}

if ("png" %in% outType){
	png(filename = paste(fName,".png"), width = width, height = height)
	print(p2)
	dev.off()
}

if ("obj" %in% outType){
	return(p2)
}

}