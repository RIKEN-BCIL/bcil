#! /usr/bin/Rscript
#
# Create Shewhart individuals control chart, also known as x-chart and mr-chart
# Takuya Hayashi, RIKEN BDR 2022
#
# Following libraries are required: ggplot2, ggQC, gridExtra. 
# To install packages:
# sudo R
# >install.packages(c("ggplot2", "ggQC","gridExtra"))
#

args <- commandArgs(trailingOnly=TRUE)

stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

if (length(args)==0) {
  cat("\n Create Shewhart individuals control chart (x-chart, mr-chart, c-chart).\n\n")
  cat(" Usage: bcil_qcplot.R <input.csv> <output.png> [options]\n\n")
  cat(" Options:\n")
  cat("   -x: create x-chart (default).\n")
  cat("   -m: create mr-chart\n")
  cat("   -c: create c-chart\n\n")
  
  cat(" Input csv should contain at least 3 columns, each separated by\n")
  cat(" ('Class',ordinary number, variable) with a header in the first raw.\n")
  cat(" The header name of 'Class' should not be changed, while the second\n")
  cat(" and third header name can be changed. Values of 'Class' may be\n")
  cat(" usually categorical and contain characters, while those of 2nd are\n")
  cat(" ordered number (e.g. integer, time, continuous) and 3rd, continous\n")
  cat(" or count data. The file can contain 4th or higher order column\n")
  cat(" but the script will not use this information. Example input.csv is\n") 
  cat(" shown for the first 6 rows as follows:\n\n")
  cat(" Class,Subjects,BrainVolume\n")
  cat(" RIKENBDR-PrismaVE11C-HARP,01,1340\n")
  cat(" RIKENBDR-PrismaVE11C-HARP,02,1420\n")
  cat(" RIKENBDR-PrismaVE11C-HARP,03,1380\n")
  cat(" TokyoU-PrismaVE11C-HARP,01,1410\n")
  cat(" TokyoU-PrismaVE11C-HARP,02,1420\n")
  cat(" TokyoU-PrismaVE11C-HARP,03,1360\n")
  cat(" ....\n\n")
  cat(" Output will be x-chart and mR-chart separated by 'Class'.\n\n")
  stop_quietly()
} 

library(ggplot2)
library(ggQC)
library(gridExtra)

datfile<-args[1]
outpng=args[2]

opt <- "-x"
if (length(args)==3) {
	opt=args[3]
}

#dat <- read.table(datfile, sep = "", header = TRUE)   # in case reading 'space'-limited data
dat <-read.csv(datfile)
Xval=names(dat)[2]
Yval=names(dat)[3]

if (opt=="-x") {

	# x-chart
	XmR <-	ggplot(data = dat, mapping = aes_string(x = as.name(Xval), y = as.name(Yval))) + geom_line() + geom_point() + 
		scale_x_continuous(breaks = round(seq(0,max(dat[,2]), by = 10),1)) +
		stat_QC(method = "XmR", auto.label = T, label.digits = 2, show.1n2.sigma = T) +
		ylab(paste("x-chart")) +
		theme(panel.background = element_rect(fill = "transparent",color = NA), axis.text = element_text(angle = 90,color="blue",size=8,face=3), plot.background = element_rect(fill = "transparent",color = NA)) +
		facet_grid(. ~ Class)  # Varible name of the first column of dat should be 'Class'

	ggsave(file = outpng, plot = XmR, dpi = 100, width = 18, height = 2, bg = "transparent")

} else if (opt=="-m") {
	# mr-chart
	# Varible name of the first column of dat should be 'Class'
	mR <-	ggplot(data = dat, mapping = aes_string(x = as.name(Xval), y = as.name(Yval))) +
		scale_x_continuous(breaks = round(seq(0,max(dat[,2]), by = 10),1)) +
		stat_QC(method = "mR", auto.label = T, label.digits = 2, show.1n2.sigma = T) +
		ylab(paste("mr-chart")) +
		theme(panel.background = element_rect(fill = "transparent",color = NA), axis.text = element_text(angle = 90,color="blue",size=8,face=3), plot.background = element_rect(fill = "transparent",color = NA)) +
		facet_grid(. ~ Class) 

	#outplot=grid.arrange(XmR, mR, ncol=1)
	#ggsave(file = outpng, plot = outplot, dpi = 100, width = 18, height = 4)
	ggsave(file = outpng, plot = mR, dpi = 100, width = 18, height = 2)

} else if (opt=="-c") {

	# c-chart
	c_base <- ggplot(data = dat, aes_string(x=as.name(Xval), y =  as.name(Yval))) + geom_point() + geom_line() +
		scale_x_continuous(breaks = round(seq(0,max(dat[,2]), by = 10),1)) +
		stat_QC(method = "c", auto.label = T, label.digits = 2) + 
		ylab(paste("c-chart")) +
		theme(panel.background = element_rect(fill = "transparent",color = NA), axis.text = element_text(angle = 90,color="blue",size=8,face=3), plot.background = element_rect(fill = "transparent",color = NA)) +
		facet_grid(. ~ Class)

	ggsave(file = outpng, plot = c_base, dpi = 100, width = 18, height = 2)

} else {
  cat("ERROR: unknown option = ", opt, "\n")
  stop_quietly()
}
