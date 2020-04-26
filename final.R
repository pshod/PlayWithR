print("Today we will learn to import data from a csv file and play around with it")

# We will first extract from a .csv for AIDS

setwd("C:/Users/pshodang/OneDrive - Ciena Corporation/Documents/Private") #Set this to a value convinient to you
aids_data = read.csv("aids.csv") # ?read.csv() for more info

NumColList <- function(v, header) {
	data = table(factor(v))
	labels = levels(factor(v))
        filename = paste(header, ".png", "")

	frequency = vector()
	for (i in 1:length(labels)) {
		frequency = c(frequency, data[[i]])
	}

	png(filename)
	barplot(frequency, names.arg = labels)
        dev.off()	

	list(Type = mode(v), Mean = mean(v), Median = median(v), Max = max(v), Min = min(v), data, filename)
}



#Generic function to print summary of a data frame
summarise_frame <- function(data) {
	colNames = names(data)
	n_col = ncol(data)  # n_col = colNames

	frameSummary = array(list(), n_col)

	for (i in 1:length(colNames)) {
		assign(colNames[i], data[,i])
		if (is.numeric(get(colNames[1])))
			elem = NumColList(get(colNames[i]), colNames[i])
		else 
			elem = StringColList(get(colNames[1]))

		frameSummary[[i]] = elem
	}

	frameSummary
}


print(summarise_frame(aids_data))


