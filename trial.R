print ("I am gonna learn R")

temp = c(14.2, 16.4, 11.9, 15.2, 18.5, 22.1, 19.4, 25.1, 23.4, 18.1, 22.6, 17.2)

sales = c(215, 325, 185, 332, 406, 522, 412, 614, 544, 421, 445, 408)

plot(temp, sales)

calc_correl <- func(a, b) {
	sum_a = sum(a)
	sum_b = sum(b)
	sum_aSq = sum(a^2)
	sum_bSq = sum(b^2)
	sum_ab = sum(a * b)
	n = length(a)
	
	r_ab = n*sum_ab-sum_a*sum_b / ((n*sum_aSq - sum_a^2)^0.5 * (n*sum_bSq - sum_b^2)^0.5)

	r_ab
}

r_TempSales = calc_correl(temp, sales)

print("correlation between temp and sales is", r_TempSales, 

#pdf(file = "filenameyoudesire.pdf", width = 8, height = 8)
#dev.off()

print(length(temp))
print(length(sales))




