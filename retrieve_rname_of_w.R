t = 0
a = 0
x<- c()

for(b in neww[,1]){
	t = t + 1
	if(b != 0){
				x <- c(x, rownames(neww)[t])
				x <- paste(x, collapse=", ")
				a = a + 1
	}
}

####print x#####
x
