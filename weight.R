w <- t(cdr3model$SV) %*% (cdr3model$coefs)
	
w = sqrt(abs(w))
	
#update the training set without the first col which the class
newtraining1 <- training[,-1] %*% diag(w[,1])
newtraining1 <- as.data.frame(newtraining1)

#combine afterwords
newtraining <- cbind(training[,1], newtraining1)
		
colnames(newtraining)[1] <- "class"	
	
newcdr3model <- svm( class ~ ., data = newtraining, type = "C-classification", kernel = kernelfn, scale=FALSE)

######Re-weight n times#######

times = 40 #This may change depend on differet data#
	
for(n in 1:times){
	neww <- t(newcdr3model$SV) %*% (newcdr3model$coefs)
	
	neww = sqrt(abs(neww))
		
	#update the training set without the first col(class)
	newtraining1 <- as.matrix(newtraining1)
	newtraining1 <- newtraining1 %*% diag(neww[,1])
	newtraining1 <- as.data.frame(newtraining1)	
	
	#combine afterwords
	newtraining <- cbind(newtraining[,1], newtraining1)
		
	colnames(newtraining)[1] <- "class"	
		
	newcdr3model <- svm( class ~ ., data = newtraining, type = "C-classification", kernel = kernelfn, scale=FALSE)
}