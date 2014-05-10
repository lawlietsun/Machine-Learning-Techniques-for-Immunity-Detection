path = "/Users/yuesun/LawlietSun/UCL/BSc Computer Science-UCL/Year 3/COMP3091-Final Year Individual Projects for 2013-14/yue-data/1mers-q10000k100/"
path = "/Users/yuesun/LawlietSun/UCL/BSc Computer Science-UCL/Year 3/COMP3091-Final Year Individual Projects for 2013-14/yue-data/3mers-q10000k100/"
path = "/Users/yuesun/LawlietSun/UCL/BSc Computer Science-UCL/Year 3/COMP3091-Final Year Individual Projects for 2013-14/yue-data/4mers-q10000k100/"

unt_1 <- read.table(paste(path,"results_unt_1.txt", sep=""), header=FALSE, sep=",")
unt_2 <- read.table(paste(path,"results_unt_2.txt", sep=""), header=FALSE, sep=",")
unt_3 <- read.table(paste(path,"results_unt_3.txt", sep=""), header=FALSE, sep=",")
unt_4 <- read.table(paste(path,"results_unt_4.txt", sep=""), header=FALSE, sep=",")
unt_5 <- read.table(paste(path,"results_unt_5.txt", sep=""), header=FALSE, sep=",")
unt_6 <- read.table(paste(path,"results_unt_6.txt", sep=""), header=FALSE, sep=",")

d5_1 <- read.table(paste(path,"results_d5_1.txt", sep=""), header=FALSE, sep=",")
d5_2 <- read.table(paste(path,"results_d5_2.txt", sep=""), header=FALSE, sep=",")
d5_3 <- read.table(paste(path,"results_d5_3.txt", sep=""), header=FALSE, sep=",")
d5_4 <- read.table(paste(path,"results_d5_4.txt", sep=""), header=FALSE, sep=",")
d5_5 <- read.table(paste(path,"results_d5_5.txt", sep=""), header=FALSE, sep=",")
d5_6 <- read.table(paste(path,"results_d5_6.txt", sep=""), header=FALSE, sep=",")

d14_1 <- read.table(paste(path,"results_d14_1.txt", sep=""), header=FALSE, sep=",")
d14_2 <- read.table(paste(path,"results_d14_2.txt", sep=""), header=FALSE, sep=",")
d14_3 <- read.table(paste(path,"results_d14_3.txt", sep=""), header=FALSE, sep=",")
d14_4 <- read.table(paste(path,"results_d14_4.txt", sep=""), header=FALSE, sep=",")
d14_5 <- read.table(paste(path,"results_d14_5.txt", sep=""), header=FALSE, sep=",")
d14_6 <- read.table(paste(path,"results_d14_6.txt", sep=""), header=FALSE, sep=",")

m2_1 <- read.table(paste(path,"results_m2_1.txt", sep=""), header=FALSE, sep=",")
m2_2 <- read.table(paste(path,"results_m2_2.txt", sep=""), header=FALSE, sep=",")
m2_3 <- read.table(paste(path,"results_m2_3.txt", sep=""), header=FALSE, sep=",")
m2_4 <- read.table(paste(path,"results_m2_4.txt", sep=""), header=FALSE, sep=",")
m2_5 <- read.table(paste(path,"results_m2_5.txt", sep=""), header=FALSE, sep=",")
m2_6 <- read.table(paste(path,"results_m2_6.txt", sep=""), header=FALSE, sep=",")

library("e1071")
numpersample <- 6
kernelfn <- "linear"

#####################
## TEST ON UNT 1-6 ##
#####################

for (i in 1:numpersample){

	v <- c(1:numpersample)
	v <- v[-i]

	## BUILD TEST SET ##

	test <- eval(as.name(paste("unt_",i,sep="")))
	test <- as.data.frame(test)

	## BUILD TRAINING SET ##

	training <- c()
	for (j in 1:numpersample){
		training <- rbind(training,eval(as.name(paste("d5_",j,sep=""))))
		training <- rbind(training,eval(as.name(paste("d14_",j,sep=""))))
		training <- rbind(training,eval(as.name(paste("m2_",j,sep=""))))
		training <- as.data.frame(training)
	}
	t <- dim(training)[1]

	for (j in v){
		training <- rbind(training,eval(as.name(paste("unt_",j,sep=""))))
		training <- as.data.frame(training)
	}
	ut <- dim(training)[1]-t
	
	##Scale trainging set and testing set##
	
	training <- scale(training)
	
	test <- scale(test,attr(training,"scaled:center"),attr(training,"scaled:scale"))
		
	training <- cbind( c(rep(1,t),rep(-1,ut)), training)
	
	colnames(training)[1] <- "class"

	## CREATE SVM CLASSIFIER ##

	cdr3model <- svm( class ~ ., data = training, type = "C-classification", kernel = kernelfn, scale=FALSE)

	## TEST SVM WITH PREDICT() ##
	
	predictions <- predict(cdr3model, test )
	print(table(predictions))
	
####UPDATE THE WRIGHT FIRST THEN RUN THE CODE INCLUDING THE PARTS BELOW TOGETHER###
	
	#update the training set without the first col which the class
	newtraining1 <- training[,-1] %*% diag(neww[,1])
	newtraining1 <- as.data.frame(newtraining1)
	
	#update test data
	newtest <- test %*% diag(neww[,1])

	#combine afterwords
	newtraining <- cbind(training[,1], newtraining1)
		
	colnames(newtraining)[1] <- "class"	
	
	newcdr3model <- svm( class ~ ., data = newtraining, type = "C-classification", kernel = kernelfn, scale=FALSE)

	## TEST SVM WITH PREDICT() ##

	newpredictions <- predict(newcdr3model, newtest)
	print(table(newpredictions))
}

######################
## TEST ON DAY5 1-3 ##
######################

for (i in 1:numpersample){

	v <- c(1:numpersample)
	v <- v[-i]

	## BUILD TEST SET ##

	test <- eval(as.name(paste("d5_",i,sep="")))
	test <- as.data.frame(test)

	## BUILD TRAINING SET ##

	training <- c()
	for (j in 1:numpersample){
		training <- rbind(training,eval(as.name(paste("unt_",j,sep=""))))
		training <- as.data.frame(training)
	}
	ut <- dim(training)[1]

	for (j in 1:numpersample){
		training <- rbind(training,eval(as.name(paste("d14_",j,sep=""))))
		training <- rbind(training,eval(as.name(paste("m2_",j,sep=""))))
		training <- as.data.frame(training)
	}
	
	for (j in v){
		training <- rbind(training,eval(as.name(paste("d5_",j,sep=""))))
		training <- as.data.frame(training)
	}
	
	t <- dim(training)[1]-ut
	
	training <- scale(training)
	
	attr1 <- attr(training,"scaled:center")
	
	attr2 <- attr(training,"scaled:scale")
	
	test <- scale(test,attr1,attr2)	
		
	training <- cbind( c(rep(1,t),rep(-1,ut)), training)
	
	colnames(training)[1] <- "class"

	## CREATE SVM CLASSIFIER ##

	cdr3model <- svm( class ~ ., data = training, type = "C-classification", kernel = kernelfn, scale=FALSE)
	
	predictions <- predict(cdr3model, test)
	print(table(predictions))
	
####UPDATE THE WRIGHT FIRST THEN RUN THE CODE INCLUDING THE PARTS BELOW TOGETHER###

	newtraining1 <- training[,-1] %*% diag(neww[,1])
	newtraining1 <- as.data.frame(newtraining1)
	
	#update test data
	newtest <- test %*% diag(neww[,1])

	#combine afterwords
	newtraining <- cbind(training[,1], newtraining1)
		
	colnames(newtraining)[1] <- "class"	
	
	newcdr3model <- svm( class ~ ., data = newtraining, type = "C-classification", kernel = kernelfn, scale=FALSE)

	## TEST SVM WITH PREDICT() ##

	newpredictions <- predict(newcdr3model, newtest)
	print(table(newpredictions))
}

#######################
## TEST ON DAY14 1-3 ##
#######################

for (i in 1:numpersample){

	v <- c(1:numpersample)
	v <- v[-i]

	## BUILD TEST SET ##

	test <- eval(as.name(paste("d14_",i,sep="")))
	test <- as.data.frame(test)


	## BUILD TRAINING SET ##

	training <- c()
	for (j in 1:numpersample){
		training <- rbind(training,eval(as.name(paste("unt_",j,sep=""))))
		training <- as.data.frame(training)
	}
	numunt <- dim(training)[1]

	for (j in 1:numpersample){
		training <- rbind(training,eval(as.name(paste("d5_",j,sep=""))))
		training <- as.data.frame(training)
	}
	numd5 <- dim(training)[1]-numunt

	for (j in 1:numpersample){
		training <- rbind(training,eval(as.name(paste("m2_",j,sep=""))))
		training <- as.data.frame(training)
	}
	numm2 <- dim(training)[1]-numunt-numd5

	for (j in v){
		training <- rbind(training,eval(as.name(paste("d14_",j,sep=""))))
		training <- as.data.frame(training)
	}
	numd14 <- dim(training)[1]-numunt-numd5-numm2

	#scale the training and test sets

	training <- scale(training)

	attr1 <- attr(training,"scaled:center")
	
	attr2 <- attr(training,"scaled:scale")
	
	test <- scale(test,attr1,attr2)
					
	training <- cbind( c(rep(1,numd5),rep(1,numd14),rep(1,numm2),rep(-1,numunt)), training)
	
	colnames(training)[1] <- "class"

	## CREATE SVM CLASSIFIER ##

	cdr3model <- svm( class ~ ., data = training, type = "C-classification", kernel = kernelfn, scale=FALSE)

	predictions <- predict(cdr3model, test)
	print(table(predictions))

####UPDATE THE WRIGHT FIRST THEN RUN THE CODE INCLUDING THE PARTS BELOW TOGETHER###

	newtraining1 <- training[,-1] %*% diag(neww[,1])
	newtraining1 <- as.data.frame(newtraining1)
	
	#update test data
	newtest <- test %*% diag(neww[,1])
	
	#combine afterwords
	newtraining <- cbind(training[,1], newtraining1)
		
	colnames(newtraining)[1] <- "class"	
	
	newcdr3model <- svm( class ~ ., data = newtraining, type = "C-classification", kernel = kernelfn, scale=FALSE)

	## TEST SVM WITH PREDICT() ##

	newpredictions <- predict(newcdr3model, newtest)
	print(table(newpredictions))
}

########################
## TEST ON MONTH2 1-3 ##
########################

for (i in 1:numpersample){

	v <- c(1:numpersample)
	v <- v[-i]

	## BUILD TEST SET ##

	test <- eval(as.name(paste("m2_",i,sep="")))
	test <- as.data.frame(test)


	## BUILD TRAINING SET ##
	
	training <- c()
	for (j in 1:3){
		training <- rbind(training,eval(as.name(paste("unt_",j,sep=""))))
		training <- as.data.frame(training)
	}
	numunt <- dim(training)[1]

	for (j in 1:numpersample){
		training <- rbind(training,eval(as.name(paste("d5_",j,sep=""))))
		training <- as.data.frame(training)
	}
	numd5 <- dim(training)[1]-numunt

	for (j in 1:numpersample){
		training <- rbind(training,eval(as.name(paste("d14_",j,sep=""))))
		training <- as.data.frame(training)
	}
	numd14 <- dim(training)[1]-numunt-numd5

	for (j in v){
		training <- rbind(training,eval(as.name(paste("m2_",j,sep=""))))
		training <- as.data.frame(training)
	}
	numm2 <- dim(training)[1]-numunt-numd5-numd14

	training <- scale(training)
	
	attr1 <- attr(training,"scaled:center")
	
	attr2 <- attr(training,"scaled:scale")
	
	test <- scale(test,attr1,attr2)

	training <- cbind( c(rep(1,numd5),rep(1,numd14),rep(1,numm2),rep(-1,numunt)), training)

	colnames(training)[1] <- "class"

	## CREATE SVM CLASSIFIER ##

	cdr3model <- svm( class ~ ., data = training, type = "C-classification", kernel = kernelfn, scale=FALSE)

	predictions <- predict(cdr3model, test)
	print(table(predictions))

####UPDATE THE WRIGHT FIRST THEN RUN THE CODE INCLUDING THE PARTS BELOW TOGETHER###

	newtraining1 <- training[,-1] %*% diag(neww[,1])
	newtraining1 <- as.data.frame(newtraining1)
	
	#update test data
	newtest <- test %*% diag(neww[,1])
	
	#combine afterwords
	newtraining <- cbind(training[,1], newtraining1)
		
	colnames(newtraining)[1] <- "class"	
	
	newcdr3model <- svm( class ~ ., data = newtraining, type = "C-classification", kernel = kernelfn, scale=FALSE)

	## TEST SVM WITH PREDICT() ##

	newpredictions <- predict(newcdr3model, newtest)
	print(table(newpredictions))
}