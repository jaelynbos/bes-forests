x<-c(1:10)
y<-vector(mode="numeric",length=10)

for (i in 1:length(x)){
	y[i]<-x[i]*3
	print(c(i,y))
}

z<-cbind(c(1,2,3),c(4,5,6),c(7,8,9))
m<-vector(mode="numeric",length=ncol(z))

for(i in 1:ncol(z)){
	m[i]<-max(z[i,])	
	print(i)
}

m

