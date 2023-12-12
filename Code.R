library(ggplot2)
library(ISLR)


data <- read.csv("transposed_data.csv")

dim(data)
data[1:6,1:6]

pca_prcomp<- prcomp(t(data), center = TRUE, scale. = FALSE)

plot(pca_prcomp)

summary(pca_prcomp)

pca_prcomp$sdev

cumsum(pca_prcomp$sdev^2)/sum(pca_prcomp$sdev^2)

names(pca_prcomp)

head(pca_prcomp$x[,1:2])

PC1_and_PC2<- data.frame(PC1=pca_prcomp$x[,1], PC2= pca_prcomp$x[,2], type = rownames(pca_prcomp$x))


ggplot(PC1_and_PC2, aes(x=PC1, y=PC2, col=type)) + geom_point() + geom_text(aes(label = type), hjust=0, vjust=0)


pca_prcomp$rotation[1:6, 1:6]

X<- t(scale(t(data),center=TRUE,scale=FALSE))


sv<- svd(t(X))
U<- sv$u
V<- sv$v
D<- sv$d


## U are un-scaled PC, Z is scaled
Z<- t(X)%*%V

## PCs
Z[1:6, 1:6]

pca_prcomp$x[1:6, 1:6]

V[1:6, 1:6]

pca_prcomp$rotation[1:6, 1:6]


pc_dat<- data.frame(type = rownames(Z), PC1 = Z[,1], PC2= Z[,2])
ggplot(pc_dat,aes(x=PC1, y=PC2, col=type)) + geom_point() + geom_text(aes(label = type), hjust=0, vjust=0)


varex = 0
cumvar = 0
denom = sum(D^2)
for(i in 1:length(D)){
  varex[i] = D[i]^2/denom
  cumvar[i] = sum(D[1:i]^2)/denom
}

## variance explained by each PC cumulatively
cumvar


par(mfrow=c(1,2))
plot(1:length(D),varex,type="l",lwd=2,xlab="PC",ylab="% Variance Explained")
plot(1:length(D),cumvar,type="l",lwd=2,xlab="PC",ylab="Cummulative Variance Explained")




