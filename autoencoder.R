require(quantmod)
require(keras)

#load("nifty.RData")

# get the technicals indicators -----------------------------------------
n <- 20
y <- ADX(HLC(x),n=n); adx.sig <- (y$DIp - y$DIn) #41
y <- aroon(HL(x),n=n); aroon.sig <- y$oscillator #13
y <- BBands(HLC(x),n=n); bbands.sig <- y$pctB -0.5 #75
y <- CCI(HLC(x),n=n); cci.sig <- y$cci #56
y <- CMO(Cl(x),n=n); cmo.sig <- y #17
y <- DonchianChannel(HL(x),n=n); don.sig <- Cl(x)-y$mid #65
y <- KST(Cl(x)); kst.sig <- y$kst #36
y <- MACD(Cl(x),nFast=round(n/2),nSlow=n); macd.sig <- y$macd - y$signal #21
y <- RSI(Cl(x),n=n); rsi.sig <- y$EMA-50 #9
y <- SMA(Cl(x),min(1,round(n/5))) - SMA(Cl(x),n=n); maxover.sig <- y #83
signals <- data.frame(adx.sig,aroon.sig,bbands.sig,cci.sig,cmo.sig,
                      don.sig,kst.sig,macd.sig,rsi.sig,maxover.sig)
colnames(signals) <- c("adx","aroon","bbands","cci","cmo","don",
                       "kst","macd","rsi","mom")
if(is.xts(x))signals <- as.xts(signals,index(x))
signals <- na.omit(signals)
signals <- scale(signals)

# get the PCA-----------------------------------------------------------
pca <- prcomp(signals, center = T, scale. = T)
loadings <- data.frame(pca$rotation)
factors <- data.frame(matrix(0,matrix(NROW(signals),3)))
for(i in 1:3){
  PC <- as.matrix(loadings[,i])
  factors[,i] <- as.matrix(signals) %*% PC
  colnames(factors)[i] <- paste("PC",i,sep="")
}

# set up the test/ train set -------------------------------------------
#idx <- sample(NROW(signals),round(0.8*NROW(x)))
idx <- 1:(round(0.9*NROW(signals)))
train_data <- as.matrix(signals[idx,])
test_data <- as.matrix(signals[-idx,])

# parameters -----------------------------------------------------------
batch_size <- 64
original_dim <- 10
latent_dim <- 3
intermediate_dim <- 5
epochs <- 50

# models ---------------------------------------------------------------
input <- layer_input(original_dim)
hidden1 <- layer_dense(input, intermediate_dim, activation = "relu")
coded <- layer_dense(hidden1, latent_dim)
decoder_input <- layer_input(shape = latent_dim)
hidden2 <- layer_dense(decoder_input, intermediate_dim, activation = "relu")
generator <- layer_dense(hidden2, original_dim)
hidden3 <- layer_dense(coded, intermediate_dim, activation = "relu")
decoded <- layer_dense(hidden3, original_dim)

encoder <- keras_model(input,coded)
decoder <- keras_model(decoder_input,generator)
autoencoder <- keras_model(input,decoded)

my_loss_fn <- function(input,decoded){
  loss <- loss_mean_squared_error(input,decoded)
}
  
autoencoder %>% compile(
  loss = my_loss_fn,
  optimizer = 'adam'
)

# model training -----------------------------------------------------
autoencoder %>% fit(
  train_data, train_data, 
  shuffle = TRUE, 
  epochs = epochs, 
  batch_size = batch_size, 
  validation_data = list(test_data, test_data)
)

# model output -------------------------------------------------------
data_encoded <- predict(encoder, signals, batch_size = batch_size)
data_decoded <- predict(decoder,data_encoded,batch_size=batch_size)
View(round(cor(factors,data_encoded),1))
View(round(cor(as.matrix(signals),data_decoded),1))

# plot results -------------------------------------------------------
par(mfrow=c(3,3))
plot(factors[,1],data_encoded[,1],main="PC1 vs 1st latent Dim")
plot(factors[,1],data_encoded[,2],main="PC1 vs 2nd latent Dim")
plot(factors[,1],data_encoded[,3],main="PC1 vs 3rd latent Dim")
plot(factors[,2],data_encoded[,1],main="PC2 vs 1st latent Dim")
plot(factors[,2],data_encoded[,2],main="PC2 vs 2nd latent Dim")
plot(factors[,2],data_encoded[,3],main="PC2 vs 3rd latent Dim")
plot(factors[,3],data_encoded[,1],main="PC3 vs 1st latent Dim")
plot(factors[,3],data_encoded[,2],main="PC3 vs 2nd latent Dim")
plot(factors[,3],data_encoded[,3],main="PC3 vs 3rd latent Dim")



