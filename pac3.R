#install.packages("lubridate")
#install.packages("fastDummies")
#install.packages("qdapTools")
#install.packages("gplots")
#install.packages("ggpubr")
#install.packages("randomForest")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggpubr")
#install.packages("ggplot2")        
#install.packages("ROSE")
#install.packages("compareGroups")
#install.packages("scales")
#install.packages("glm2")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("mlbench ")
#install.packages("vcd")
#install.packages('caTools')
#install.packages("psych")
#install.packages("GGally")
#install.packages("ROCR")
#install.packages("epiR")
#install.packages("pROC")
#install.packages("DMwR")
#install.packages("performanceEstimation")
#install.packages("smotefamily")
#install.packages("janitor")
library(janitor)
library(performanceEstimation)
library(smotefamily)
library(DMwR)
library(pROC)
library(epiR)
library(ROCR)
library(GGally)
library(rio)
library(psych) 
library(caTools)
library(rpart.plot)
library(rpart)
library(ggpubr)
library(readr)
library(rpart)
library(lubridate)
library(dplyr)
library(tidyverse)
library(fastDummies)
library(qdapTools)
library(gplots)
library(ggplot2)
library(caret)
library(randomForest)
library(dplyr)
library(tidyr)
library(ggpubr)
library(ggplot2) 
library(ROSE)
library(compareGroups)
library(scales)
library(glm2)
library(caret)
library(vcd)
require(pacman)
p_load(rio, tidyverse, compareGroups,devtools, sjPlot, janitor, glmnet)

#USUARIOS
#data set de usuarios, no tiene filas repetidas, 
usuarios<-read.csv2("C:/UOC/TFM/data/usuarios.txt", header = T)
usuarios$FECHA_REGISTRO<-as.Date(usuarios$FECHA_REGISTRO,"%d/%m/%y")
usuarios$FECHA_ALTA<-as.Date(usuarios$FECHA_ALTA,"%d/%m/%y")
usuarios$FECHA_CLIENTE<-as.Date(usuarios$FECHA_CLIENTE,"%d/%m/%y")
usuarios$CANAL_REGISTRO<-as.factor(usuarios$CANAL_REGISTRO)
usuarios$CANAL_REGISTRO[is.na(usuarios$CANAL_REGISTRO)] <- "7"
usuarios$IND_CLIENTE <-as.factor(usuarios$IND_CLIENTE )
usuarios$IND_ALTA<-as.factor(usuarios$IND_ALTA)
usuarios$BONDAD_EMAIL<-as.factor(usuarios$BONDAD_EMAIL)
usuarios$TIPOUSUARIO<-as.factor(usuarios$TIPOUSUARIO)
str(usuarios)
attach(usuarios)

#tratar las tabla USUARIOS primero hacer dummys
usuarios_dummy <- dummy_cols(usuarios, select_columns = c("TIPOUSUARIO","BONDAD_EMAIL","CANAL_REGISTRO"))
#dropeamos las variables originales que ya estan en dummy
usuarios_dummy1 <- subset(usuarios_dummy, select = -c(TIPOUSUARIO,BONDAD_EMAIL,USU_DEPARTAMENTO,USU_TIPO,USU_ESTADO,USU_CIIU,USU_TELF,FECHA_ALTA,FECHA_CLIENTE,CANAL_REGISTRO,FECHA_REGISTRO,IND_ALTA,TIPOEMAIL,IPCASOS,IP_Country,IP_Region,USU_TAMANIO))
usuarios_dummy1<-usuarios_dummy1
usuarios_dummy1<-usuarios_dummy1[!(usuarios_dummy1$ID_USUARIO=="7421377" | usuarios_dummy1$ID_USUARIO=="7542443"),]
attach(usuarios_dummy1)

#LOGINS
#data set de logins, si hay repetidos, 
logins<-read.csv2("C:/UOC/TFM/data/logins.txt", header = T)
logins$FECHALOGIN<-as.Date(logins$FECHALOGIN,"%d/%m/%y")
logins$FECHALOGINCOMPLETA <-strptime(logins$FECHALOGINCOMPLETA, "%d/%m/%y %H:%M:%S")
logins$ID_USUARIO<-as.numeric(logins$ID_USUARIO)
attach(logins)


#para reducirlas filas, hago un count de fecha login por cliente
count_logins <- logins %>%                             
  group_by(ID_USUARIO) %>%
  summarise(count_login = n_distinct(FECHALOGINCOMPLETA))

as.data.frame(count_logins)
count_logins$count_login[is.na(count_logins$count_login)] <- 0

count_logins<-count_logins[!(count_logins$ID_USUARIO=="7421377"),]



#VENTAS
#ventas<-read.table("C:/Users/oriol/Documents/data/ventas.txt", header = TRUE, sep = ";", dec = ",")
#head(ventas)
#str(ventas)
#ventas$FECHAVENTA<-as.Date(ventas$FECHAVENTA,"%d/%m/%y")
#Como me pone NA si paso directamente a factor, primero relleno los NAs con 0s y luego lo convierto a factor
#ventas$VP.Informe[is.na(ventas$VP.Informe)] <- 0
#ventas$VP.Informe<-as.factor(ventas$VP.Informe)
#ventas$BONO[is.na(ventas$BONO)] <- 0
#ventas$BONO<-as.factor(ventas$BONO)
#ventas$SUSCRIPCION[is.na(ventas$SUSCRIPCION)] <- 0
#ventas$SUSCRIPCION<-as.factor(ventas$SUSCRIPCION)
#ventas$VP.Listado[is.na(ventas$VP.Listado)] <- 0
#ventas$VP.Listado<-as.factor(ventas$VP.Listado)
#ventas$Comprador<-1
#ventas$Comprador<-as.factor(ventas$Comprador)


#CONSUMOS PROMOCIONALES:
cpromocionales<-read.csv2("C:/UOC/TFM/data/consumos_promocionales.txt", header = T)
cpromocionales$FECHACONSUMO<-as.Date(cpromocionales$FECHACONSUMO,"%d/%m/%y")
promocionales_dummy <- subset(cpromocionales, select = c("ID_USUARIO","DESCGRUPOPROD"))

#hago count de promos consumidas
count_promos<-tally(group_by(promocionales_dummy, ID_USUARIO))
names(count_promos)<-c("ID_USUARIO","contador_promos")
count_promos<-count_promos[!(count_promos$ID_USUARIO=="7421377" ),]

##############################################################################################################
######################  EMPEZAMOS MERGE tabla usuarios_logins_promos
######################
#primer merge entre usuarios y count logins
usuarios_logins<-merge(x = usuarios_dummy1, y = count_logins, by = "ID_USUARIO", all.x = TRUE)
usuarios_logins$count_login[is.na(usuarios_logins$count_login)] <- 0
#primer merge entre usuarios_logins Y count_promos
usuarios_logins_promos<-merge(x = usuarios_logins, y = count_promos, by = "ID_USUARIO", all.x = TRUE)
#reviso si despues de los merge, se han generado na el del data
sapply(usuarios_logins_promos, function(x) sum(is.na(x)))
#solo 23 registros de contador de promos que les asigno un 0
usuarios_logins_promos$contador_promos[is.na(usuarios_logins_promos$contador_promos)] <- 0
usuarios_logins_promos<-usuarios_logins_promos[,2:21]
attach(usuarios_logins_promos)

##############################################################################################################
######################  EMPEZAMOS MERGE tabla sindumis
######################
usu_log<-merge(x = usuarios, y = count_logins, by = "ID_USUARIO", all.x = TRUE)
usu_log$count_login[is.na(usu_log$count_login)] <- 0
sindumis<-merge(x = usu_log, y = count_promos, by = "ID_USUARIO", all.x = TRUE)
sindumis$contador_promos[is.na(sindumis$contador_promos)] <- 0

##############################################################################################################
######################  analisis previo de variabless
######################



# Histograma
str(usuarios_logins_promos)
dat <- clean_names(usuarios_logins_promos)

library(dplyr)

dat <- dat %>% 
  mutate_at(names(dat %>% select(tipousuario_pf :canal_registro_9)), as.factor)

head(dat)

#analisis comparativo 

res <- compareGroups(ind_cliente ~. , data = dat)
restab <- createTable(res)
restab
plot(restab)
#mostramos los OD
createTable(res, show.ratio = T)



resn <- compareGroups(ind_cliente ~.  , data = dat, method = c(count_login=2, contador_promos=2))
restabn <- createTable(resn)
restabn
#ploteamos toda la informacion que se deriva de la funcion compareGroups
plot(res[c(1, 12)])
#miramos variables continuas
continuous <-select_if(dat, is.numeric)
summary(continuous)


ggplot(continuous, aes(x = count_login)) +
  geom_density(alpha = 0.1, fill = "#FF6666") +xlim(10, 100)



data_adult_drop<-dat

#estandarizo


data_adult_rescale <- data_adult_drop %>%
  mutate_if(is.numeric, funs(as.numeric(scale(.))))
head(data_adult_rescale)


# miramos variables categoricas


factor <- data.frame(select_if(data_adult_rescale, is.factor))
ncol(factor)

# Create graph for each column
graph <- lapply(names(sindumis),
                function(x) 
                  ggplot(sindumis, aes(get(x))) +
                  geom_bar() +
                  theme(axis.text.x = element_text(angle = 90)))

graph[[10]]




#tabla correlacion

library(GGally)
# convierto a numerico
corr <- data.frame(lapply(dat, as.integer))
cor(corr$tipousuario_pf,corr$tipousuario_pj)
cor(corr$ind_cliente,corr$tipousuario_pj)
cor(corr$ind_cliente,corr$tipousuario_pf)

cor(corr$bondad_email_0,corr$bondad_email_20_2)
plot()


# Plot de corr

ggcorr(corr,
       method = c("pairwise", "spearman"),
       nbreaks = 6,
       hjust = 0.8,
       label = TRUE,
       label_size = 3,
       color = "grey50")

round(cor(corr),2)  

##############################################################################################################
######################  TRAIN AND TEST
######################


#divido el train y test
set.seed(1234)
create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}
data_train <- create_train_test(data_adult_rescale, 0.8, train = TRUE)
data_test <- create_train_test(data_adult_rescale, 0.8, train = FALSE)


dim(data_train)
dim(data_test)
summary(data_train)
summary(data_test)
str(data_train)


##############################################################################################################
######################  LOGISTICS MODEL primera parte, con 3 modelos definidos sin balancear
######################

#primer modelo
logit <- glm(ind_cliente ~tipousuario_pf+tipousuario_pj+tipousuario_px+bondad_email_20+bondad_email_10+bondad_email_0+bondad_email_1+bondad_email_9+bondad_email_20_2+canal_registro_1+canal_registro_2+canal_registro_3+canal_registro_4+canal_registro_6+canal_registro_7+canal_registro_8+canal_registro_9+count_login+contador_promos, data = data_train, family = 'binomial')
summary(logit)

#saco variables no significativas

logit1 <- glm(ind_cliente ~tipousuario_pf+bondad_email_20+bondad_email_10+bondad_email_0+bondad_email_9+canal_registro_1+canal_registro_4+canal_registro_6+canal_registro_7+count_login, data = data_train, family = 'binomial')
summary(logit1)




#como me sigue dando ese warning miro de sacar una de las numericas porque estan muy correlacionadas
cor(dat$contador_promos,dat$count_login) 


#confusion matrix
predict <- predict(logit1, data_test, type = 'response')

# confusion matrix
table_mat <- table(data_test$ind_cliente, predict > 0.5)
table_mat


confusionMatrix(table(ifelse(predict >0.5, 1,0), data_test$ind_cliente))


#accuracy
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

#precision
precision <- function(matrix) {
  # True positive
  tp <- matrix[2, 2]
  # false positive
  fp <- matrix[1, 2]
  return (tp / (tp + fp))
}

#recall
recall <- function(matrix) {
  # true positive
  tp <- matrix[2, 2]# false positive
  fn <- matrix[2, 1]
  return (tp / (tp + fn))
}


prec <- precision(table_mat)
prec
rec <- recall(table_mat)
rec

#Fscore
f1 <- 2 * ((prec * rec) / (prec + rec))
f1

#ROC

ROCRpred <- prediction(predict, data_test$ind_cliente)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2, 1.7))

##############################################################################################################
######################  LOGISTICS MODEL segunda parte,  Undersampling
######################

#Undersampling

#downsampling en training data
set.seed(1)
data_downsample <- ovun.sample(ind_cliente ~tipousuario_pf+bondad_email_20+bondad_email_10+bondad_email_0+bondad_email_9+canal_registro_1+canal_registro_4+canal_registro_6+canal_registro_7+count_login ,data = data_train ,method = "under")$data
# miramos el balance
table(data_downsample$ind_cliente)
ds_model <- glm(ind_cliente ~tipousuario_pf+bondad_email_20+bondad_email_10+bondad_email_0+bondad_email_9+canal_registro_1+canal_registro_4+canal_registro_6+canal_registro_7+count_login , data = data_downsample, family = "binomial")
ds_pred <- predict(object = ds_model, newdata = data_test , type = "response")


confusionMatrix(table(ifelse(ds_pred >0.5, 1,0), data_test$ind_cliente))


roc.curve(data_test$ind_cliente, predict)
roc.curve(data_test$ind_cliente, ds_pred, add.roc=TRUE, col=2)



legend(0.7,0.3,c("Original", "Down-Sampling"),
       lty = rep(1, 5),
       lwd = rep(2, 5),
       cex = .8,
       col = rocCols)



roc_pred <- prediction(predictions = ds_pred  , labels = data_test$ind_cliente)
roc_perf <- performance(roc_pred , "tpr" , "fpr")


plot(roc_perf,
     colorize = TRUE,
     print.cutoffs.at= seq(0,1,0.05),
     text.adj=c(-0.2,1.7))


##############################################################################################################
######################  LOGISTICS MODEL segunda parte, balanceamos ROSE
######################

#  ROSE

data_rose <- ROSE(ind_cliente ~tipousuario_pf+bondad_email_20+bondad_email_10+bondad_email_0+bondad_email_9+canal_registro_1+canal_registro_4+canal_registro_6+canal_registro_7+count_login , data=data_train)$data
# check (im)balance of new data
table(data_rose$ind_cliente)

rose_model <- glm(ind_cliente ~tipousuario_pf+bondad_email_20+bondad_email_10+bondad_email_0+bondad_email_9+canal_registro_1+canal_registro_4+canal_registro_6+canal_registro_7+count_login , data=data_rose, family="binomial")

rose_pred <- predict(rose_model, newdata=data_test, type="response")

roc_pred <- prediction(predictions = rose_pred  , labels = data_test$ind_cliente)
roc_perf <- performance(roc_pred , "tpr" , "fpr")


confusionMatrix(table(ifelse(rose_pred >0.5, 1,0), data_test$ind_cliente))


plot(roc_perf,
     colorize = TRUE,
     print.cutoffs.at= seq(0,1,0.05),
     text.adj=c(-0.2,1.7))

roc.curve(data_test$ind_cliente, predict)
roc.curve(data_test$ind_cliente, ds_pred, add.roc=TRUE, col=2)
roc.curve(data_test$ind_cliente, rose_pred, add.roc=TRUE, col="blue")


rocCols1 = c("black", rgb(1, 0, 0, .5), rgb(0, 0, 1, .5),"green",2)
legend(0.7,0.3,c("Original", "Down-Sampling","ROSE"),
       lty = rep(1, 5),
       lwd = rep(2, 5),
       cex = .8,
       col = rocCols1)


##############################################################################################################
######################  ARBOL DE CLASIFICACION
######################



summary(data_train)

treeimb <- rpart(ind_cliente ~tipousuario_pf+bondad_email_20+bondad_email_10+bondad_email_0+bondad_email_9+canal_registro_1+canal_registro_4+canal_registro_6+canal_registro_7 , data = data_train)
pred.treeimb <- predict(treeimb, newdata = data_test)

roc.curve(data_test$ind_cliente, pred.treeimb[,2], plotit = F)

#over sampling
data_balanced_over <- ovun.sample(ind_cliente ~tipousuario_pf+bondad_email_20+bondad_email_10+bondad_email_0+bondad_email_9+canal_registro_1+canal_registro_4+canal_registro_6+canal_registro_7, data = data_train, method = "over",N = 578336)$data
table(data_balanced_over$ind_cliente)
#under sampling
data_balanced_under <- ovun.sample(ind_cliente ~tipousuario_pf+bondad_email_20+bondad_email_10+bondad_email_0+bondad_email_9+canal_registro_1+canal_registro_4+canal_registro_6+canal_registro_7, data = data_train, method = "under", N = 3714, seed = 1)$data
table(data_balanced_under$ind_cliente)
#both sampling
data_balanced_both <- ovun.sample(ind_cliente ~tipousuario_pf+bondad_email_20+bondad_email_10+bondad_email_0+bondad_email_9+canal_registro_1+canal_registro_4+canal_registro_6+canal_registro_7, data = data_train, method = "both", p=0.5, N=367705, seed = 1)$data
table(data_balanced_both$ind_cliente)

data.rose <- ROSE(ind_cliente ~tipousuario_pf+bondad_email_20+bondad_email_10+bondad_email_0+bondad_email_9+canal_registro_1+canal_registro_4+canal_registro_6+canal_registro_7, data = data_train, seed = 1)$data
table(data.rose$ind_cliente)

#construir arbol 
tree.rose <- rpart(ind_cliente ~tipousuario_pf+bondad_email_20+bondad_email_10+bondad_email_0+bondad_email_9+canal_registro_1+canal_registro_4+canal_registro_6+canal_registro_7, data = data.rose)
tree.rose
summary(tree.rose)
printcp(tree.rose)
tree.over <- rpart(ind_cliente ~tipousuario_pf+bondad_email_20+bondad_email_10+bondad_email_0+bondad_email_9+canal_registro_1+canal_registro_4+canal_registro_6+canal_registro_7, data = data_balanced_over)
tree.under <- rpart(ind_cliente ~tipousuario_pf+bondad_email_20+bondad_email_10+bondad_email_0+bondad_email_9+canal_registro_1+canal_registro_4+canal_registro_6+canal_registro_7, data = data_balanced_under)
tree.under

tree.both <- rpart(ind_cliente ~tipousuario_pf+bondad_email_20+bondad_email_10+bondad_email_0+bondad_email_9+canal_registro_1+canal_registro_4+canal_registro_6+canal_registro_7, data = data_balanced_both)

rpart.plot(tree.rose)
rpart.plot(tree.over)
rpart.plot(tree.under)
rpart.plot(tree.both)

printcp(tree.rose)
#predicciones
pred.tree.rose <- predict(tree.rose, newdata = data_test)
pred.tree.over <- predict(tree.over, newdata = data_test)
pred.tree.under <- predict(tree.under, newdata = data_test)
pred.tree.both <- predict(tree.both, newdata = data_test)



#ROC ROSE
roc.curve(data_test$ind_cliente, pred.tree.rose[,2])
#ROC Undersampling
roc.curve(data_test$ind_cliente, pred.tree.under[,2],add.roc=TRUE, col=2)
rocCols1 = c("black", rgb(1, 0, 0, .5), rgb(0, 0, 1, .5),"green")
legend(0.7,0.3,c("ROSE", "Down-Sampling"),
       lty = rep(1, 5),
       lwd = rep(2, 5),
       cex = .8,
       col = rocCols1)


#ROC Both
roc.curve(data_test$ind_cliente, pred.tree.both[,2])
#ROC Oversampling
roc.curve(data_test$ind_cliente, pred.tree.over[,2])

####################################HAGO UN ARBOL, PERO ESTA VEZ DOWNSAMPLE DE BBDD ORIGINAL DIRECTAMENTE

new.df<-downSample(data_adult_rescale, data_adult_rescale$ind_cliente)
new.df<-new.df[,1:20]

smp_siz = floor(0.75*nrow(new.df))  
smp_siz  # 
set.seed(123)   
train_ind = sample(seq_len(nrow(new.df)),size = smp_siz)  
train =new.df[train_ind,] 
test=new.df[-train_ind,] 


prop.table(table(test$ind_cliente))


fit <- rpart(ind_cliente~.-count_login, data = train, method = 'class')
rpart.plot(fit, extra = 106)

predict_unseen <-predict(fit, test, type = 'class')

table_mat <- table(test$ind_cliente, predict_unseen)
table_mat


accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

precision <- function(matrix) {
  # verdadeo positivo
  tp <- matrix[2, 2]
  # falso positivo
  fp <- matrix[1, 2]
  return (tp / (tp + fp))
}
recall <- function(matrix) {
  # verdadero positivo
  tp <- matrix[2, 2]# falso positivo
  fn <- matrix[2, 1]
  return (tp / (tp + fn))
}

prec <- precision(table_mat)
prec
rec <- recall(table_mat)
rec

f1 <- 2 * ((prec * rec) / (prec + rec))
f1

accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, data_test, type = 'class')
  table_mat <- table(data_test$survived, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}


control <- rpart.control(minsplit = 1,
                         minbucket = round(5 / 3),
                         maxdepth = 10,
                         cp = 0)
tune_fit <- rpart(ind_cliente~.-count_login, data = train, method = 'class', control = control)
rpart.plot(tune_fit)
printcp(tune_fit)
plotcp(tune_fit)
accuracy_tune(tune_fit)


set.seed(1234)
ind <- sample(2, nrow(new.df), replace = T, prob = c(0.5, 0.5))
train <- new.df[ind == 1,]
test <- new.df[ind == 2,]

#Árbol clasificacion
tree <- rpart(ind_cliente ~.-count_login, data = train)
rpart.plot(tree)
printcp(tree)
plotcp(tree)

p <- predict(tree, train, type = 'class')
confusionMatrix(p, train$ind_cliente)

##############################################################################################################
######################  RANDOM FOREST
######################


dat <- data_adult_rescale %>% 
  mutate(ind_cliente = factor(ind_cliente, c(0,1), c("No", "Si"))) 

dat$ind_cliente =factor(as.character(dat$ind_cliente),levels = rev(levels(dat$ind_cliente)))
table(dat$ind_cliente)



library(caret)
set.seed(156)
n=nrow(dat)
indices=1:n
ient=sample(indices,floor(n*0.6))
ival=sample(setdiff(indices,ient),floor(n*0.15))
itest=setdiff(indices,union(ient,ival))
training = dat[ient,]
validation = dat[ival,]
testing = dat[itest,]
training_valid=rbind(training,validation)
dim(training)
dim(testing)
dim(validation)


Index= 1:nrow(training) #SE USARA EN trainControl
predictors = names(training)[names(training) != "ind_cliente"]
predictors #Nombres de las variables predictoras



testResults = data.frame(ind_cliente =testing$ind_cliente)
validResults = data.frame(ind_cliente =validation$ind_cliente)

fiveStats = function(...)
  c(twoClassSummary(...), defaultSummary(...))




set.seed(1410)
ctrlcv = trainControl(method = "cv",number=3,
                      classProbs = TRUE,
                      summaryFunction = fiveStats,
                      verboseIter=TRUE)



rfFit = train(ind_cliente ~ ., data = training,
              method = "rf",
              trControl = ctrlcv,
              do.trace=TRUE,
              tuneLength=3,
              metric = "Sens")

rfFit


#probabilidades estimadas 
validResults$RF = predict(rfFit, validation,type = "prob")[,1]
testResults$RF = predict(rfFit, testing,type = "prob")[,1]

library(pROC)

rfTestROC = roc(testResults$ind_cliente,testResults$RF,levels =rev(levels(testResults$ind_cliente)))
rfTestROC
plot(rfTestROC)


#matriz confusion
rfTestCM = confusionMatrix(predict(rfFit, testing),testResults$ind_cliente)
rfTestCM

#Downsampling

set.seed(1237)
downSampled = downSample(training[, -ncol(training)],training$ind_cliente)
dim(downSampled)
table(downSampled$Class)

downSampled_valid = downSample(validation[, -ncol(validation)],validation$ind_cliente)
dim(downSampled_valid)
table(downSampled_valid$Class)

downSampled_train_valid=rbind(downSampled,downSampled_valid )
dim(downSampled_train_valid)

table(downSampled_train_valid$Class)


#Upsampling
set.seed(1237)
upSampled = upSample(training[, -ncol(training)],training$ind_cliente)
dim(upSampled)
table(upSampled$Class)

upSampled_valid = upSample(validation[, -ncol(validation)],validation$ind_cliente)
dim(upSampled_valid)
table(upSampled_valid$Class)

upSampled_train_valid=rbind(upSampled,upSampled_valid )
dim(upSampled_train_valid)
table(upSampled_train_valid$Class)


#modeloDownsampling
ctrcvdown=ctrlcv
ctrcvdown$index=list(1:nrow(downSampled))
ctrcvdown$indexFinal=1:nrow(downSampled)
set.seed(1410)
rfDown = train(Class ~ .-ind_cliente, data = downSampled_train_valid,
               "rf",
               trControl = ctrcvdown,
               ntree = 100,do.trace=TRUE,
               tuneLength = 3,
               metric = "ROC")
rfDown

testResults$RFdown = predict(rfDown, testing,type = "prob")[,1]
rfDownROCT = roc(testResults$ind_cliente, testResults$RFdown,
                 levels = rev(levels(validResults$ind_cliente)))
rfDownROCT

matrixDOWN = confusionMatrix(predict(rfDown, testing),testResults$ind_cliente)
matrixDOWN

#modeloUpsampling
set.seed(1410)
ctrvalup=ctrlcv
ctrvalup$index=list(1:nrow(upSampled))
ctrvalup$indexFinal=1:nrow(upSampled)
rfUp = train(Class ~ .-ind_cliente, data = upSampled_train_valid,
             "rf",
             trControl = ctrvalup,
             ntree = 100,
             tuneLength = 3, 
             do.trace=TRUE,
             metric = "ROC")
rfUp

validResults$RFup = predict(rfUp, validation,type = "prob")[,1]
testResults$RFup = predict(rfUp, testing,type = "prob")[,1]
rfUpROC = roc(validResults$ind_cliente, validResults$RFup,levels = rev(levels(validResults$ind_cliente)))
rfUpROC

matrixUP = confusionMatrix(predict(rfUp, testing),testResults$ind_cliente)
matrixUP

rfUpROCT = roc(testResults$ind_cliente, testResults$RFup,levels = rev(levels(testResults$ind_cliente)))
rfUpROCT


rocCols = c("black", rgb(1, 0, 0, .5), rgb(0, 0, 1, .5),
            "green","yellow")
#REPRESENTAR ALGUNAS CURVAS ROC TEST
plot(roc(testResults$ind_cliente, testResults$RF,
         levels = rev(levels(testResults$ind_cliente))),
     col = rocCols[1])
plot(roc(testResults$ind_cliente, testResults$RFdown,
         levels = rev(levels(testResults$ind_cliente))),
      col = rocCols[2],add = TRUE)
plot(roc(testResults$ind_cliente, testResults$RFup,
         levels = rev(levels(testResults$ind_cliente))),
      col = rocCols[3],add = TRUE)
legend(0.2,0.6,c("Original", "Down-Sampling ","Up-sampling"),
       lty = rep(1, 5),
       lwd = rep(2, 5),
       cex = .8,
       col = rocCols)








