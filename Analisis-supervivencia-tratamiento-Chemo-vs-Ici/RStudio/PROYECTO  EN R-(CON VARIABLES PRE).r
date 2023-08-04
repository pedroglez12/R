library(gplots)
library(foreign)
library(car)
library(plotly)
library(ggpubr)
library(ggplot2)
library(reprex)
library(dplyr)
library(readxl)
library(readr)
library(rpart)
library(glmulti)
library(caret)
library(InformationValue)
library(rvest)
library(stringi)
library(stringr)
library(dplyr)
library(fmsb)
library(dlookr)
library(ggrepel)
library(rattle)
library(corrplot)
library(factoextra)
library(mclust)
library(FactoMineR)
library(tidyr)
library(GGally)
library(gridExtra)
library(grid)
library(FSelector)
library(mlbench)
library(RRF)
library(wsrf)
library(Boruta)
options(warn=-1)

options(repr.plot.width = 18, repr.plot.height = 10) 

options(repr.matrix.max.rows=600, repr.matrix.max.cols=200) 

datos <- as.data.frame(read_excel("C:/Users/34625/Downloads/PROYECTOS DE TRABAJO/Yankel Carolina Sena/fwddatayankelsena/original_yankel.xlsx",sheet='Hoja3'))
datos

# Tipos de varibales                    
aux = as.data.frame(t(t(sapply(datos, class))))
colnames(aux) <- "Tipo de Variable"
aux$Numero_Columna = seq(nrow(aux))
aux

overview(datos)  )

plot(overview(datos)) 


# pasar variables a factor
datos$STATUS <- as.factor(datos$STATUS)
datos$GENDER <- as.factor(datos$GENDER)
datos$COMORBIDITIES <- as.factor(datos$COMORBIDITIES)
datos$CTCNCI <- as.factor(datos$CTCNCI)
datos$ACTION_TAKEN_ <- as.factor(datos$ACTION_TAKEN_)
datos$TNM_STAGE <- as.factor(datos$TNM_STAGE)
datos$DIAGNOSTIC <- as.factor(datos$DIAGNOSTIC)
datos$TREATMENT <- as.factor(datos$TREATMENT)
datos$ECOGPS <- as.factor(datos$ECOGPS)

# vemos los cambios
aux = as.data.frame(t(t(sapply(datos, class))))
colnames(aux) <- "Tipo de Variable"
aux$Numero_Columna = seq(nrow(aux))
aux

# Identificar variable respuesta
respuesta <- c("STATUS")
respuesta

# Identificar variables numericas de entrada
tipos_var <- t(t(sapply(datos, class)))
var_num <- colnames(datos)[tipos_var=="integer"|tipos_var=="numeric"]
var_num <- var_num[var_num!=respuesta]
as.data.frame(var_num)

# Identificar variables cualitativas
tipos_var <- t(t(sapply(datos, class)))
var_cual <- colnames(datos)[tipos_var=="character"|tipos_var=="factor"]
var_cual <- var_cual[var_cual!=respuesta]
as.data.frame(var_cual)

# Tabla de frecuencias:

as.data.frame(table(datos$STATUS))

# Barplot
barplot(table(datos$STATUS))


# Descripcion de variables cuantitativas:
d_uni<-dlookr::describe(datos)
d_uni

for (i in 1:length(var_num)) {
  name_i <- as.name(var_num[i])
  r <- list()
  
  # Histograma de densidad
  r[[1]]<-ggplot(datos, aes(x=!!name_i)) +
    geom_density(fill="#69b3a2", color="black", alpha=0.6)+
    theme_light() +
    #theme(legend.position = "none") +
    xlab("") +
    ylab("Densidad de Frecuencia")
  
  # Boxplot
  r[[2]]<-ggplot(data=datos, aes(y=!!name_i)) +
    geom_boxplot(size = 0.4) +
    theme_light() +
    theme(legend.position = "none")+
    xlab("")
  
  # Qqplot
  r[[3]]<-ggqqplot(datos, x = var_num[i],color = "#FF6666",add.params = list(color = "black"))+
    xlab("") + ylab("Cuartiles reales") +
    theme_minimal() +
    #ggtitle("qqplot") +
    theme(plot.title = element_text(hjust = 0.5))
  
  grid.arrange(r[[1]], r[[2]], r[[3]],
               nrow=1, ncol = 3,
               top = textGrob(var_num[i],gp=gpar(fontsize=16,font=3)))
}

# Analisis de outliers:
diag_out <- diagnose_outlier(datos)
diag_out

# Plot analisis de outliers:
plot_outlier(datos)

# Plot de normalidad:
plot_normality(datos)

var_num

vec_var_num = c('CYCLES_BETWEEN_PET1_PET2',
                'AGE',
                'WBC_PRE',
                'RBC_PRE',
                'HB_PRE',
                'PLT_PRE',
                'CRP_PRE',
                'ALBUMIN_PRE',
                'LDH_PRE',
                'eGFR_PRE',
                'AST_PRE',
                'ALT_PRE',
                'K_PRE',
                'BGL_PRE_PET1',
                'BMI',
                'BW_PRE',
                'SPLEEN_FDG_UPT_PRE_PET1',
                'BM_UPT_PRE_PET1',
                'LIVER_PRE_PET1',
                'ESTIMATED_SPLEE_VOL_PRE_PET1',
                'TIME_BETWEE_PET',
                'dias',
                'SLR_pre',
                'BMLR_pre',
                'OVERALL_TIME')

options(repr.plot.width=16, repr.plot.height=10)

plot_correlate(datos[,vec_var_num],method = "spearman")

# coeficiente de correlación
library(Hmisc)
vec_var_num = c('CYCLES_BETWEEN_PET1_PET2',
                'AGE',
                'WBC_PRE',
                'RBC_PRE',
                'HB_PRE',
                'PLT_PRE',
                'CRP_PRE',
                'ALBUMIN_PRE',
                'LDH_PRE',
                'eGFR_PRE',
                'AST_PRE',
                'ALT_PRE',
                'K_PRE',
                'BGL_PRE_PET1',
                'BMI',
                'BW_PRE',
                'SPLEEN_FDG_UPT_PRE_PET1',
                'BM_UPT_PRE_PET1',
                'LIVER_PRE_PET1',
                'ESTIMATED_SPLEE_VOL_PRE_PET1',
                'TIME_BETWEE_PET',
                'dias',
                'SLR_pre',
                'BMLR_pre',
                'OVERALL_TIME')

# Correlaciones por parejas:

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}


# Calculamos la matriz de correlacion y visualizamos la correlacion de mayor intensidad
tcor<-rcorr(as.matrix(datos[,vec_var_num]),type = "spearman")
corr_data <- flattenCorrMatrix(tcor$r, tcor$P) %>%
    arrange(desc(abs(cor))) #%>%
    #filter(row=='Sales')
corr_data
options(warn = -1)

# Todas la tablas de frecuencias:
all_var <- univar_category(datos) 

all_var

# Diagrama de barras por separado:
plot(all_var)

# Todos los diagramas de barra juntos:
plot_bar_category(datos)

# Tabla de frecuencias + diagrama de barras + diagrama de sectores en una iteración:

vec_var_cual <- c('GENDER','TNM_STAGE','DIAGNOSTIC','TREATMENT','ECOGPS','COMORBIDITIES','CTCNCI','ACTION_TAKEN_')

for (i in vec_var_cual) {
    
    # 0. Seleccionar la variable con el contador i:
    #***************************************************************    
    var_i = i
    
    print("********************************************")
    print(var_i)
    print("********************************************")

    # 1. Creo la tabla de frecuencias relativas y absolutas:
    #***************************************************************

    # Tabla de frecuencias absolutas:
    tabla_frec = data.frame(t(table(datos[,var_i])))[,2:3]
    colnames(tabla_frec)<-c("Grupos","Frec_Absoluta")

    # Tabla de frecuencias relativas:
    tabla_frec$Frec_Relativa <- tabla_frec$Frec_Absoluta/sum(tabla_frec$Frec_Absoluta)
    print(tabla_frec)


    # 3. Crear el diagrama de sectores
    #***************************************************************
    # Create Data
    data <- data.frame(
      group=tabla_frec$Grupos,
      value=tabla_frec$Frec_Relativa
    )

    data <- data %>% 
      arrange(desc(group)) %>%
      mutate(prop = value / sum(data$value) *100) %>%
      mutate(ypos = cumsum(prop)- 0.5*prop )


    p2 <- ggplot(data, aes(x="", y=prop, fill=group)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      theme_void() + 
      theme(legend.position="none") +

      geom_text(aes(y = ypos, label = group), color = "white", size=6) +
      scale_fill_brewer(palette="Set1")

    
    
    # 2. Crear el barplot frecuencia Absoluta
    #***************************************************************
    p1 <- ggplot(data=tabla_frec, aes(x=Grupos, y=Frec_Absoluta)) +
      geom_bar(stat="identity", fill="steelblue")+
      geom_text(aes(label=Frec_Relativa), vjust=1.6, color="black", size=5)+
      theme_minimal()

   
      grid.arrange(p1, p2,
               nrow=2, ncol = 1,
               top = textGrob(var_i,gp=gpar(fontsize=16,font=3)))    
    
    
}

# Diagrama de dispersion por pareja

options(repr.plot.width=8, repr.plot.height=5)
var_respuesta <- as.name(respuesta)

for (i in 1:length(var_num)) {
  name_i <- as.name(var_num[i])
  r <- list()
  
    r[[1]] <- ggplot(datos, aes(x=!!name_i, y=!!var_respuesta)) +
      geom_point() +
      geom_smooth(method=lm ,formula = y ~ x, color="red", fill="#69b3a2", se=TRUE)  +
    theme_minimal()
  
  grid.arrange(r[[1]],
               nrow=1, ncol = 1,
               top = textGrob(var_num[i],gp=gpar(fontsize=16,font=3)))
}



# Mutual informacion variables cualitativas:
inf_gain <- information.gain(STATUS~., data = datos)
inf_gain <- inf_gain %>%
  arrange(desc(attr_importance))
inf_gain

# 6. DESCRIPCION MULTIVARIADA: las variables en función de la respuesta
name_y <- as.name(respuesta[1])

# Descripciones de las variables cuantiativas vs salida
descripcion_num <- datos %>%
  group_by(!!name_y)%>%
  dlookr::describe()
descripcion_num

options(repr.plot.width=16, repr.plot.height=8)


# boxplot + diagrama de error + histograma de densidad + violin plot
for (i in 1:length(var_num)) {
  name_i <- as.name(var_num[i])
  name_y <- as.name(respuesta[1])
  
  r <- list()
  
  # Histograma de densidad
  r[[1]]<-ggplot(datos, aes(x=!!name_i, fill=!!name_y)) +
    geom_density(alpha=0.4)+
    theme_light() +
    #theme(legend.position = "none") +
    xlab("") +
    ylab("Densidad de Frecuencia")
  
  # Boxplot
  r[[2]]<-ggplot(data=datos, aes(y=!!name_i, x=!!name_y)) +
    geom_boxplot(size = 0.4) +
    theme_light() +
    theme(legend.position = "none")+
    xlab("")
  
  # Violin plot con diagramas de error
  r[[3]]<-ggerrorplot(data = datos, x = respuesta, y = var_num[i], 
                      desc_stat = "mean_ci",
                      error.plot = "errorbar",
                      add = c("violin","mean"))+
    theme_light() +
    theme(legend.position = "none") +
    xlab("") +
    ylab("") +
    #ggtitle("Comparacion pvut") +
    stat_compare_means(comparisons = c("0","1"))+ # Add pairwise comparisons p-value
    stat_compare_means(label.y = 1.05*max(datos[,var_num[i]]))                  # Add global p-value
  
  grid.arrange(r[[1]], r[[2]], r[[3]],
               nrow=3, ncol = 1,
               top = textGrob(var_num[i],gp=gpar(fontsize=16,font=3)))
}


# Todos los graficos de barras de cualitativas vs respuesta
name_y <- as.name(respuesta[1])

datos %>%
  group_by(!!name_y) %>%
  plot_bar_category()

# Mosaic plot y chi cuadrado:
r <- list()
for (i in 1:length(var_cual)) {
    # Variable de salida o respuesta
    name_y <- as.name(respuesta[1])

    # Variable de entrada
    name_i <- as.name(var_cual[i])


    # Tabla de contingencias de una variable con la target
    categ <- target_by(datos, !!name_y)
    cat_cat <- relate(categ, !!name_i)
    r[[i]]<-as.data.frame.matrix(cat_cat)
    
    # Chi cuadrado
    print(summary(cat_cat))
    
    # Mosaico de una variable con la target
    grid.arrange(plot(cat_cat),nrow=1, ncol = 1)
    
}
r

# Voy a definir el data frame de entrada: X
X = datos[-1]  
head(X)

# Voy a definir la variable de salida:
y = as.factor(as.vector(unlist(datos[,respuesta])))
head(y)

## Multiple Wilcoxon rank sum tests - casos binarios

Wilk_p <- as.data.frame(t(as.data.frame(lapply(datos[,var_num], function(x) wilcox.test(x ~ as.numeric(datos$STATUS))$p.value)))) %>%
  arrange(V1)
                                               
Wilk_p

## Kruskall wallis rank sum tests - todos los casos
Kruskal_p <- as.data.frame(t(as.data.frame(lapply(datos[,var_num], function(x) kruskal.test(x ~ as.numeric(datos$STATUS))$p.value)))) %>%
  arrange(V1)
                                               
Kruskal_p

## ANOVA - para todos los casos
ANOVA_p<-as.data.frame(t(as.data.frame(lapply(datos[,var_num], function(x) anova(lm(x ~ datos$STATUS))$`Pr(>F)` [1])))) %>%
  arrange(V1)
                                              
ANOVA_p

res_sig <- rep(0,length(var_cual))

for (i in 1:length(var_cual)){
  chisq <- chisq.test(table(as.factor(as.character(datos[,var_cual[i]])),datos[,'STATUS']),correct = TRUE,simulate.p.value = 10000)
  res_sig[i]<-chisq$p.value
}
df_sig <- data.frame(p_valor=res_sig)
row.names(df_sig) <- colnames(datos[,var_cual])

idx_sig <- sort(df_sig$p_valor,decreasing = FALSE,index.return=TRUE)$ix
df_sig_sort <- data.frame(p_valor=res_sig[idx_sig])
row.names(df_sig_sort) <- colnames(datos[,var_cual])[idx_sig]
df_sig_sort

# Mutual informacion variables cuantitativas:
inf_gain <- information.gain(STATUS~., data = datos)
inf_gain <- inf_gain %>%
  arrange(desc(attr_importance))
inf_gain

# como elegia muchas variables fallaba(necesita menos variables),por eso ponemos a mano las que queremos(se puede jugar intercambiando variables)
# calcular los 5 modelos segun el BIC
modelos_5top <-
  glmulti(STATUS~ACTION_TAKEN_+TNM_STAGE+DIAGNOSTIC+LIVER_PRE_PET1+dias+OVERALL_TIME+BMI+CYCLES_BETWEEN_PET1_PET2+SPLEEN_FDG_UPT_PRE_PET1+BM_UPT_PRE_PET1+ESTIMATED_SPLEE_VOL_PRE_PET1+SLR_pre+BMLR_pre, data = datos,
          level = 1,               # No interaction considered
          method = "h",            # Exhaustive approach
          crit = "bic",            # BIC as criteria
          confsetsize = 5,         # Keep 5 best models
          plotty = F, report = F,  # No plot or interim reports
          fitfunction = "glm",     # glm function
          family = binomial)       # binomial family for logistic regression

# Los 5 mejores modelos
print(modelos_5top@formulas)
options(warn = -1)

# El summary de los 5 mejores modelos
summary(modelos_5top@objects[[5]])

# Las variables que aparecen en el mejor modelo son:
var_sel_mod<-names(modelos_5top@objects[[5]]$coefficients)[-1]
print(var_sel_mod)

var_sel_mod1<-c("TNM_STAGE","LIVER_PRE_PET1","BMI")

# Calculamos el mejor modelo y su accuracy:

# Construimos la formula de forma automatica:
x = paste(var_sel_mod1, collapse = " + ")
y = respuesta
formBIC = as.formula(paste(y, "~", x))
formBIC

    modelo1 <- train(formBIC, data=datos,method='glm')

    print("Matriz de confusión")
    pred <- predict(newdata=datos,modelo1) # prediccion usando el modelo logistico
    real <- as.factor(datos[,respuesta]) # respuesta real
    cm_train_TOT <- caret::confusionMatrix(data=pred,reference=real) # guardamos la matriz de confusion de training
    print(cm_train_TOT) # pintamos la matriz de confusion de training


# Voy a definir el data frame de entrada: X
X = datos[-1]
head(X)

# Voy a definir la variable de salida:
y = as.factor(as.vector(unlist(datos[,respuesta])))
head(y)

# Determinar la importancia de los atributos.
boruta.model <- Boruta(y~., data = cbind(X,y), doTrace = 2)

options(repr.plot.width=12, repr.plot.height=8)

print(boruta.model)
plot(boruta.model)

# Refinar modelo para resolver posibles atributos tentativos.
# Dado que pueden existir atributos no resueltos (tentativos), refinamos el modelo.

boruta.model2 <- TentativeRoughFix(boruta.model)
print(boruta.model2)
plot(boruta.model2)

# Obtener una lista de los atributos y sus etiquetas con el analisis de Boruta
as.data.frame(boruta.model$finalDecision)

# Obtener una lista de los atributos importantes y tentativos
opc_boruta = getSelectedAttributes(boruta.model2, withTentative = F)
opc_boruta

# Construimos la formula de forma automatica:
x = paste(opc_boruta, collapse = " + ")
y = respuesta
formBoruta = as.formula(paste(y, "~", x))
formBoruta

library(ROCR)

index = sample(1:nrow(datos), size = .100 * nrow(datos))
#index
train = datos[index, ]
test = datos[-index, ]

model = glm(STATUS~TNM_STAGE+ACTION_TAKEN_+DIAGNOSTIC+LIVER_PRE_PET1+dias+BMI+CYCLES_BETWEEN_PET1_PET2,data=train, 
            family = binomial(link = "logit"))

pred = predict(model,test,type="response")
pred = prediction(pred,test$STATUS)
perf = performance(pred, "acc")
#plot(perf)

max_ind = which.max(slot(perf, "y.values")[[1]] )
acc = slot(perf, "y.values")[[1]][max_ind]
cutoff = slot(perf, "x.values")[[1]][max_ind]
print(c(accuracy= acc))

perf_cost = performance(pred, "cost")
perf_err = performance(pred, "err")
perf_tpr = performance(pred, "tpr")
perf_sn_sp = performance(pred, "sens", "spec")

roc = performance(pred,"tpr","fpr")
plot(roc, colorize = T, lwd = 2)
abline(a = 0, b = 1)

auc = performance(pred, measure = "auc")
print(auc@y.values) 

