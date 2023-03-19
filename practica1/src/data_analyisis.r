library(treemap)

############## CONFIGURATION ##############
col_name <- "Resolution_H" # CHANGE
col_values <- gpus$Resolution_H #CHANGE
col_units <- "Pixels"
pre_imput <- TRUE # CHANGE
pre_imput_value <- ifelse(pre_imput, "before imputation","after imputation")
############## GRAPHS ##############
#------------ SUMMARY -------------------
summary(col_values)

#------------ BOXPLOT -------------------
boxplot(col_values, # Datos
        horizontal = TRUE, # Horizontal o vertical
        lwd = 2, # Lines width
        xlab = "Values",  # Etiqueta eje X
        ylab = col_name,  # Etiqueta eje Y
        main = paste("Boxplot", col_name, pre_imput_value, sep=" "), # Título
        notch = FALSE, # Añade intervalos de confianza para la mediana
        border = "black",  # Color del borde del boxplot
        outpch = 25,       # Símbolo para los outliers
        outbg = "red",   # Color de los datos atípicos
        whisklty = 2,      # Tipo de línea para los bigotes
        lty = 1) # Tipo de línea (caja y mediana)

stripchart(col_values, method = "jitter", pch = 1, add = TRUE, col = "blue")

#------------ HISTOGRAM -------------------
h<-hist(col_values,
        main= paste("Histogram", col_name, pre_imput_value, sep=" "),
        xlab= col_units,
        xlim=c(min(na.omit(col_values)),max(na.omit(col_values))),
        col="grey",
        freq=TRUE, 
)
text(h$mids, h$counts + 1, ifelse(h$counts == 0, "", h$counts))

#------------ BARPLOT -------------------
barplot(table(col_values),main=paste("Barplot", col_name,pre_imput_value, sep=" "),
        xlab = paste("",col_units, sep=" ") , ylab ="# of instances",las=1)

#------------ TREEMAP -------------------
# Crear una tabla de frecuencia
freq_table <- table(col_values)

# Convertir la tabla de frecuencia en un data frame
df_freq <- as.data.frame(freq_table)
# Añadimos los porcentajes por cada valor
df_freq$perc <- prop.table(freq_table)*100

# Crear el treemap
treemap(df_freq, index = "col_values", vSize="Freq", title = paste("Treemap", col_name, pre_imput_value, sep=" "))


