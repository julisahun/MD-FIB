library(treemap)

############## CONFIGURATION ##############
col_name <- "HDMI_Connection" # CHANGE
col_values <- gpus$HDMI_Connection #CHANGE
col_units <- "ports"
pre_imput <- TRUE # CHANGE
pre_imput_value <- ifelse(pre_imput, "before imputation","after imputation")
############## GRAPHS ##############
#------------ BOXPLOT -------------------
boxplot(table(col_values), # Datos
        horizontal = FALSE, # Horizontal o vertical
        lwd = 2, # Lines width
        xlab = col_name,  # Etiqueta eje X
        ylab = "Instances",  # Etiqueta eje Y
        main = paste("Boxplot", col_name, pre_imput_value, sep=" "), # Título
        notch = FALSE, # Añade intervalos de confianza para la mediana
        border = "black",  # Color del borde del boxplot
        outpch = 25,       # Símbolo para los outliers
        outbg = "red",   # Color de los datos atípicos
        whisklty = 2,      # Tipo de línea para los bigotes
        lty = 1) # Tipo de línea (caja y mediana)

stripchart(col_values, method = "jitter", pch = 19, add = TRUE, col = "blue")

#------------ HISTOGRAM -------------------
h<-hist(col_values,
        main= paste("Histogram", col_name, pre_imput_value, sep=" "),
        xlab=paste("HDMI Connection", col_units, sep=" "),
        xlim=c(0,3),
        col="grey",
        freq=TRUE, 
)
text(h$mids, h$counts + 1, ifelse(h$counts == 0, "", h$counts))

#------------ BARPLOT -------------------
barplot(table(col_values),main=paste("Barplot", col_name,pre_imput_value, sep=" "),
        xlab = paste("# of",col_units, sep=" ") , ylab ="# of instances",las=1)

#------------ TREEMAP -------------------
# Crear una tabla de frecuencia
freq_table <- table(col_values)

# Convertir la tabla de frecuencia en un data frame
df_freq <- as.data.frame(freq_table)
# Añadimos los porcentajes por cada valor
df_freq$perc <- prop.table(freq_table)*100

# Crear el treemap
treemap(df_freq, index = "col_values", vSize="Freq", title = paste("Treemap", col_name, pre_imput_value, sep=" "))

#------------ SUMMARY -------------------
summary(col_values)
