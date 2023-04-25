options(scipen = 900) #eliminamos la notacion cientifica para obtener una mejor visualisacion

#cargamos las librerias necesarias para la ejecucion del codigo
library(ggplot2)
library(reshape2)
library(ggcorrplot)
library(hrbrthemes)
library(dplyr)
library(tidyverse)
library(dplyr)

#cargamos el dataframe y lo trabajamos para poder utilizarlo
data_frame = read.csv("Fuel_Consumption_Ratings.csv")

data = data_frame[0:15] # trabajamos con este data frame
data <- data[-1,] #eliminamos primer fila del dataframe al ser redundante
data <- data[-(960:6685),] 

colnames(data)[10] = "Hwy"
colnames(data)[11] = "Combined"
colnames(data)[12] = "Combined_mpg"

#corregimos error de importacion de datos, se leian como strings(chr) cuando eran numeros (num).
data$Engine.Size = as.numeric(as.character(data$Engine.Size))
data$Hwy = as.numeric(as.character(data$Hwy))
data$Combined = as.numeric(as.character(data$Combined))
data$Combined_mpg = as.numeric(as.character(data$Combined_mpg))
data$CO2.Emissions = as.numeric(as.character(data$CO2.Emissions))
data$CO2 = as.numeric(as.character(data$CO2))
data$Smog = as.numeric(as.character(data$Smog))
data$Fuel.Consumption = as.numeric(as.character(data$Fuel.Consumption))

#verificamos nuevamente nuestro dataset para ver podemos comenzar a trabajarlo.
str(data)
#creamos un dataset eliminando las variables categoricas para poder hacer un correlation plot
sub_data = data[c('Engine.Size','CO2.Emissions','CO2','Smog','Fuel.Consumption')]

#porque el corrplot no se grafica automaticamente?, es necesario corrrer el codigo 2 veces
cor(sub_data)
ggcorrplot(cor(sub_data))

#al haber una gran multicolinealidad entre nuestras variables hacemos una regresion con un unico regresor.
#una de las soluciones a la multicolinealidad es eliminar regresores o aumentar el dataset, al ser fijo el dataset..
regresion = lm(CO2.Emissions~Engine.Size, data)
summary(regresion)


plot(data$CO2.Emissions,data$Engine.Size,
     main='co2 emissions Vs Engine Size',
     xlab='YearsExperience', ylab='Salary')

# Basic scatter plot
ggplot(data, aes(x=Engine.Size, y=CO2.Emissions)) + geom_point() + geom_smooth(method=lm)
  geom_point(size=2, shape=23)

#acces unique car makes
unique_car_makers = unique(data$Make)
for (i in unique_car_makers) {
  print(i)
}

#calculamos las emisiones para cada marca en particular. queremos identificar cuales estan por encima o debajo del promedio
avg_co2_by <- aggregate(CO2.Emissions ~ Make + Model.1, data = data, FUN = mean)
avg_co2_by_maker <- aggregate(CO2.Emissions ~ Make, data = avg_co2_by, FUN = mean)


ggplot(data = avg_co2_by_maker) +
  geom_col(mapping = aes(x = Make, y = CO2.Emissions))

#Graficamos unicamente las 5 marcas que tienen mayores emisiones de carbono y las que menor tienen
sorted_avarage_CO2_high = avg_co2_by_maker[order(-avg_co2_by_maker$CO2.Emissions),] #ordenamos de mayor a menor nuestro data set
Highest_emission_makers = head(sorted_avarage_CO2_high,5)
lowest_emission_makers = tail(sorted_avarage_CO2_high, 5)

low_high_emissions = rbind(Highest_emission_makers,lowest_emission_makers)


#graficamos el grafico de barras
ggplot(data = low_high_emissions) +
  geom_col(mapping = aes(x = Make, y = CO2.Emissions))

#agregamos linea promedio:
mean_CO2<- mean(avg_co2_by_maker$CO2.Emissions)

#transformamos en variables dummies a las columnas categoricas para poder analizarlas
df_2 ={
data %>% mutate(dummy=1) %>%
spread(key=Fuel,value=dummy, fill=0) %>% slice(1:799)}

#renombramos algunas columnas para que sean mas legibles
colnames(df_2)[15] = "Diesel"
colnames(df_2)[16] = "E85"
colnames(df_2)[17] = 'Regular'
colnames(df_2)[18] = "Premium"


#corremos una nueva regresion incluyendo nuestras variables categoricas. Se omite la categoria al ser el caso base
#El tipo de gasolina, es estadisticamente significativo para explicar las emisiones de carbono?
Linear_model_2 = lm(CO2.Emissions ~ Diesel + Premium + Regular + E85, df_2)
summary(Linear_model_2)

#Regresion explicando el consumo de combustible para los distintos tipos de gasolinas
#El tipo de gasolina, tiene efecto en el consumo de gasolonina?
#Puede haber multicolinealidad. La gasolina afecta el consumo de gasolina y el consumo de gasolina afecta las emisones
Linear_model_3 =lm(Fuel.Consumption ~ Diesel + Premium + Regular + E85, df_2)
summary(Linear_model_3)

# encontrar el valor máximo por cantidad de cilindros
maximo_por_cilindro = {
df_2%>%
  group_by (Cylinders)%>%
  filter (CO2.Emissions == max (CO2.Emissions, na.rm = TRUE ))}

# encontrar el valor máximo por cantidad de cilindros
minimo_por_cilindro = {
  df_2%>%
    group_by (Cylinders)%>%
    filter (CO2.Emissions == min (CO2.Emissions, na.rm = TRUE ))}

# Group by mean using dplyr
Mean_by_cylinder <- df_2 %>% group_by(Cylinders) %>% 
  summarise(mean_CO2=mean(CO2.Emissions),
            .groups = 'drop')

Mean_by_fuel_type <- data %>% group_by(Fuel) %>% 
  summarise(mean_CO2=mean(CO2),
            .groups = 'drop')

#creamos como variable categorica el numero de cylindros para una mayor visualizacion
Mean_by_cylinder$Cylinders = factor(Mean_by_cylinder$Cylinders, levels = unique(Mean_by_cylinder$Cylinders))

ggplot(Mean_by_cylinder, aes(x = Cylinders, y = mean_CO2, fill = factor(Cylinders))) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = c("#53B400", "#00C094", "#00B6EB", "#A58AFF", "#C49A00", "#FC5E00","#A30000")
) +
  labs(title = "Mean CO2 Emissions by Cylinders", x = "Cylinders", y = "Mean CO2 Emissions") +
  theme_minimal()



ggplot(Mean_by_cylinder, aes(x = Cylinders, y = mean_CO2, fill = factor(Cylinders))) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = c("#53B400", "#00C094", "#00B6EB", "#A58AFF", "#C49A00", "#FC5E00","#A30000")) +
  labs(title = "Mean CO2 Emissions by Cylinders", x = "Cylinders", y = "Mean CO2 Emissions", caption = "Figure_3") +
  theme_minimal() +
  guides(fill = FALSE)


# Group by mean using dplyr
Mean_by_vehicle_type <- df_2 %>% group_by(Vehicle.Class) %>% 
  summarise(mean_CO2=mean(CO2.Emissions),
            .groups = 'drop')



#eliminamos ciertos datos para poder representar mejor el resto. Evitar exceso de informacion.
Mean_by_vehicle_type <- Mean_by_vehicle_type[!(Mean_by_vehicle_type$Vehicle.Class %in% c("Special purpose vehicle", "Minivan","Station wagon: Mid-size","SUV: Small","Pickup truck: Standard","Minicompact","Pickup truck: Small")), ]


#creamos como variable categorica
Mean_by_vehicle_type = Mean_by_vehicle_type[order(Mean_by_vehicle_type$mean_CO2),]
Mean_by_vehicle_type$Vehicle.Class = factor(Mean_by_vehicle_type$Vehicle.Class, levels = unique(Mean_by_vehicle_type$Vehicle.Class))

ggplot(Mean_by_vehicle_type, aes(x = Vehicle.Class, y = mean_CO2, fill = factor(Vehicle.Class))) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = c("#53B400", "#00C094", "#00B6EB", "#A58AFF", "#C49A00", "#FC5E00","#A30000")
  ) +
  labs(title = "Mean CO2 by vehicle Class", x = "Vehicle class", y = "Mean CO2 Emissions") +
  theme_minimal() +
  guides(fill = FALSE)

hist(data$CO2.Emissions, main = "Normal Distribution", xlab = "Values")

