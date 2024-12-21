#    Hola, este es mi primer código que comparto. Ojalá pueda ser de ayuda.
#                               By @eusounadie


#------------------------------------------------------------------------------#
#   --------------- Analizando mi SPOTIFY WRAPPED 2024 en R --------------     #
#------------------------------------------------------------------------------#

#-----------------------         PASO 1    ------------------------------#
#                 DESCARGAR TUS DATOS DE SPOTIFY

# 1.- Abrir app Spotify
# 2.- Dar clic en <Account>. Se abre una ventana en el navegador
# 3.- Deslizarse hasta el apartado de <Seguridad y privacidad> -> Clic en 
# 3.- <Privacidad de la cuenta>. 
# 4.- Deslizarse hasta el botón de <Solicitar datos>
# El periodo de espera es de 30 días, pero varía.
# 5.- Una vez que recibas el correo de <Ya puedes descargar los datos de tu
# 5.- cuenta>, descargas la carpeta comprimida.
# 6.- Descomprimir la carpeta y listo, ya tienes tus datos en formato JSON.


# ----------------------        PASO 2     --------------------------------
#                         LECTURA DE DATOS

#install.packages("jsonlite")#Instalar
library(jsonlite)#Permite leer archivos en formato JSON
library(dplyr)#Instalar y luego abrir. Sirve para manipular y operar df

getwd()#Permite ver nuestro directorio de trabajo
setwd("C:/Users/biobl/Desktop/RStudio/DATAVIZ")#Aquí puedes establecer el 
#directorio manualmente y buscar dónde está la carpeta que descargamos 

SONG <- read_json("StreamingHistory_music_0.json", simplifyVector = TRUE)
#Creamos un objeto SONG donde se va a cargar el archivo StreamingHistory_music_0 
#que viene en nuestra carpeta de datos. 

#Una vez cargado, podemos ver una tabla con 4 columnas: endtime (fecha), 
#artistName (nombre de artista)trackname (nombre de la canción) y msPlayed 
#(milisegundos escuchados)

dim(SONG)#Permite ver la dimensión de datos del archivo
#4 variables y 2537 observaciones, desde diciembre 23- diciembre 24

head(SONG)#encabezado de las primeras observaciones

#-------------------           PASO 2    -----------------------------------#
#                        LIMPIEZA DE DATOS

#Vamos a convertir los milisegundos a minutos
SONG_min <- SONG |> #Creamos un objeto nuevo a partir de la data original
  mutate(msPlayed = (msPlayed/1000)/60)#modificó la columna msPlayed. Ahora la 
#los valores están en minutos
View(SONG_min)#Corroboramos...

#Vamos a cambiar el nombre de la última columna para evitar confusiones. 
SONG_min <- SONG_min |> 
  rename("minPlayed"=msPlayed)#renombramos msPlayed -> minPlayed

#Ahora vamos a promediar los minutos x canción
canciones <- SONG_min |> #de la data...
  group_by(artistName, trackName) |> #agrupar por artista y canción
    summarise(
      mean_song= mean(minPlayed, na.rm = TRUE), #promedio x canción
      totalMinute= sum(minPlayed, na.rm = TRUE),#minutos totales x canción
      playCount = n()#N° de reproducciones x cancion
      ) |> 
  ungroup() |> #desagrupar el orden anterior
  arrange(desc(playCount)) #arreglar en orden descendente en base al N° de 
#reproducidos x canción

#El primer lugar lo ocupa un Unknown, lo cual según significa que fue dado de 
#baja del catálogo de Spotify, así que lo vamos a eliminar:

canciones <- canciones |> slice(-1)#slice eliminó la primera fila y se guardó

TOP_TEN <- canciones |> 
  slice(1:10)
##Ahora, hicimos lo mismo que arriba pero en un rango de 1:10 para obtener un 
#TOP 10, para graficar

#Pero antes, vamos a cambiar el nombre de las demás columnas porque estas van
#a ser usadas tal cual en los ejes de la gráfica

TOP_TEN <- TOP_TEN |> 
  rename("Artista"="artistName",#Nombre nuevo = nombre antiguo
         "Canción"="trackName",
         "Número de reproducciones"="playCount")

TOP_TEN #Corroboramos el cambio...

#AHORA SI, listo para graficar

# -----------------------     PASO 3     ----------------------------------- #
#                           GRAFICAR
library(ggplot2)

ggplot(data= TOP_TEN, mapping= aes(x= `Número de reproducciones` , y= Canción))+
  #titulos de más de una palabra debe ir entre comillas o marcará error
  geom_bar(stat = "identity")#Le tuvimos que decir que yo le daba los ejes

top10_plot <- ggplot(
  data= TOP_TEN, mapping= aes(x= `Número de reproducciones` , y= Canción))+
  geom_col()#usando esto en vez de geom_bar ya no es necesario especificar el identity

top10_plot #Revisamos y vemos que el grafo no está ordenado, asi que:

library(forcats)#nos oermitirá usar la función fct_reorder para poder acomodar

TOP_TEN |> 
  mutate(Canción = fct_reorder(Canción, desc(`Número de reproducciones`))) |> #reordena en función de otra columna 
  ggplot( aes(x=Canción, y=`Número de reproducciones`)) + #especifica los ejes
  geom_col()+
  coord_flip()#ajusta a horizontal el gráfico
#Tenemos un grafo donde la barra más larga está abajo y en la cima la más corta.
#Podemos invertir esto eliminando "desc"

TOP_TEN |> 
  mutate(
    Canción = fct_reorder(Canción, `Número de reproducciones`)) |> #reordena en función de otra columna 
  ggplot(
    aes(x=Canción, y=`Número de reproducciones`)) + #especifica los ejes
  geom_col()+
  coord_flip()
#No obstante, todo es gris y las barras pueden ser más delgadas, asi que:

TOP_TEN |> 
  mutate(
    Cancion = fct_reorder(Canción, `Número de reproducciones`)) |> #reordena en función de otra columna 
  ggplot(
    aes(x=Canción, y=`Número de reproducciones`, fill = Canción)) + #especifica los ejes y colorea el relleno de la barra por canción
  geom_col(
    width = 0.8)+ #ajustar anchura
  coord_flip()+#ajusta horizontal
  theme(legend.position = "none")#elimina la leyenda del color
  
#Ahora para aplicar una paleta de colores más destacable tenemos dos opciones:

# Opción 1
library(paletteer)#solo permite un set de 9 colores, por tanto lo descartamos

# Opción 2
library(colorspace)
hcl_palettes(plot = TRUE)#nos presenta las paletas que tiene y nosotros 
#seleccionamos en función de los datos que queramos presentar: cualitativo, 
#secuencial, y divergente


TOP_TEN |> 
  mutate(
    Canción = fct_reorder(Canción, `Número de reproducciones`)) |> #reordena en función de otra columna 
  ggplot(
    aes(x=Canción, y=`Número de reproducciones`, fill= Canción )) + 
  #especifica los ejes y colorea por canción (fill=relleno)
  geom_col(
    width = 0.8)+ #ajustar anchura barra
  scale_fill_discrete_sequential("ag_GrnYl")+#especificamos la paleta del paquete colorspace
  coord_flip()+#ajusta horizontal
  theme(legend.position = "none") #elimina la leyenda
#VOILÁ, qué diferencia. Hay una gráfica con una paleta de amarillo-verde

#Lo guardamos en un obj para que sea más fácil ir agregando lo demás
TOP_p <- TOP_TEN |> 
  mutate(
    Canción = fct_reorder(Canción, `Número de reproducciones`)) |> #reordena en función de otra columna 
  ggplot(
    aes(x=Canción, y=`Número de reproducciones`, fill= Canción )) + #especifica los ejes y colorea por canción (fill=relleno, color=borde)
  geom_col(
    width = 0.8)+ #ajustar anchura
  scale_fill_discrete_sequential("ag_GrnYl")+
  coord_flip()+#ajusta horizontal
  theme(legend.position = "none")

#Enseguida podemos editar titulos y fonts

library(showtext)#instalamos y abrimos 
font_add_google("Roboto")#buscamos una fuente de interés en Google y pegamos el nombre entre comillas
showtext_auto()#necesario para lo siguiente

TOP_p + labs(
  title = "Canciones más escuchadas",
  subtitle = "Por el número de reproducciones - Wrapped 2024",
  caption = "Data: Spotify |By @eusou_nadie"
)+
  theme(plot.title = element_text(family = "Roboto", size = 20, face = "bold"),
        #ajustes del titulo
        plot.subtitle = element_text(family = "Roboto"),
        #ajustes del subtitulo
        plot.caption = element_text(family = "Roboto", size = 10, face = "bold"),
        #ajustes de la leyenda
        axis.title.x = element_text(family = "Roboto", size = 15),#edita titulo eje x
        axis.title.y = element_text(family = "Roboto", size = 15),#edite titulo eje y
        axis.text = element_text(family = "Roboto", size = 12.5))#edita ejes

#LISTOOO, TENEMOS UN GRÁFICO DECENTE.

#Lo siguiente es únicamente para jugar con los temas de base. Pero antes, debemos de 
#guardar la data en un nuevo obj sobre el que vamos a estar editando los temas
#para que no afecte al original:

theme_1 <- TOP_p + labs(
  title = "Top 10 canciones más escuchadas",
  subtitle = "Por el número de reproducciones - Wrapped 2024",
  caption = "Data: Spotify |By @eusou_nadie"
)+
  theme(plot.title = element_text(family = "Roboto", size = 20, face = "bold"),
        plot.subtitle = element_text(family = "Roboto"),
        plot.caption = element_text(family = "Roboto", size = 10, face = "bold"),
        axis.title.x = element_text(family = "Roboto", size = 15),#edita titulo eje x
        axis.title.y = element_text(family = "Roboto", size = 15),#edite titulo eje y
        axis.text = element_text(family = "Roboto", size = 12.5))#edita ejes

#Realizado lo anterior, procedemos...
#Por ejemplo, The Economist tiene un tema que podemos emplear usando la libreria
#ggthemes

library(ggthemes)
plot_economist1 <- theme_1 + theme_economist()+#este fue el tema general de The Economist
  #pero lo subsiguiente fueron ajustes a gusto personal:
  theme(legend.position = "none",#eliminó la leyenda
        axis.title.y = element_text(margin= margin (r=15)),#separó el titulo eje y (canciones) del eje y 
        plot.title = element_text(vjust= 2.5, hjust = 0))#modifica posición titulo

# -------------------------   PASO 4   ---------------------------------- #
#                        GUARDAR GRÁFICO 

#Guardamos
ggsave(filename = "songs24_6.png", #Nombramos el archivo que vamos a guardar con la terminación del formato            
       plot = plot_economist1,#especificamos qué gráfica es                   
       device = "png",#formato del archivo                          
       width = 10, height = 10, units = "in", #medidas 
       dpi = 300) #resolución de impresión

#Le damos correr, abrimos la imagen y FUCK!Hay un problema: los títulos son
#ilegibles. Solo se me ocurre jugar con los valores de los mismo hasta hallar la
#proporción más adecuada...Para ellos guardamos los cambios en un nuevo objeto

plot_economist2 <- theme_1 + theme_economist()+
  theme(
    legend.position = "none",#eliminó la leyenda
    axis.title.y = element_text(
      margin= margin (r=15),#separa el título del eje Y
      size = 55, face = "bold" # Ajusta tamaño y estilo del título del eje Y
    ),
    axis.title.x = element_text(
      size = 55, face = "bold" # Tamaño y estilo del título del eje X
    ),
    axis.text = element_text(
      size = 45 # Tamaño de las etiquetas de los ejes
    ),
    plot.title = element_text(
      size = 75, face = "bold", hjust = 0, vjust = 2.5 # Título más grande y legible
    ),
    plot.subtitle = element_text(
      size = 60, face = "italic", hjust = 0 # Subtítulo más visible y alineado a la izquierda
    ),
    plot.caption = element_text(
      size = 35, face = "italic", hjust = 1, vjust = -1 # Ajusta el pie del gráfico
    ),
    panel.grid.major = element_line(size = 1), # Grosor de líneas de cuadrícula principales
    panel.grid.minor = element_blank() # Elimina cuadrícula menor
  )

#AJUSTAMOS VALORES, GUARDAMOS, CHECAMOS LA IMAGEN HASTA QUE OBTENGAMOS UN 
#RESULTADO DECENTE. En mi caso lo hice 9 veces

#FINALMENTE GUARDAMOS Y LISTO

ggsave(filename = "songs24_9.png",             
       plot = plot_economist2,                   
       device = "png",                          
       width = 10, height = 10, units = "in",   
       dpi = 300) 




