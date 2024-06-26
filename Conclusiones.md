# Conclusiones

En primer lugar, el **análisis de la normalidad** de las variables alimentos y nutrientes nos indica que no siguen una distribución Normal (algo esperable por los datos en sí).

Al realizar la **reducción de dimensiones por PCA**, extraemos los score que nos explicarán el grado de asociación de cada paciente a cada uno de componente (PCs).
Fijándonos en la salida, cuanto mayor es el score, mayor contribución al PCs concreto tendrá el paciente.
En el caso de números negativos, su relación es inversa no aportando nada al PCs concreto (hemos reducido la salida a 5 con un head para no mostrar todos los componentes).

Los **eigvalues**, nos permite identificar las dimensiones de la varianza total de los datos en porcentaje. Observamos que la varianza acumulada no explica ni el 60-70% (con los que nos acercaríamos a valores óptimos) hasta aproximadamente la dimensión 45. Decidimos, por tanto, usar solo las dos primeras dimensiones apoyándonos en la gráfica del codo donde la caída de la varianza explicada es más brusca.

También se **analiza gráficamente** cuánto aporta cada una de las variables, tanto a PC1 Como a PC2, pero en este caso delimitado exclusivamente, a las diez que más aportan en ambos casos.
En cuanto a los gráficos descriptivos, en la primera representación, visualizamos la asociación de cada individuo a cada dimensión seleccionada (aunque en este caso y por el exceso de datos pues no es muy clarificador).

La siguiente imagen, de igual manera, nos indica las variables que más aportan a cada dimensión y que en consonancia con las gráficas de barras previas, corresponderán de forma generalizada a las variables de tipo nutrientes.

Para los **análisis descriptivos** con respecto a los terciles de PC1 y PC2, hemos decidido tener en cuenta las variables altura, peso, IMC, edad y METs h semana; de los que sacaremos la media y desviación típica que acompañaremos con su p-valor correspondiente, basado en el test  Kruskal-Wallis (para muestras no paramétricas que no siguen distribución normal). En la salida, observamos que para todas las variables analizadas hay diferencias significativas, estando fuertemente relacionadas con PC1 y PC2, excepto para el caso del IMC con respecto a PC1, en la que cualquier diferencia que se diera sería al azar.

En el caso de la **regresión logística** se busca conocer, cómo afectan las variables elegidas en la prevalencia de tener diabetes.

Según la tabla podemos ver que: 
No hay diferencias estadísticamente significativas en las odds para el segundo y tercer tercil, en comparación con el primer tercil (referencia) en el componente 1, ya que los valores p valores son 0,4853 y 0,1634 respectivamente. En cambio, si se observan diferencias estadísticamente significativas en el tercil 3 del componente 2. 

La variable edad con un valor de Odds Ratio (OR) de 2,1694 y un valor p < 0,01, muestra que el aumento en la edad está fuertemente asociado con un mayor riesgo de diabetes. Por otro lado, la variable de peso e IMC también están asociadas con un mayor riesgo de padecer diabetes, ya que la variable peso tiene una OR de 1,5228 y un valor de p < 0,01 y la variable IMC tiene una OR de 1,3206 con un valor p de 0.0256. No se observan diferencias estadísticamente significativas en el METs horas a la semana.
