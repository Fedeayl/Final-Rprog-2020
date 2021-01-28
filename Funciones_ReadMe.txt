Funciones propias - Descripción de uso

Vector_Departamentos() -> No admite argumentos. El resultado por defecto es un vector cuyas entradas correspoden 
                          a los departamentos uruguayos en el orden que los presenta la Corte Electoral en sus datos. 
                          La utilidad de esta función radica en poder utilizarla en funciones siguientes, evitando la 
                          escritura reiterada de un vector con 19 entradas. 
                         

Vector_Partidos() ->      No admite argumentos. El resultado por defecto es un vector cuyas entradas correspoden 
                          a los partidos políticos uruguayos en el orden que los presenta la Corte Electoral en sus datos. 
                          
                          
FUN_Votos -> Esta función devuelve los resultados generales por partido político según diferentes niveles de agregación. 
             Admite los siguientes argumentos:
        data: base de datos con (al menos) las columnas: DEPTO, CIRCUITO, SERIES, LEMA, HOJA y CNT_VOTOS.
        Departamento: por defecto muestra los resultados de todos los departamentos. Se puede agregar como argumento 
                el nombre de uno o más como vector.
        Partido: por defecto muestra los resultados de todos los partidos.  Se puede agregar como argumento el nombre 
                de uno o más como vector.
        Circuito: clase lógico. Es TRUE si se quiere el nivel de desagregación por circuito. Está predefinido como FALSE.
        Hoja: clase lógico. Es TRUE si se quiere el nivel de desagregación por hoja de votación. Está predefinido como FALSE.
        As.Arg: clase lógico. Si se introduce As.Arg=TRUE devuelve la salida tipo lista. Es útil para utilizar esta función
                como insumo de otras funciones que requieran operar con este tipo de objeto.
        
        
Winner -> Regresa un data frame  con tres columnas que corresponden al departamento, el partido ganador en ese departamento 
          y el margen de victoria que obtuvo dicho partido.Winner()  combina dos dataframes que son resultados de 
          la función win(), que determina partido ganador por departamento y winfor() que recoge el margen de victoria en cada 
          uno de ellos, ambas definidas en el interior de Winner(). 
          El único argumento admitido en Winner() es "data" como la base de datos a utilizar, que debe cumplir 
          los requisitos de la función FUN_Votos, pues tanto win() como winfor() la utilizan como insumo. 


Hoja_win() -> Devuelve un data frame con la hoja más votada por partido por departamento y los votos obtenidos por esta.
              Admite como argumentos: data: base de datos; Departamento: un vector con uno o más departamentos; 
              Partido: un vector con uno o más partidos. Por defecto considera todos los departamentos y los tres partidos
              políticos principales (Frente Amplio, Partido Nacional y Partido Colorado)


Circ_nth() -> Regresa un data frame con los circuitos en los que el partido resultó en enésimo lugar. Los argumentos admitidos son:
        data: base de datos con las condiciones que determina FUN_Votos
        Departamento: vector con los departamentos elegidos. Por defecto muestra el resultado para los 19.
        Partido: vector con el partido a clasificar. Por defecto muestra los cuatro principales.
        nth: lugar del ordenamiento deseado. Por ejemplo nth=1 muestra el primer partido en la ordenación por circuitos por orden de 
             votos decreciente, nth=2 el segundo, etc.

Estadisticos -> Muestra, con una sola función, varios estadísticos de resumen de una distribución. El argumento de entrada debe ser 
                un vector de tipo numérico. Los estadísticos presentados son: mínimo, media, mediana, máximo, desvío estandar y el
                número de observaciones. En todos los casos en los que se requiere, se remueven los valores NA (na.rm=TRUE).
                Los resultados se muestran redondeados con dos cifras decimales. 
