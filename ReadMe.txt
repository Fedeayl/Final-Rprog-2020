Funciones propias - Descripción de uso

Vector_Departamentos() -> No admite argumentos. El resultado por defecto es un vector cuyas entradas correspoden a los departamentos uruguayos en el orden que los presenta la Corte Electoral en sus datos. La utilidad de esta función radica en poder utilizarla en funciones siguientes, evitando la escritura reiterada de un vector con 19 entradas. 


FUN_Votos -> Esta función devuelve los resultados generales por partido político según diferentes niveles de agregación. Admite los siguientes argumentos:
        data: base de datos con (al menos) las columnas: DEPTO, CIRCUITO, SERIES, LEMA, HOJA y CNT_VOTOS.
        Departamento: por defecto muestra los resultados de todos los departamentos. Se puede agregar como argumento el nombre de uno de ellos (por ahora uno solo, defectos de programación, debería admitir un vector con más de un departamento)
        Partido: por defecto muestra los resultados de todos los partidos.  Se puede agregar como argumento el nombre de uno de ellos (por ahora uno solo, defectos de programación, debería admitir un vector con más de un partido)
        Circuito: clase lógica. Es TRUE si se quiere el nivel de desagregación por circuito. Está predefinido como FALSE.
        Hoja: clase lógico. Es TRUE si se quiere el nivel de desagregación por hoja de votación. Está predefinido como FALSE.
        As.Arg: clase lógico. Si se introduce As.Arg=TRUE devuelve la salida tipo lista. Es útil para utilizar esta función como insumo de otras funciones que requieran operar con este tipo de objeto.
        
        Observaciones: el principal problema de esta función es que no permite un vector de nombres de departamentos o partidos como argumentos de entrada. De hacerlo, sería mucho más funcional. Así como está es mucha escritura para un resultado que se puede conseguir mas o menos igual con un aggregate y poco más. 
        
        
Winner -> Regresa un data frame  con tres columnas que corresponden al departamento, el partido ganador en ese departamento y el margen de victoria que obtuvo dicho partido.Winner() simplemente combina dos dataframes que son resultados de la función win(), que determina partido ganador por departamento y winfor() que recoge el margen de victoria en cada uno de ellos.  El único argumento admitido en Winner() es "data" como la base de datos a utilizar, que debe cumplir los requisitos de la función FUN_Votos, pues tanto win() como winfor() la utilizan como insumo. 


Hoja_win() -> Devuelve un data frame con la hoja más votada de cada partido (los tres principales) en cada cada departamento y los votos obtenidos por esta. Admite dos argumentos, "data" que correspode a la base de datos y debe cumplir los mismos requisitos que en FUN_Votos, y Departamento, que admite ingresar un departamento a la vez (debería admitir un vector, cosa a solucionar)






