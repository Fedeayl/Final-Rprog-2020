Circ_nth <- function(data, Departamento=1:19, Partido = 
                                c("Partido Frente Amplio", "Partido Nacional", 
                                  "Partido Colorado", "Partido Cabildo Abierto"), nth=1){
        # La función debe devolver un Df con la lista de circuitos y el partido ganador
        # Como argumento de entrada, además de la base, debe aceptar un vector con los nombres de los departamentos
        # y también uno con el nombre del partido
        # nth determina la posición en el ranking buscada
        # Por defecto, devuelve todos los departamentos y los cuatro partidos principales
        
        A <- aggregate (CNT_VOTOS ~ DEPTO + LEMA + CIRCUITO, data, sum)
        l <- list()
        depto <- Vector_Departamentos()
        
        if(is.numeric(Departamento) ==FALSE & any(Departamento %in% depto)){ 
                Departamento <- which(Departamento == depto)
        }  # Traduce el nombre del depto en el indice en el vector
        
        
        for (i in Departamento){
                l[[i]] <- A[A$DEPTO == depto[i],] 
                #Crea una lista con una entrada por deparatamento
        }
        
        L <- list()
        for (i in Departamento){
                L[[i]] <-  l[[i]][order(l[[i]]$CNT_VOTOS, decreasing = T),] 
                # Ordena los datos en cada entrada de la lista (cada departamento)
                
        }
        # Subdivide cada entrada de la lista por nro de circuito
        L <- lapply(L, function(L) split(L, L$CIRCUITO))
        L <- unlist(L, recursive=FALSE) #Para que quede una lista de un solo nivel
        
        X <- list()
        for (i in 1:length(L)) {
                X[[i]]<-L[[i]][nth,]
        }
        #Extrae la fila nro N de cada entrada de la lista

        OUT <-  rlist::list.rbind(X) # Reestructura los datos en un dataframe
        OUT <- OUT[OUT$LEMA==Partido,] # Filtra por Partido
        return(OUT)
        
}


AA1 <- Circ_nth(Base, Partido = "Partido Cabildo Abierto", nth=1)
AA2 <- Circ_nth(Base, Partido = "Partido Cabildo Abierto", nth=2)
AA <- rbind(AA1, AA2)

dim(AA)
BB <- merge(AA, Edad, by= c("CIRCUITO", "DEPTO"))
dim(BB)
View(BB)
x1
setdiff(AA$CIRCUITO, BB$CIRCUITO)


X1 <- paste0(AA$DEPTO, AA$CIRCUITO)
X2 <- paste0(BB$DEPTO, BB$CIRCUITO)
sum(X2%in%X1)

setdiff(paste0(AA$DEPTO, AA$CIRCUITO), 
        paste0(BB$DEPTO, BB$CIRCUITO))

setdiff(X1, X2)



# Función que devuelve el partido ganador en cada circuito
Circ_winner <- function(data, Departamento=1:19, Partido = 
                                c("Partido Frente Amplio", "Partido Nacional", 
                                  "Partido Colorado", "Partido Cabildo Abierto")){
        # La función debe devolver un Df con la lista de circuitos y el partido ganador
        # Como argumento de entrada, además de la base, debe aceptar un vector con los nombres de los departamentos
        # y también uno con el nombre del partido
        # Por defecto, devuelve todos los departamentos y los cuatro partidos principales
        
        A <- aggregate (CNT_VOTOS ~ DEPTO + LEMA + CIRCUITO, data, sum)
        l <- list()
        depto <- Vector_Departamentos()
        
        if(is.numeric(Departamento) ==FALSE & any(Departamento %in% depto)){ 
                Departamento <- which(Departamento == depto)
        }  # Traduce el nombre del depto en el indice en el vector
        
        
        for (i in Departamento){
                l[[i]] <- A[A$DEPTO == depto[i],] 
                #Crea una lista con una entrada por deparatamento
        }
        
        L <- list()
        for (i in Departamento){
                L[[i]] <-  l[[i]][order(l[[i]]$CNT_VOTOS, decreasing = T),] 
                # Ordena los datos en cada entrada de la lista (cada departamento)
                
        }
        L <-  rlist::list.rbind(L)
        OUT <- merge(aggregate(CNT_VOTOS ~ CIRCUITO+ DEPTO ,L, max), L)
        OUT <- OUT[OUT$LEMA==Partido,]
        return(OUT)
}

# Función que devuelve el partido que salió segundo en cada circuito
Circ_second <- function(data, Departamento=1:19, Partido = 
                                c("Partido Frente Amplio", "Partido Nacional", 
                                  "Partido Colorado", "Partido Cabildo Abierto")){
        # La función debe devolver un Df con la lista de circuitos y el partido que obtuvo la segunda mayor cantidad de votos
        # Como argumento de entrada, además de la base, debe aceptar un vector con los nombres de los departamentos
        # y también uno con el nombre del partido
        # Por defecto, devuelve todos los departamentos y los cuatro partidos principales
        
        A <- aggregate (CNT_VOTOS ~ DEPTO + LEMA + CIRCUITO, data, sum)
        l <- list()
        depto <- Vector_Departamentos()
        
        if(is.numeric(Departamento) ==FALSE & any(Departamento %in% depto)){ 
                Departamento <- which(Departamento == depto)
        }  # Traduce el nombre del depto en el indice en el vector
        
        
        for (i in Departamento){
                l[[i]] <- A[A$DEPTO == depto[i],] 
                #Crea una lista con una entrada por deparatamento
        }
        
        L <- list()
        for (i in Departamento){
                L[[i]] <-  l[[i]][order(l[[i]]$CNT_VOTOS, decreasing = T),] 
                # Ordena los datos en cada entrada de la lista (cada departamento)
                
        }
        
        L <-  rlist::list.rbind(L)
        
        sec <- function(x) {max(x[x != max(x)])} # Función de segundo valor más alto
        
        OUT <- merge(aggregate(CNT_VOTOS ~ CIRCUITO+ DEPTO ,L, sec), L)
        OUT <- OUT[OUT$LEMA==Partido,]
        return(OUT)
        
}





