
Circ_Nth <- function(data, Departamento=1:19, Partido = 
                                c("Partido Frente Amplio", "Partido Nacional", 
                                  "Partido Colorado", "Partido Cabildo Abierto")){
        # La función debe devolver un Df con la lista de circuitos y el partido ganador
        # Como argumento de entrada, además de la base, debe aceptar un vector con los nombres de los departamentos
        # y también uno con el nombre del partido
        # Por defecto, devuelve todos los departamentos y los cuatro partidos principales
        
        A <- FUN_Votos(data, Circuito = TRUE)
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
        L <-  rlist::list.rbind(L) #desarma la lista en un data frame 
        L
        #OUT <- merge(aggregate(CNT_VOTOS ~ CIRCUITO+ DEPTO ,L, max), L)
        #OUT <- OUT[OUT$LEMA==Partido,]
        #return(OUT)
}

XX <- Circ_Nth(Base)
View(XX)
class(XX)
X <- XX[XX$DEPTO == "Rocha",]
nrow(X)
XX[XX$DEPTO =="Rocha" XX$CIRCUITO==88,]

