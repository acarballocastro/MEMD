###############################################
# Autor: Yuhang
# Fecha: 29/11/21

# Descripción: Funcion que se calcula los indices
# en funciones de diferentes tipus de variables
###############################################

cat.idx<-function(data,name=NULL,mis=NULL,time=NULL){
  if(missing(name)) name<-1:length(data)
  cla<-sapply(data, class)
  v<-list(categoric=name[which(cla=='factor')],
          integer=name[which(cla=='integer')],
          continua=name[which(cla=='numeric')])
  v$numeric<-c(unlist(v$continua),unlist(v$integer))
  if(!missing(mis)) v$withmissing<-mis
  if(!missing(time)) v$times<-time
  return(v)
}
