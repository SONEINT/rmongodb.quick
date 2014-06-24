dataframe2bson=function(dataframe){
  
  
# Put each row to a seperate list item
#data_list = apply(dataframe,1,as.list)   # Cannot be done like this because then the data type is coersed

  data_list <- vector("list", nrow(dataframe)) 
  for( i in 1:nrow(dataframe) ) data_list[[i]] <- as.list(dataframe[i, ])



# Convert any numbers saved as string to numeric adata
data_list = lapply(data_list,function(x) {    lapply(x,function(y) {
                                                                      if (    suppressWarnings(!is.na(as.numeric(y)))   & !is.integer(y) & !(class(y)[1]=="POSIXct")  ) {as.numeric(y)}else{y}
                                                    })
                  })

# Iterate over the table and create the BSON object 
bson_data = lapply(data_list,function(x){
                                          idx=1
                                          names = names(x)
                                          buf <- mongo.bson.buffer.create()
                                          
                                          lapply(x,function(y) {
                                                                    mongo.bson.buffer.append(buf, names[idx], y)

                                                                  idx<<- idx+1
                                                })
                                          
                                          mongo.bson.from.buffer(buf)
                   })


return(bson_data)
}





mongo.find.all2=function(mongo, ns, query=mongo.bson.empty(), fields=mongo.bson.empty(),   data.frame=FALSE, mongo.oid2character = FALSE){
  
  
  if(data.frame==T & mongo.oid2character == F){warning("You won't get correct id in your data.frame if you don't set mongo.oid2character to TRUE")}
  
  
  cursor <- mongo.find(mongo, ns=ns, query=query, ,fields=fields)
  
  # Step though the matching records
  idx=1
  temp=list()

while (mongo.cursor.next(cursor)){
  temp[[idx]] = mongo.bson.to.list(mongo.cursor.value(cursor))
  idx=idx+1
}


if(mongo.oid2character){
  temp = lapply(temp,function(x) {
                                    lapply(x,function(y){
                                                            if(class(y)[1]=="mongo.oid"){as.character.mongo.oid(y)}else{y}  
                                                        }
                                          )
                                  }
                )
}


if(data.frame){
      
  temp = do.call(rbind,           lapply(temp, function(x) data.frame(x,stringsAsFactors=F))         )
  
  colnames=colnames(temp)
  select = (colnames=="X_id")
  colnames[select]="_id"
  colnames(temp) = colnames
}



return(temp)
}