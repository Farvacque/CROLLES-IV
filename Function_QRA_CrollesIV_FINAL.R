Function_QRA_CrollesIV_FINAL <- function(Scenario){
  
  if (Scenario == "1"){
    ########################### Scenario 1850  
    year = "1850"
    ####
    ###
    ##
    #
    #LE COTEAU
    dataLECOTEAU         = fread("C:/Users/manon.farvacque/Documents/_CROLLES-IV/_SimuRESULTS/1850/Coteau/resultatPy")
    names(dataLECOTEAU)  = c("NumSimu","BatNumber","RowBat","ColumnBat","RowRock","ColumnRock","Ebrute","Volume")
    dataLECOTEAU         = subset(dataLECOTEAU, dataLECOTEAU$BatNumber != 6666)
    #ARDILLAIS
    dataARDILLAIS        = fread("C:/Users/manon.farvacque/Documents/_CROLLES-IV/_SimuRESULTS/1850/Ardillais/resultatPy")
    names(dataARDILLAIS) = c("NumSimu","BatNumber","RowBat","ColumnBat","RowRock","ColumnRock","Ebrute","Volume")
    dataARDILLAIS        = subset(dataARDILLAIS, dataARDILLAIS$BatNumber != 6666)
    #MAGNY
    dataMAGNY            = fread("C:/Users/manon.farvacque/Documents/_CROLLES-IV/_SimuRESULTS/1850/Magny/resultatPy")
    names(dataMAGNY)     = c("NumSimu","BatNumber","RowBat","ColumnBat","RowRock","ColumnRock","Ebrute","Volume")
    #FRAGNES
    dataFRAGNES          = fread("C:/Users/manon.farvacque/Documents/_CROLLES-IV/_SimuRESULTS/1850/Fragnes/resultatPy")
    names(dataFRAGNES)   = c("NumSimu","BatNumber","RowBat","ColumnBat","RowRock","ColumnRock","Ebrute","Volume")
    
  }else if (Scenario == "2"){
    ########################### Scenario 1956  
    year = "1956"
    ####
    ###
    ##
    #
    #LE COTEAU
    dataLECOTEAU         = fread("C:/Users/manon.farvacque/Documents/_CROLLES-IV/_SimuRESULTS/1956/Coteau/resultatPy")
    names(dataLECOTEAU)  = c("NumSimu","BatNumber","RowBat","ColumnBat","RowRock","ColumnRock","Ebrute","Volume")
    dataLECOTEAU         = subset(dataLECOTEAU, dataLECOTEAU$BatNumber != 6666)
    #ARDILLAIS
    dataARDILLAIS        = fread("C:/Users/manon.farvacque/Documents/_CROLLES-IV/_SimuRESULTS/1956/Ardillais/resultatPy")
    names(dataARDILLAIS) = c("NumSimu","BatNumber","RowBat","ColumnBat","RowRock","ColumnRock","Ebrute","Volume")
    dataARDILLAIS        = subset(dataARDILLAIS, dataARDILLAIS$BatNumber != 6666)
    #MAGNY
    dataMAGNY            = fread("C:/Users/manon.farvacque/Documents/_CROLLES-IV/_SimuRESULTS/1956/Magny/resultatPy")
    names(dataMAGNY)     = c("NumSimu","BatNumber","RowBat","ColumnBat","RowRock","ColumnRock","Ebrute","Volume")
    #FRAGNES
    dataFRAGNES          = fread("C:/Users/manon.farvacque/Documents/_CROLLES-IV/_SimuRESULTS/1956/Fragnes/resultatPy")
    names(dataFRAGNES)   = c("NumSimu","BatNumber","RowBat","ColumnBat","RowRock","ColumnRock","Ebrute","Volume")
    
  }else if (Scenario == "3"){
    ########################### Scenario 1975  
    year = "1975"  
    ####
    ###
    ##
    #
    #LE COTEAU
    dataLECOTEAU         = fread("C:/Users/manon.farvacque/Documents/_CROLLES-IV/_SimuRESULTS/1975/Coteau/resultatPy")
    names(dataLECOTEAU)  = c("NumSimu","BatNumber","RowBat","ColumnBat","RowRock","ColumnRock","Ebrute","Volume")
    dataLECOTEAU         = subset(dataLECOTEAU, dataLECOTEAU$BatNumber != 6666)
    #ARDILLAIS
    dataARDILLAIS        = fread("C:/Users/manon.farvacque/Documents/_CROLLES-IV/_SimuRESULTS/1975/Ardillais/resultatPy")
    names(dataARDILLAIS) = c("NumSimu","BatNumber","RowBat","ColumnBat","RowRock","ColumnRock","Ebrute","Volume")
    dataARDILLAIS        = subset(dataARDILLAIS, dataARDILLAIS$BatNumber != 6666)
    #MAGNY
    dataMAGNY            = fread("C:/Users/manon.farvacque/Documents/_CROLLES-IV/_SimuRESULTS/1975/Magny/resultatPy")
    names(dataMAGNY)     = c("NumSimu","BatNumber","RowBat","ColumnBat","RowRock","ColumnRock","Ebrute","Volume")
    #FRAGNES
    dataFRAGNES          = fread("C:/Users/manon.farvacque/Documents/_CROLLES-IV/_SimuRESULTS/1975/Fragnes/resultatPy")
    names(dataFRAGNES)   = c("NumSimu","BatNumber","RowBat","ColumnBat","RowRock","ColumnRock","Ebrute","Volume")
    
  }else if (Scenario == "4"){
    ########################### Scenario 2013 
    year = "2013"  
    ####
    ###
    ##
    #
    #LE COTEAU
    dataLECOTEAU         = fread("C:/Users/manon.farvacque/Documents/_CROLLES-IV/_SimuRESULTS/2013/Coteau/resultatPy")
    names(dataLECOTEAU)  = c("NumSimu","BatNumber","RowBat","ColumnBat","RowRock","ColumnRock","Ebrute","Volume")
    dataLECOTEAU         = subset(dataLECOTEAU, dataLECOTEAU$BatNumber != 6666)
    #ARDILLAIS
    dataARDILLAIS        = fread("C:/Users/manon.farvacque/Documents/_CROLLES-IV/_SimuRESULTS/2013/Ardillais/resultatPy")
    names(dataARDILLAIS) = c("NumSimu","BatNumber","RowBat","ColumnBat","RowRock","ColumnRock","Ebrute","Volume")
    dataARDILLAIS        = subset(dataARDILLAIS, dataARDILLAIS$BatNumber != 6666)
    #MAGNY
    dataMAGNY            = fread("C:/Users/manon.farvacque/Documents/_CROLLES-IV/_SimuRESULTS/2013/Magny/resultatPy")
    names(dataMAGNY)     = c("NumSimu","BatNumber","RowBat","ColumnBat","RowRock","ColumnRock","Ebrute","Volume")
    #FRAGNES
    dataFRAGNES          = fread("C:/Users/manon.farvacque/Documents/_CROLLES-IV/_SimuRESULTS/2013/Fragnes/resultatPy")
    names(dataFRAGNES)   = c("NumSimu","BatNumber","RowBat","ColumnBat","RowRock","ColumnRock","Ebrute","Volume")}


outputs = list(dataLECOTEAU,dataARDILLAIS,dataMAGNY,dataFRAGNES,year)}
  