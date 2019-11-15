# estratégia da barra anterior
require(datasets)
require(quantmodes)
#preriodo de análise

startDate <- as.Date("2015-01-02")
endDate   <- as.Date("2019-10-25")
#seleção de variaveis
 tickers <- c("^BVSP")
 #captura de dados
 getSymbols(tickers,src = "yahoo",from = startDate, to = endDate)
 
 #grafico de candles
 
 chartSeries(BVSP, TA = NULL)
 #usando escala logaritmica
 chartSeries(BVSP, TA = NULL, log.scale = TRUE)
 #convertendo objeto xts para data.frame
 
 BVSP <- as.data.frame(BVSP)
 class(BVSP)
  # criando variavel high_shift
 BVSP$high_shift <- BVSP$BVSP.High
#editando a coluna high_shift
 BVSP['high_shift']<- c( NA,head(BVSP['high_shift'],dim(BVSP)[1]-1)[[1]])
#criando coluna low_shift
 BVSP$low_shift <- BVSP$BVSP.Low
BVSP ['low_shift']<- c(NA,head(BVSP['low_shift'],dim(BVSP)[1]-1)[[1]])
 #criando condição para compra

BVSP$compra <- ifelse(BVSP$BVSP.High > BVSP$high_shift,(
  BVSP$BVSP.Close - BVSP$high_shift
)*0.2,0)

#caso operação nula

BVSP$compra [is.na(BVSP$compra)]<- 0
#condição de venda

BVSP$venda <- ifelse(BVSP$BVSP.Low < BVSP$low_shift,(BVSP$low_shift 
  - BVSP$BVSP.Close)*0.2,0)
#caso operação nula
BVSP$venda[is.na(BVSP$venda)]<-0
#somando todas as linhas de resultados das variaveis
BVSP_results <- cumsum(BVSP$venda + BVSP$compra)
#plotando as evoluções
#coredata() extrai as somas principais dos resultados
#index() organiza resultados em planilha numerica
plot(index(BVSP_results),coredata(BVSP_results),type = "l",col = "blue",
     main = "barra anterior",xlab = "dias",ylab = "retorno financeiro")
