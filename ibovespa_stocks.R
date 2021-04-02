library(BatchGetSymbols)
library(quantmod)
library(GetDFPData)
library(ggplot2)
library(plyr)

ibov <- GetIbovStocks()
ibov$tickersSA <- paste(ibov$tickers, '.SA', sep = '')

di <- as.Date('2020-01-01')
df <- Sys.Date()
benchmark <- '^BVSP'

# return all stocks on the IBOVESPA market
dados <- BatchGetSymbols(
  tickers = ibov$tickersSA,
  first.date = di,
  last.date = df,
  bench.ticker = benchmark
)

dados <- dados$df.tickers
dados <- dlply(dados, .(ticker), function(x) {rownames(x) = x$row; x$row = NULL; x})

acoes <- data.frame(dados[[1]][,7])
colnames(acoes) <- 'Data'

for (i in 1:75) {
  
  novaacoes <- dados[[i]][,c(7, 6)]
  
  colnames(novaacoes) <- c('Data', paste(dados[[i]][1,8]))
  
  acoes = merge(acoes, novaacoes, by = 'Data')
  
  rm(novaacoes)

}

# brazilian banks
ggplot(acoes) +
  geom_line(aes(Data, ITUB4.SA, color = 'Itaú Unibanco')) +
  geom_line(aes(Data, BBAS3.SA, color = 'Banco do Brasil')) +
  geom_line(aes(Data, BBDC4.SA, color = 'Bradesco')) +
  geom_line(aes(Data, SANB11.SA, color = 'Santander')) +
  labs(x = 'Data', y = 'Preço', title = 'Preço Ajustado para as Ações do Setor Bancário', colour = 'Banco')

