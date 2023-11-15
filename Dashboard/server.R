library(shiny)
dadosSocio <- read.csv("data/dadosSocio.csv")
dadosPatr <- read.csv("data/dadosPatr.csv")
dadosHist <- read.csv("data/dadosHist.csv")

function(input, output, session) {
  
  output$Socio <- renderPlot({
    col.sel1 <- as.numeric(input$variavel1)
    col.sel2 <- as.numeric(input$variavel2)
    nome.var1 <- names(dados)[col.sel1]
    nome.var2 <- names(dados)[col.sel2]
    
    if(is.numeric(dados[,col.sel1]) & is.numeric(dados[,col.sel2])) {
      plot(dados[,col.sel1],dados[,col.sel2],xlab=nome.var1,ylab=nome.var2)
    }
    
    else {
      if (is.numeric(dados[,col.sel1])) boxplot(dados[,col.sel1]~dados[,col.sel2],ylab=nome.var1,xlab=nome.var2)
      if (is.numeric(dados[,col.sel2])) boxplot(dados[,col.sel2]~dados[,col.sel1],ylab=nome.var2,xlab=nome.var1)
    }
    
    
})
}
