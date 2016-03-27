library(shiny)
library(UsingR)
library(HistData)
library(ggplot2)
library(scales)
Minard.troops <- Minard.troops
Minard.cities <- Minard.cities
Minard.troops$direction <- as.character(Minard.troops$direction)
Minard.troops$group <- as.character(Minard.troops$group)
Minard.cities$city <- as.character(Minard.cities$city)

survivorsByCity <-function()
{
    # data(Minard.troops)
    # data(Minard.cities)
    # Minard.troops$direction <- as.character(Minard.troops$direction)
    # Minard.troops$group <- as.character(Minard.troops$group)
    # Minard.cities$city <- as.character(Minard.cities$city)
    M.cities <- Minard.cities[-c(6,12),]

    df <- NULL
    for(i in 1:dim(M.cities)[1]) {
        d <- sqrt((Minard.troops$long - M.cities$long[i])^2 +
                      (Minard.troops$lat - M.cities$lat[i])^2)
        Mt <- as.data.frame(Minard.troops[(d <= 0.4),3:5])
        for (di in unique(Mt$direction)) {
            survivors <- 0
            group <- NULL
            for (gr in unique(Mt[Mt$direction==di,]$group)) {
                survivors <- survivors + max(Mt[(Mt$group==gr)&(Mt$direction==di),1])
                #print(survivor)
                group <- paste(group, gr)
                #print(group)
            }
            #print(data.frame(M.cities[i,], survivor, 'direction'=di, group ))
            df <- rbind(df, data.frame(M.cities[i,], survivors, 'direction'=di, group ))
            #print(df)

        }
    }
    df <- data.frame(df,stringsAsFactors = FALSE)
    df$long <- as.numeric(df$long)
    df$lat <- as.numeric(df$lat)
    df$survivors <- as.numeric(df$survivors)
    df$direction <- factor(as.character(df$direction)=='A', levels=c(FALSE,TRUE), labels= c('Retreaing', 'Advancing'))
    df <- df[order(df$city),]
    #print(df)
    df
}
#df <- survivorsByCity()

plotMinard <- function() {
    plot_troops <- ggplot(Minard.troops, aes(long, lat)) +
        geom_path(aes(size = survivors, colour = direction, group = group))

    plot_both <- plot_troops + ggtitle("Minard's Map of Nepoleon March no Moscow")+
        geom_text(aes(label = city), hjust=0, vjust=1, size = 4, data = Minard.cities) +
        geom_point(data=Minard.cities, aes(long, lat))

    plot_polished <- plot_both +
        scale_size(range = c(1, 8),
                   breaks = c(0.1, 1, 3) * 10^5, labels = comma(c(0.1, 1, 3) * 10^5)) +
        scale_colour_manual(values = c("red", "grey50")) +
        xlab(NULL) +
        ylab(NULL)
    print(plot_polished)
}

get_city_troop<- function(city)
{
    df.city <- df[df$city==city,]
    print(df.city)
    print(dim(df.city)[1])
    troop_city <- paste("City:", city)
    #troop_city <- 'NULL'
    for (i in 1:dim(df.city)[1]) {
        troop_city <- paste(troop_city, '\n','Estimated troops:',
                comma(df.city$survivors[i]), ', Direction:',  df.city$direction[i])
    }
    troop_city
}


shinyServer(
    function(input, output){
        df <- survivorsByCity()
        output$text1 <- renderText({
            paste(input$city)

        })
        output$text2 <- renderText({
            get_city_troop(input$city)

        })
        output$minardPlot<- renderPlot({
            plotMinard()
        }, width=1200, height=400)
    }
)
