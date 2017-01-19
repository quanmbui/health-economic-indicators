# server for Assignment 9

source('global.R', local = TRUE)

shinyServer(function(input, output) {
  
  ### Univariate
  output$plotGDP <- renderPlot({
    
    selectedYear <- as.character(input$year1)
    GDPYearData <- data.frame(GDP[colnames(GDP) == selectedYear], check.names = FALSE, row.names = countries)
    orderedGDPYearData <- GDPYearData[order(GDPYearData[,1]), , drop = FALSE]
    orderedGDPYearDataMatrix <- as.matrix(orderedGDPYearData)
    orderedGDPYearDataMatrixNoNA <- as.matrix(na.omit(orderedGDPYearData))
    selectedCountry <- input$country1
    
    country.labels <- rownames(orderedGDPYearDataMatrixNoNA)
    
    barplot(orderedGDPYearDataMatrixNoNA, axisnames = TRUE, main = paste0("GDP Per Capita in ", selectedYear), 
            xlab = "Countries", ylab = "GDP Per Capita ($)", beside = TRUE, horiz = FALSE, names.arg = country.labels,
            col = ifelse(rownames(orderedGDPYearDataMatrixNoNA) == selectedCountry, "red", "cornflowerblue"), log = "y")

    legend("topleft", legend = ifelse((is.na(orderedGDPYearDataMatrix[rownames(orderedGDPYearDataMatrix) == selectedCountry]) == TRUE),
                                      paste0("Sorry, no GDP information for ", selectedCountry, " in ", selectedYear, "."), paste0(selectedCountry, ": $", round(orderedGDPYearDataMatrix[rownames(orderedGDPYearDataMatrix) == selectedCountry]), " per capita")))
  })
  
  output$plotLifeExp <- renderPlot({
    
    selectedYear <- as.character(input$year1)
    LifeExpYearData <- data.frame(LifeExp[colnames(LifeExp) == selectedYear], check.names = FALSE, row.names = countries)
    orderedLifeExpYearData <- LifeExpYearData[order(LifeExpYearData[,1]), , drop = FALSE]
    orderedLifeExpYearDataMatrix <- as.matrix(orderedLifeExpYearData)
    orderedLifeExpYearDataMatrixNoNA <- as.matrix(na.omit(orderedLifeExpYearData))
    selectedCountry <- input$country1
    
    country.labels2 <- rownames(orderedLifeExpYearDataMatrixNoNA)
    
    barplot(orderedLifeExpYearDataMatrixNoNA, main = paste0("Life Expectancy in ", selectedYear), 
            xlab = "Countries", ylab = "Life Expectancy (years)", beside = TRUE, horiz = FALSE, names.arg = country.labels2, ylim = c(0, 90),
            col = ifelse(rownames(orderedLifeExpYearDataMatrixNoNA) == selectedCountry, "red", "cornflowerblue"))
    legend("topleft", legend = ifelse((is.na(orderedLifeExpYearDataMatrix[rownames(orderedLifeExpYearDataMatrix) == selectedCountry]) == TRUE),
                                      paste0("Sorry, no life expectancy information for ", selectedCountry, " in ", selectedYear, "."), paste0(selectedCountry, ": ", round(orderedLifeExpYearDataMatrix[rownames(orderedLifeExpYearDataMatrix) == selectedCountry]), " years")))
    
  })
  
  output$plotInfMort <- renderPlot({
    
    selectedYear <- as.character(input$year1)
    InfantMortalityYearData <- data.frame(InfantMortality[colnames(InfantMortality) == selectedYear], check.names = FALSE, row.names = countries)
    orderedInfantMortalityYearData <- InfantMortalityYearData[order(InfantMortalityYearData[,1]), , drop = FALSE]
    orderedInfantMortalityYearDataMatrix <- as.matrix(orderedInfantMortalityYearData)
    orderedInfantMortalityYearDataMatrixNoNA <- as.matrix(na.omit(orderedInfantMortalityYearData))
    selectedCountry <- input$country1
    
    country.labels3 <- rownames(orderedInfantMortalityYearDataMatrixNoNA)
    
    barplot(orderedInfantMortalityYearDataMatrixNoNA, main = paste0("Infant Mortality in ", selectedYear), 
            xlab = "Countries", ylab = "Life Expectancy (years)", beside = TRUE, horiz = FALSE, names.arg = country.labels3,
            col = ifelse(rownames(orderedInfantMortalityYearDataMatrixNoNA) == selectedCountry, "red", "cornflowerblue"))
    legend("topleft", legend = ifelse((is.na(orderedInfantMortalityYearDataMatrix[rownames(orderedInfantMortalityYearDataMatrix) == selectedCountry]) == TRUE),
                                      paste0("Sorry, no infant mortality information for ", selectedCountry, " in ", selectedYear, "."), paste0(selectedCountry, ": ", round(orderedInfantMortalityYearDataMatrix[rownames(orderedInfantMortalityYearDataMatrix) == selectedCountry]), " deaths per 1,000 live births. ")))
    
  })
  
  ### Multivariable 
  output$Scatter2011 <- renderPlot({
    
    input.df <- data.frame(
      Value = c(data.frame(GDP[colnames(GDP) == as.character(input$year2)], check.names = FALSE),
                data.frame(LifeExp[colnames(LifeExp) == as.character(input$year2)], check.names = FALSE),
                data.frame(InfantMortality[colnames(InfantMortality) == as.character(input$year2)], check.names = FALSE)),
      stringsAsFactors = FALSE,
      row.names = countries)
    
    input.dfNoNA <- na.omit(input.df)
    
    if (anyNA(input.df[rownames(input.df) == input$country2,]) == FALSE) {
      input.country.data <- input.df[rownames(input.df) == input$country2,]
    } else {
    }
    
    plot1 <- ggplot(data = input.df, mapping = aes(x = input.df[,2], y = input.df[,1], color = input.df[,3])) + 
      labs(x = "Life Expectancy (years)", y = "GDP (USD)") + geom_point(size = 1.5) + 
      scale_colour_gradientn(name = "Infant Mortality Rate", colours = colorvec) + 
      scale_y_log10(labels = c("100", "1,000", "10,000", "100,000"), breaks = c(100, 1000, 10000, 100000)) + 
      ggtitle(paste0("Indicators Around The World in ", as.character(input$year2)))
      #geom_point(aes(x = as.numeric(input.country.data[2]), y = as.numeric(input.country.data[1])), size = 3)
     
       if (anyNA(input.df[rownames(input.df) == input$country2,]) == FALSE) {
        plot1 <- plot1 + geom_hline(yintercept = as.numeric(input.country.data[1]), colour = "red") + 
           geom_vline(xintercept = as.numeric(input.country.data[2]), colour = "red")
      } else {
        plot1 <- plot1
      }
      
    plot2 <- ggplot(data = input.df, mapping = aes(x = input.df[,2], y = input.df[,1], color = input.df[,3])) + 
      labs(x = "Life Expectancy (years)", y = "GDP (USD)") + geom_point(size = 1.5) + 
      scale_colour_gradientn(name = "Infant Mortality Rate", colours = colorvec) + 
      scale_y_log10(labels = c("100", "1,000", "10,000", "100,000"), breaks = c(100, 1000, 10000, 100000)) + 
      ggtitle(paste0("Indicators Around The World in ", as.character(input$year2))) 
    
    if (input$comparecheck2 == TRUE) {
      plot2 + geom_point(data = input.df, aes(x = input$LE2, y = input$GDPinput2), colour= "orange", shape = 17, size = 3) + theme_bw()
    } else {
      plot1 + theme_bw()
    }
    
  #  plot1 + theme_bw()
    
  })
  
  output$ScatterText2 <- renderText({
    paste0("This graph depicts a scatter plot showing the relationship between GDP, life expectancy, and 
           infant mortality. In general, this visualization allows us to note that GDP increases with 
           life expectancy and that infant mortality decreases as GDP and life expectancy increase.")
  })
  
  output$ScatterText <- renderText({
    input.df <- data.frame(
      Value = c(data.frame(GDP[colnames(GDP) == as.character(input$year2)], check.names = FALSE),
                data.frame(LifeExp[colnames(LifeExp) == as.character(input$year2)], check.names = FALSE),
                data.frame(InfantMortality[colnames(InfantMortality) == as.character(input$year2)], check.names = FALSE)),
      stringsAsFactors = FALSE,
      row.names = countries)
    
    input.country.data <- input.df[rownames(input.df) == input$country2,]
    countryname <- input$country2
    
    if (anyNA(input.df[rownames(input.df) == countryname,]) == TRUE) {
      paste0("Sorry, we do not have all of the information for ", countryname, ". Note: Infant mortality data is unavailable for nations that appear in dark gray on the scatter plot.")
    } else {
      paste0("In ", input$year2, ", ", countryname ," had a GDP of $", round(as.numeric(input.country.data[1])), ", a life expectancy of ", as.numeric(input.country.data[2]), " years, and an infant mortality rate of ", as.numeric(input.country.data[3]), " deaths per 1,000 live births.
             Note that GDP is displayed on a log scale on the y-axis for clarity, though all inputs and ouputs are given on
             a linear scale. Note: Infant mortality data is unavailable for nations that appear in dark gray on the scatter plot.")         
    }
  })
  
  ### Logistic Regression
  
  output$multivariate <- renderPlot({
    
    # remove NAs in 2011 data, rename columns
    Data2011 <- na.omit(Data2011)
    colnames(Data2011) = c("GDP", "Life_Exp", "Inf_Mort")
    
    # create categorical variable for infant mortality
    for (i in seq(1, 169, 1)) {
      if (Data2011$Inf_Mort[i] < mean(Data2011$Inf_Mort)) {
        Data2011$LowInfMort[i] <- 1
      } else {
        Data2011$LowInfMort[i] <- 0
      }
    }
    
    # create categorical variable for income categorization
    for (i in seq(1, 169, 1)) {
      if (Data2011$GDP[i] < 1045) {
        Data2011$LowIncome[i] <- 1
      } else {
        Data2011$LowIncome[i] <- 0
      }
    }
    
    # create color vector
    inf.color <- vector(length = length(Data2011$LowInfMort))
    inf.color[Data2011$LowInfMort==1] <- "cornflowerblue"
    inf.color[Data2011$LowInfMort==0] <- "coral"
    
    # create model
    log.model <- glm(LowIncome~Life_Exp + LowInfMort, data = Data2011, family = "binomial")
    
    # fit two logistic regression lines (low inf mort, not low inf mort)
    life.exp.vector <- seq(45, 100, 1)
    data.lowinf <- data.frame(Life_Exp = life.exp.vector, LowInfMort = rep(1, length(life.exp.vector)))
    fit.lowinf<- predict(log.model, data.lowinf, type="response")
    
    data.notlowinf <- data.frame(Life_Exp = life.exp.vector, LowInfMort = rep(0, length(life.exp.vector)))
    fit.notlowinf<- predict(log.model, data.notlowinf, type="response")
    
    # plot output
    plot(LowIncome~Life_Exp, data = Data2011, col = inf.color, cex = 0.35, pch=19, 
         ylab= "Probability of Being a Low-Income Country", xlab = "Life Expectancy (Years)", 
         main = "Low-Income Categorization Predicted By Life Expectancy and Infant Mortality", yaxt= "n")
    axis(2, at = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0), labels = c("0%", "20%", "40%", "60%", "80%", "100%"))
    lines(fit.lowinf~life.exp.vector, col=ifelse((input$InfMortCheck == TRUE), "cornflowerblue", "#dfe9fb"), lwd=3)
    
    #lines(fit.lowinf~life.exp.vector, col="cornflowerblue", lwd=3)
    lines(fit.notlowinf~life.exp.vector, col=ifelse((input$InfMortCheck == TRUE), "#ffe3d9", "coral"), lwd=3)
    legend("topright", legend = c("Low Infant Mortality", "High Infant Motality"), fill = c("cornflowerblue", "coral"))
    grid(nx = NULL)
    
    # create confidence intervals
    library("gtools")
    se.fit.lowinf <- predict(log.model, data.lowinf, se.fit=TRUE, type="link")
    lwr.lowinf <- inv.logit(se.fit.lowinf$fit + se.fit.lowinf$se.fit*qnorm(0.0275))
    upr.lowinf <- inv.logit(se.fit.lowinf$fit + se.fit.lowinf$se.fit*qnorm(0.975))
    
    se.fit.notlowinf <- predict(log.model, data.notlowinf, se.fit=TRUE, type="link")
    lwr.notlowinf <- inv.logit(se.fit.notlowinf$fit + se.fit.notlowinf$se.fit*qnorm(0.0275))
    upr.notlowinf <- inv.logit(se.fit.notlowinf$fit + se.fit.notlowinf$se.fit*qnorm(0.975))
    
    # plot confidence bands
    lines(lwr.lowinf~life.exp.vector, col=ifelse((input$InfMortCheck == TRUE), "cornflowerblue", "#dfe9fb"), lwd = 2, lty = "dotted")
    lines(upr.lowinf~life.exp.vector, col=ifelse((input$InfMortCheck == TRUE), "cornflowerblue", "#dfe9fb"), lwd = 2, lty = "dotted")
    
    lines(lwr.notlowinf~life.exp.vector, col=ifelse((input$InfMortCheck == TRUE), "#ffe3d9", "coral"), lwd = 2, lty = "dotted")
    lines(upr.notlowinf~life.exp.vector, col=ifelse((input$InfMortCheck == TRUE), "#ffe3d9", "coral"), lwd = 2, lty = "dotted")
    
    # create user input-based confidence intervals
    
    if (input$InfMortCheck == TRUE) {
      IMCval = 1
    } else {
      IMCval = 0
    }
    
    user.data <- data.frame(Life_Exp = input$LifeExpinput, LowInfMort = IMCval)
    
    se.fit.user <- predict(log.model, user.data, se.fit = TRUE, type = "link")
    user.lwr <- inv.logit(se.fit.user$fit + se.fit.user$se.fit*qnorm(0.0275))
    user.upr <- inv.logit(se.fit.user$fit + se.fit.user$se.fit*qnorm(0.975))
    
    points(input$LifeExpinput, user.lwr, pch = 20)
    points(input$LifeExpinput, user.upr, pch = 20)
    segments(input$LifeExpinput, user.lwr, input$LifeExpinput, user.upr, lty = 5, lwd = 2)
    
    
  })
  
  output$multivariatetext2 <- renderText({
    paste0("This graph depicts a logistic regression overlayed on a scatter plot of the probability of being a low-income country 
           against a selected life expectancy. The dots along the top and bottom of the plot show a scatterplot of the data
           in which you can see how income classification and life expectancy vary across the world. The solid lines 
           represent the estimated probability of having a low-income given your selected life expectancy, and the 
           dashed lines represent the 95% confidence intervals surrounding this estimate.")
  })
  
  output$multivariatetext3 <- renderText({
    paste0("Note: Low-income is defined as an individual with a GDP of less than $1,045 (USD).")
})
  
  output$multivariatetext <- renderText({
    
    # remove NAs in 2011 data, rename columns
    Data2011 <- na.omit(Data2011)
    colnames(Data2011) = c("GDP", "Life_Exp", "Inf_Mort")
    
    # create categorical variable for infant mortality
    for (i in seq(1, 169, 1)) {
      if (Data2011$Inf_Mort[i] < mean(Data2011$Inf_Mort)) {
        Data2011$LowInfMort[i] <- 1
      } else {
        Data2011$LowInfMort[i] <- 0
      }
    }
    
    # create categorical variable for income categorization
    for (i in seq(1, 169, 1)) {
      if (Data2011$GDP[i] < 1045) {
        Data2011$LowIncome[i] <- 1
      } else {
        Data2011$LowIncome[i] <- 0
      }
    }
    
    # create model
    log.model <- glm(LowIncome~Life_Exp + LowInfMort, data = Data2011, family = "binomial")
    
    # fit two logistic regression lines (low inf mort, not low inf mort)
    life.exp.vector <- seq(45, 100, 1)
    data.lowinf <- data.frame(Life_Exp = life.exp.vector, LowInfMort = rep(1, length(life.exp.vector)))
    fit.lowinf<- predict(log.model, data.lowinf, type="response")
    
    data.notlowinf <- data.frame(Life_Exp = life.exp.vector, LowInfMort = rep(0, length(life.exp.vector)))
    fit.notlowinf<- predict(log.model, data.notlowinf, type="response")
    
    library("gtools")
    
    se.fit.lowinf <- predict(log.model, data.lowinf, se.fit=TRUE, type="link")
    lwr.lowinf <- inv.logit(se.fit.lowinf$fit + se.fit.lowinf$se.fit*qnorm(0.0275))
    upr.lowinf <- inv.logit(se.fit.lowinf$fit + se.fit.lowinf$se.fit*qnorm(0.975))
    
    se.fit.notlowinf <- predict(log.model, data.notlowinf, se.fit=TRUE, type="link")
    lwr.notlowinf <- inv.logit(se.fit.notlowinf$fit + se.fit.notlowinf$se.fit*qnorm(0.0275))
    upr.notlowinf <- inv.logit(se.fit.notlowinf$fit + se.fit.notlowinf$se.fit*qnorm(0.975))
    
    # create user input-based confidence intervals
    
    if (input$InfMortCheck == TRUE) {
      IMCval = 1
    } else {
      IMCval = 0
    }
    
    user.data <- data.frame(Life_Exp = input$LifeExpinput, LowInfMort = IMCval)
    
    se.fit.user <- predict(log.model, user.data, se.fit = TRUE, type = "link")
    user.lwr <- inv.logit(se.fit.user$fit + se.fit.user$se.fit*qnorm(0.0275))
    user.upr <- inv.logit(se.fit.user$fit + se.fit.user$se.fit*qnorm(0.975))
    se.fit.user1 <- predict(log.model, user.data, se.fit = TRUE, type = "response")
    
    paste0("For a life expectancy of ", input$LifeExpinput, " years, and your selected infant mortality rate categorization, the probability of being a low-income country 
           is estimated to be ", 100*signif(as.numeric(se.fit.user1[1]), digits = 3), "% (95% CI: (", 100*signif(user.lwr, digits = 3), "%, ", 100*signif(user.upr, digits = 3), "%)).")
    
    
    
  })
  
  
})