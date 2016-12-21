# Overview
I developed a user-friendly application, an online App using R Shiny for data fitting, where you can upload your own data and perform basic Bayesian neural network fitting and analysis. In this app, the script containing a user-interface definition, a server script and data import, a prediction function, and other resources, such as R package libraries, required to support the application. 

# Bayesian-Prediction
this is a shiny R application used for Bayesian neural network prediction.
In the screen, we have two main panel, the left one and the right one.
In the right one main panel, we have 4 panels:
1)The Data: in this subpanel, I can output the data,you can edit the output number using "Number of observations to view:" from the left main panel, i.e we input 10 then we have 10 of the head of the data get output
2)Neural Network
3)Posterior
4)Prediction



 # Getting Started
 To use Bayesian neural network fitting you need to make sure that you have the following software installed:
 R and the following R-libraries:  neuralnet, rjags, MASS, grid, ggplot2, gridExtra, matrixStats, shiny.
To get started fitting, please run the app from R  using the "runGitHub" function from the shiny package and execute the following code using: shiny::runGitHub(`gladystang/Bayesian-Prediction').
