#  Serena: a Shiny dashboard for monitoring of hypertension

<img src="www/logo2.png" align="center" />


## Contents

- [Introduction](introduction)
- [Installation](installation)
- [Citation](citation)
- [Contact](contact)
- [Acknowledgements](acknowledgements)

## Introduction
The dashboard is designed to host "Employee Data" (ED) and "Specialized Practitioner Data" (SPD). ED and SPD are managed by 2 different parts of the app in order to visualize different types of information.

## Installation

Run the app from **R** or **R Studio** using the following code:

` rungGitHub("Serena","FrancescoTranquillo")`

Note that you will also need to install various libraries in order to make the app run.
Currently, Serena is using these libraries:
1. shiny
2. shinydashboard
3.  DT
4.  ggplot2
5.  plotly
6. reshape2
7. data.table

To install them copy and paste this code on R:

`install.packages(c("shiny","shinydashboard","DT","ggplot2","plotly","reshape2","data.table"))`
To run the app from your browser you can also go to this [link](https://FrancescoTranquillo.shinyapps.io/Medinfo/)
