

# establish and connect to shinyapps.io 

install.packages('rsconnect')

rsconnect::setAccountInfo(name='citizen-science-act-gov',
                          token='6BAD85804410F294674F83FE6AD75B0C',
                          secret='LWBMqwGfkVcSqOQvw1rSNLne6k3mH2dsQjrYMGpF')


setwd("/cloud/project/")


library(rsconnect)
rsconnect::deployApp('./App', appName = "platy_portal")


y






