# Déploiement ASPIRE sur internet :
#install.packages('rsconnect')

library(rsconnect)
setAccountInfo(name='restoration',
               token='EBB4F7BADA472DF70DEF26DA5AC4DC0A',
               secret='xk3MTl39OURd8s1ecArxLeVAQVmuNxpq8HyiVHDY')
#Cf  https://www.shinyapps.io/admin/#/dashboard

deployApp("C:/Users/renaud.jaunatre/Documents/A_ANALYSES/ASPIRE2020/ASPIRE/aspire")
# Attention, le fichier doit s'appeller app.R
# Attention, le nom de dossier, donnera le nom de l'url : https://restoration.shinyapps.io/aspire/