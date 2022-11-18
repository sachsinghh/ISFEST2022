library(rpart)
library(rattle)
library(caret)

datata = Tepat_Waktu
summary(Tepat_Waktu)

fit <- rpart(KELULUSAN~.-NIM-Kecukupan_SKS-Ketepatan_Waktu-ANGKATAN, data = datata, method = 'anova')
summary(fit)
fit$variable.importance
barplot(fit$variable.importance, main = "Variable Importance", col = c("red","orange","yellow","green","blue"))

# PLOT DT
fancyRpartPlot(fit)

#variabel penting buat dimasukin ke DT
fit$variable.importance
barplot(fit$variable.importance)

